# llp_loco.py
import torch
import torch.nn as nn
import torch.optim as optim
import numpy as np
import pandas as pd

# ============================================================
# 1. MODELE PYTORCH
# ============================================================
class LLPNet(nn.Module):
    def __init__(self, n_cont, n_area, n_cohortes, emb_dim, hidden1, hidden2, n_Z, phi_init):
        super(LLPNet, self).__init__()
        self.emb_cohorte = nn.Embedding(n_cohortes, emb_dim)
        self.encoder = nn.Sequential(
            nn.Linear(n_cont + n_area + emb_dim, hidden1),
            nn.ReLU(),
            nn.Linear(hidden1, hidden2),
            nn.ReLU(),
            nn.Linear(hidden2, 1)
        )
        self.decoder = nn.Linear(1 + n_Z, 1)
        self.log_phi = nn.Parameter(torch.tensor(np.log(phi_init), dtype=torch.float32))

    def forward(self, X_cont, area_oh, cohorte_idx, Z, cohort_sizes):
        emb = self.emb_cohorte(cohorte_idx)
        x_in = torch.cat([X_cont, area_oh, emb], dim=1)
        h_ij = self.encoder(x_in)

        lambda_list = []
        start = 0
        for size in cohort_sizes:
            end = start + size
            lambda_list.append(h_ij[start:end, :].mean(dim=0, keepdim=True))
            start = end
        lambda_j = torch.cat(lambda_list, dim=0)

        dec_in = torch.cat([lambda_j, Z], dim=1)
        eta_j = self.decoder(dec_in)
        mu_j = torch.sigmoid(eta_j).squeeze(1)
        return mu_j, torch.exp(self.log_phi)

# ============================================================
# 2. LOSS BETA-BINOMIALE
# ============================================================
def beta_binomial_nll(k, n, mu, phi):
    alpha = mu * phi
    beta_ = (1 - mu) * phi
    nll = -(
        torch.lgamma(n + 1) - torch.lgamma(k + 1) - torch.lgamma(n - k + 1) +
        torch.lgamma(alpha + beta_) - torch.lgamma(alpha) - torch.lgamma(beta_) +
        torch.lgamma(alpha + k) + torch.lgamma(beta_ + n - k) -
        torch.lgamma(alpha + beta_ + n)
    )
    return nll.mean()

# ============================================================
# 3. BOUCLE LOCO (version corrigée pour reticulate)
# ============================================================
def train_loco(dat, emb_dim=8, hidden1=32, hidden2=16, lr=1e-3, n_epochs=1000, phi_init=10.0, device=None):
    if device is None:
        device = torch.device("cuda" if torch.cuda.is_available() else "cpu")

    n_J = dat['n_cohortes']
    sr_preds = np.zeros(n_J)
    sr_obs = dat['df_cohort']['SR_obs'].values

    # Conversion des inputs R -> Python / Torch
    X_cont_all = torch.tensor(dat['X_cont'], dtype=torch.float32, device=device)
    area_oh_all = torch.tensor(dat['area_oh'], dtype=torch.float32, device=device)
    Z_all = torch.tensor(dat['Z'], dtype=torch.float32, device=device)
    cohort_sizes_all = dat['df_cohort']['n_j'].values.astype(int)

    # Assurer cohorte_idx 1D
    coh_idx_array = np.asarray(dat['cohorte_idx']).ravel()

    for j_test in range(n_J):
        print(f"── LOCO cohorte {j_test+1} / {n_J}")

        # Cohortes d'entraînement
        idx_train_cohort = [i for i in range(n_J) if i != j_test]
        mask_indiv_train = np.isin(coh_idx_array, idx_train_cohort)

        X_cont_tr = X_cont_all[mask_indiv_train]
        area_oh_tr = area_oh_all[mask_indiv_train]

        df_tr = dat['df_cohort'].iloc[idx_train_cohort]
        Z_tr = Z_all[idx_train_cohort]
        k_tr = torch.tensor(df_tr['k_j'].values, dtype=torch.float32, device=device)
        n_tr = torch.tensor(df_tr['n_j'].values, dtype=torch.float32, device=device)
        sizes_tr = df_tr['n_j'].values.astype(int)

        # Reindex pour embedding
        old2new = np.zeros(n_J, dtype=int)
        old2new[idx_train_cohort] = np.arange(len(idx_train_cohort))
        coh_idx_tr_reindexed = torch.tensor(old2new[coh_idx_array[mask_indiv_train]], dtype=torch.long, device=device)

        # Modèle
        model = LLPNet(
            n_cont=X_cont_tr.shape[1],
            n_area=dat['n_area'],
            n_cohortes=len(idx_train_cohort),
            emb_dim=emb_dim,
            hidden1=hidden1,
            hidden2=hidden2,
            n_Z=dat['n_Z'],
            phi_init=phi_init
        ).to(device)

        optimizer = optim.Adam(model.parameters(), lr=lr)
        model.train()

        # Entraînement
        for epoch in range(n_epochs):
            optimizer.zero_grad()
            mu_j, phi = model(X_cont_tr, area_oh_tr, coh_idx_tr_reindexed, Z_tr, sizes_tr)
            loss = beta_binomial_nll(k_tr, n_tr, mu_j, phi)
            loss.backward()
            optimizer.step()
            if (epoch + 1) % 100 == 0:
                print(f"   epoch {epoch + 1} | loss {loss.item():.4f}")

        # Prediction cohorte test
        model.eval()
        with torch.no_grad():
            mask_test = coh_idx_array == j_test
            X_cont_te = X_cont_all[mask_test]
            area_oh_te = area_oh_all[mask_test]

            emb_mean = model.emb_cohorte.weight.mean(dim=0, keepdim=True).expand(X_cont_te.shape[0], emb_dim)
            x_in_te = torch.cat([X_cont_te, area_oh_te, emb_mean], dim=1)
            h_te = model.encoder(x_in_te).mean(dim=0, keepdim=True)

            Z_te = Z_all[j_test].unsqueeze(0) if len(Z_all.shape) == 2 else Z_all[j_test:j_test+1, :]
            dec_in_te = torch.cat([h_te, Z_te], dim=1)
            eta_te = model.decoder(dec_in_te)
            sr_preds[j_test] = torch.sigmoid(eta_te).item()

    return {'sr_obs': sr_obs, 'sr_preds': sr_preds}