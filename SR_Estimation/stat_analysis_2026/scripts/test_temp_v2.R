# ================================================================
# PIPELINE COMPLET v2 - Merlangius merlangus
# GAM sex-ratio (te Age/Lngt, s spatial, s saisonnier)
# + fenêtre thermique lissée (P-spline : ridge + rugosité RW2) sur
#   l'anomalie GAM, estimée conjointement à un résidu cohorte (RTMB)
#
#   0. bam_ref fitté avec discrete = FALSE : garantit que les
#      noeuds utilisés pour lambda_bam_ref sont EXACTEMENT ceux utilisés
#      par extract_gam_structure().
#   1. Vérification explicite que le NLL RTMB reproduit bam_ref
#      (modèle réduit, cohorte IID) avant d'interpréter le modèle complet.
#   2. Modèle nul (sans thermal_index) fitté en parallèle : comparaison
#      formelle (AIC marginal, réduction de sigma_cohort).
#   3. sigma_cohort_resid n'est plus plafonné arbitrairement : 
#      vérification post-fit de bordure sur tous les log_sigma_*.
#   4. w_month passe d'un ridge IID à une pénalité P-spline (ridge +
#      rugosité d'ordre 2), pour refléter l'hypothèse d'une fenêtre
#      contiguë plutôt qu'un vecteur de poids indépendants.
#   5. Diagnostics de colinéarité post-fit.
#   6. Test de permutation (calibration sous H0), désactivé par défaut
#      (coûteux), pour borner le risque de faux positif avec ~40 cohortes.
#   7. Alignement des lignes : X_fixed peut être plus court que data_sp
#      si na.omit a supprimé des lignes sur un prédicteur (pas seulement
#      Numeric_sex).
#   8. Nettoyage : formule construite une seule fois, constantes
#      (month_offsets, n_months) définies une seule fois, k_time
#      paramétré par espèce au lieu d'un magic number commenté.
# ================================================================

library(dplyr)
library(mgcv)
library(lubridate)
library(RTMB)
library(Matrix)

stopifnot(exists("df_long"))
stopifnot(exists("data_expanded_1991_2023"))

# ----------------------------------------------------------------
# 0. Constantes partagées
# ----------------------------------------------------------------

MONTH_OFFSETS <- -3:14
N_MONTHS      <- length(MONTH_OFFSETS)

SPECIES_NAME <- "Merlangius merlangus"

# k_time par espèce : ponte étalée sur l'année -> k_time doublé.
# A remplir au fur et à mesure des espèces traitées avec ce pipeline.
K_TIME_BY_SPECIES <- list(
  "Merlangius merlangus"  = 24,
  "Trisopterus esmarkii"  = 24
)
k_time_default <- 12

# Cache disque pour bam_ref et le modèle nul : ces deux fits sont identiques
# à chaque run tant que data_sp/formula_sp ne changent pas - les recalculer
# à chaque essai de la fenêtre thermique fait perdre du temps pour rien.
# Mettre FORCE_REFIT_* à TRUE pour forcer un recalcul (ex. après avoir changé
# k_age/k_lngt/k_space/k_time, la formule, ou les données en entrée).
CACHE_DIR <- "cache"
if (!dir.exists(CACHE_DIR)) dir.create(CACHE_DIR, recursive = TRUE)
FORCE_REFIT_BAM_REF <- FALSE
FORCE_REFIT_NULL    <- FALSE

# ----------------------------------------------------------------
# 1. Série temporelle mensuelle agrégée (bassin)
# ----------------------------------------------------------------

temp_surface_mensuelle <- df_long %>%
  dplyr::group_by(time) %>%
  dplyr::summarise(temp_moy_bassin = mean(temperature, na.rm = TRUE)) %>%
  dplyr::rename(date = time) %>%
  dplyr::arrange(date)

# ----------------------------------------------------------------
# 2. Anomalie thermique : GAM tendance (tp) + saisonnalité (cc)
# ----------------------------------------------------------------

temp_surface_mensuelle <- temp_surface_mensuelle %>%
  dplyr::mutate(
    mois           = as.numeric(format(date, "%m")),
    annee_continue = as.numeric(format(date, "%Y")) + (mois - 1) / 12,
    mois_cyclique  = mois
  )

gam_temp <- mgcv::gam(
  temp_moy_bassin ~ s(annee_continue, bs = "tp", k = 20) +
                    s(mois_cyclique, bs = "cc", k = 12),
  data   = temp_surface_mensuelle,
  method = "REML",
  knots  = list(mois_cyclique = c(0.5, 12.5))
)

temp_surface_mensuelle <- temp_surface_mensuelle %>%
  dplyr::mutate(temp_anomaly_gam = residuals(gam_temp))

cat("SD anomalie GAM :", sd(temp_surface_mensuelle$temp_anomaly_gam), "\n")
cat("Corrélation anomalie GAM vs temps :",
    cor(as.numeric(temp_surface_mensuelle$date), temp_surface_mensuelle$temp_anomaly_gam), "\n")

# ----------------------------------------------------------------
# 3. Données espèce
# ----------------------------------------------------------------

data_sp <- data_expanded_1991_2023 %>%
  dplyr::filter(Species == SPECIES_NAME, !is.na(Numeric_sex)) %>%
  droplevels()

cat("N =", nrow(data_sp), "\n")

k_age   <- min(15, n_distinct(data_sp$Age_sc) - 1)
k_lngt  <- min(20, n_distinct(data_sp$LngtClassGrouped_sc) - 1)
k_space <- 120
k_time  <- if (!is.null(K_TIME_BY_SPECIES[[SPECIES_NAME]])) {
  K_TIME_BY_SPECIES[[SPECIES_NAME]]
} else {
  k_time_default
}

# ----------------------------------------------------------------
# 4. Formule GAM - construite une seule fois, réutilisée partout
# ----------------------------------------------------------------

build_formula_sp <- function(k_age, k_lngt, k_space, k_time) {
  as.formula(bquote(Numeric_sex ~
    te(Age_sc, LngtClassGrouped_sc,
       k = c(.(k_age), .(k_lngt)), bs = c("cr", "cr")) +
    s(Latitude, Longitude, k = .(k_space), bs = "sos") +
    s(julian_day,          k = .(k_time),  bs = "cc") +
    s(Cohort_fact,         bs = "re")
  ))
}

formula_sp <- build_formula_sp(k_age, k_lngt, k_space, k_time)

# ----------------------------------------------------------------
# 5. GAM de référence (bam) - extraire les lambda REML
#    discrete = FALSE : garantit les mêmes noeuds que
#    extract_gam_structure()
# ----------------------------------------------------------------

model_sig <- paste(SPECIES_NAME, k_age, k_lngt, k_space, k_time, nrow(data_sp), sep = "_")
bam_ref_cache_file <- file.path(CACHE_DIR, paste0("bam_ref_", model_sig, ".rds"))

if (!FORCE_REFIT_BAM_REF && file.exists(bam_ref_cache_file)) {
  cat("bam_ref chargé depuis le cache :", bam_ref_cache_file, "\n")
  bam_ref <- readRDS(bam_ref_cache_file)
} else {
  bam_ref <- bam(
    formula_sp,
    family   = binomial(link = "logit"),
    data     = data_sp,
    method   = "fREML",
    discrete = FALSE,
    keepData = TRUE
  )
  saveRDS(bam_ref, bam_ref_cache_file)
  cat("bam_ref sauvegardé dans :", bam_ref_cache_file, "\n")
}

summary(bam_ref)$s.table

lambda_bam_ref <- bam_ref$sp
print(lambda_bam_ref)

# ----------------------------------------------------------------
# 6. Extraction de la structure GAM (matrices de design + pénalités)
# ----------------------------------------------------------------

extract_gam_structure <- function(data_sp, formula_sp) {
  gam_setup <- mgcv::gam(
    formula_sp,
    family = binomial(link = "logit"),
    data   = data_sp,
    fit    = FALSE
  )

  smooth_info <- gam_setup$smooth
  cohort_idx <- which(sapply(smooth_info, function(s) s$term[1]) == "Cohort_fact")

  cols_cohort <- smooth_info[[cohort_idx]]$first.para:smooth_info[[cohort_idx]]$last.para
  cols_fixed  <- setdiff(seq_len(ncol(gam_setup$X)), cols_cohort)

  penalty_list <- list()
  for (i in seq_along(smooth_info)) {
    if (i == cohort_idx) next
    s_term <- smooth_info[[i]]
    cols_term <- s_term$first.para:s_term$last.para
    local_start <- 1 + (cols_term[1] - cols_fixed[1])
    for (j in seq_along(s_term$S)) {
      penalty_list[[length(penalty_list) + 1]] <- list(
        S = s_term$S[[j]],
        cols_local = local_start:(local_start + nrow(s_term$S[[j]]) - 1)
      )
    }
  }

  list(
    X_fixed   = gam_setup$X[, cols_fixed, drop = FALSE],
    penalty_list = penalty_list,
    na_action = gam_setup$na.action
  )
}

gam_struct_full <- extract_gam_structure(data_sp, formula_sp)

cat("Dim X_fixed :", paste(dim(gam_struct_full$X_fixed), collapse = " x "), "\n")
cat("Colonne 1 == intercept :", all(gam_struct_full$X_fixed[, 1] == 1), "\n")

# --- Si na.omit a supprimé des lignes sur un prédicteur ---
if (!is.null(gam_struct_full$na_action)) {
  cat("ATTENTION :", length(gam_struct_full$na_action),
      "lignes supprimées par na.omit (NA sur un prédicteur du GAM) - réalignement de data_sp.\n")
  data_sp <- data_sp[-gam_struct_full$na_action, , drop = FALSE]
  data_sp$Cohort_fact <- droplevels(data_sp$Cohort_fact)
}

stopifnot(nrow(gam_struct_full$X_fixed) == nrow(data_sp))

cohort_levels     <- levels(data_sp$Cohort_fact)
cohort_id_per_obs <- as.integer(data_sp$Cohort_fact)
n_cohort          <- length(cohort_levels)

# ----------------------------------------------------------------
# 7. Retrait de l'intercept redondant (colinéaire avec intercept_cohort)
# ----------------------------------------------------------------

X_fixed_no_intercept <- gam_struct_full$X_fixed[, -1, drop = FALSE]

penalty_list_adjusted <- lapply(gam_struct_full$penalty_list, function(p) {
  p$cols_local <- p$cols_local - 1
  p
})

cat("Dim X_fixed_no_intercept :", paste(dim(X_fixed_no_intercept), collapse = " x "), "\n")

# ----------------------------------------------------------------
# 8. Alignement des lambda REML avec les 4 blocs de penalty_list
# ----------------------------------------------------------------

lambda_bam_ref_aligned <- as.numeric(lambda_bam_ref[c(
  "te(Age_sc,LngtClassGrouped_sc)1",
  "te(Age_sc,LngtClassGrouped_sc)2",
  "s(Latitude,Longitude)",
  "s(julian_day)"
)])

cat("Lambda alignés :", paste(round(lambda_bam_ref_aligned, 3), collapse = ", "), "\n")
cat("Nb lambda :", length(lambda_bam_ref_aligned), "| Nb blocs pénalité :",
    length(penalty_list_adjusted), "\n")

XtX <- t(X_fixed_no_intercept) %*% X_fixed_no_intercept
S_full <- matrix(0, ncol(X_fixed_no_intercept), ncol(X_fixed_no_intercept))
for (k in seq_along(penalty_list_adjusted)) {
  cols_k <- penalty_list_adjusted[[k]]$cols_local
  S_full[cols_k, cols_k] <- S_full[cols_k, cols_k] +
    lambda_bam_ref_aligned[k] * penalty_list_adjusted[[k]]$S
}
rank_penalized <- Matrix::rankMatrix(XtX + S_full)
cat("Rang de X'X + lambda*S :", rank_penalized[1], "sur", ncol(XtX), "\n")

# ----------------------------------------------------------------
# 9. Index thermique étendu (sur l'anomalie GAM), NA gérés explicitement
# ----------------------------------------------------------------

build_temp_extended <- function(monthly_series, cohort_years, month_offsets) {
  n_cohort <- length(cohort_years)
  temp_extended <- matrix(NA_real_, nrow = n_cohort, ncol = length(month_offsets))

  for (i in seq_along(cohort_years)) {
    yr <- cohort_years[i]
    target_dates <- as.Date(paste0(yr, "-01-01")) %m+% months(month_offsets)
    idx_match <- match(target_dates, monthly_series$date)
    temp_extended[i, ] <- monthly_series$temp_moy_bassin[idx_match]
  }

  temp_extended
}

cohort_years_sp <- as.numeric(as.character(cohort_levels))

temp_extended <- build_temp_extended(
  monthly_series = temp_surface_mensuelle %>%
    dplyr::select(date, temp_anomaly_gam) %>%
    dplyr::rename(temp_moy_bassin = temp_anomaly_gam),
  cohort_years  = cohort_years_sp,
  month_offsets = MONTH_OFFSETS
)

n_na <- sum(is.na(temp_extended))
if (n_na > 0) {
  na_cohorts <- cohort_years_sp[apply(temp_extended, 1, anyNA)]
  stop(sprintf(
    paste0(
      "temp_extended contient %d NA (cohortes en dehors de la couverture temporelle de ",
      "temp_surface_mensuelle, probablement en bord de série 1991/2023). Cohortes concernées : %s. ",
      "Choix a faire explicitement : (a) retirer ces cohortes de data_sp, ou ",
      "(b) reduire MONTH_OFFSETS pour qu'il tienne dans la couverture disponible. ",
      "Ne pas laisser passer silencieusement."
    ),
    n_na, paste(na_cohorts, collapse = ", ")
  ))
}

w_uniform <- rep(1 / N_MONTHS, N_MONTHS)
thermal_index_check <- as.vector(temp_extended %*% w_uniform)
cat("Corrélation cohorte vs anomalie thermique (approx uniforme) :",
    cor(cohort_years_sp, thermal_index_check), "\n")

# ----------------------------------------------------------------
# 10. Modèle NUL (cohorte IID, sans thermal_index) - référence
#     pour (a) valider le NLL RTMB contre bam_ref, (b) comparer au
#     modèle complet (AIC marginal, reduction de sigma_cohort)
# ----------------------------------------------------------------

data_null <- list(
  y            = data_sp$Numeric_sex,
  X_fixed      = X_fixed_no_intercept,
  cohort_id    = cohort_id_per_obs,
  penalty_list = penalty_list_adjusted,
  lambda_fixed = lambda_bam_ref_aligned
)

parameters_null <- list(
  beta_fixed      = rep(0, ncol(X_fixed_no_intercept)),
  b_cohort        = rep(0, n_cohort),
  log_sigma_cohort = 0
)

make_f_null_iid <- function(data_null) {
  function(parms) {
    getAll(parms, data_null)

    sigma_cohort <- exp(log_sigma_cohort)

    eta <- as.vector(X_fixed %*% beta_fixed) + b_cohort[cohort_id]

    log_prob    <- -log1p(exp(-eta))
    log_1m_prob <- -log1p(exp(eta))
    nll_obs <- -sum(y * log_prob + (1 - y) * log_1m_prob)

    nll_penalty <- 0
    for (k in seq_len(length(penalty_list))) {
      cols_k <- penalty_list[[k]]$cols_local
      beta_k <- beta_fixed[cols_k]
      S_k    <- penalty_list[[k]]$S
      nll_penalty <- nll_penalty + 0.5 * lambda_fixed[k] * as.numeric(t(beta_k) %*% S_k %*% beta_k)
    }

    nll_resid_cohort <- -sum(dnorm(b_cohort, mean = 0, sd = sigma_cohort, log = TRUE))

    nll <- nll_obs + nll_penalty + nll_resid_cohort

    REPORT(eta)
    REPORT(sigma_cohort)
    REPORT(b_cohort)

    nll
  }
}

cat("\n--- Modèle nul (cohorte IID, sans index thermique) ---\n")
obj_null <- RTMB::MakeADFun(make_f_null_iid(data_null), parameters_null,
                             random = "b_cohort", silent = TRUE)

null_model_cache_file <- file.path(CACHE_DIR, paste0("null_model_", model_sig, ".rds"))

if (!FORCE_REFIT_NULL && file.exists(null_model_cache_file)) {
  cat("Modèle nul chargé depuis le cache :", null_model_cache_file, "\n")
  null_cache <- readRDS(null_model_cache_file)
  opt_null   <- null_cache$opt_null
  rep_null   <- null_cache$rep_null
} else {
  opt_null_1 <- nlminb(obj_null$par, obj_null$fn, obj_null$gr,
                        control = list(trace = 0, iter.max = 5000, eval.max = 10000,
                                       rel.tol = 1e-12, x.tol = 1e-10))
  opt_null <- nlminb(opt_null_1$par, obj_null$fn, obj_null$gr,
                      control = list(trace = 0, iter.max = 5000, eval.max = 10000,
                                     rel.tol = 1e-12, x.tol = 1e-10))
  rep_null <- obj_null$report(obj_null$env$last.par.best)
  saveRDS(list(opt_null = opt_null, rep_null = rep_null), null_model_cache_file)
  cat("Modèle nul sauvegardé dans :", null_model_cache_file, "\n")
}
cat("Convergence modèle nul :", opt_null$convergence, "| Message :", opt_null$message, "\n")
g_null <- obj_null$gr(opt_null$par)
cat("Max |gradient| modele nul :", max(abs(g_null)), "\n")
# ----------------------------------------------------------------
# 11. VALIDATION - le NLL RTMB reproduit-il bam_ref ?
# ----------------------------------------------------------------

eta_bam_ref <- bam_ref$linear.predictors
eta_rtmb_null <- rep_null$eta

stopifnot(length(eta_bam_ref) == length(eta_rtmb_null))

cor_eta <- cor(eta_bam_ref, eta_rtmb_null)
rmse_eta <- sqrt(mean((eta_bam_ref - eta_rtmb_null)^2))

cat("\n--- Validation RTMB vs bam_ref (modele nul, cohorte IID) ---\n")
cat("Cor(eta_bam_ref, eta_rtmb) :", cor_eta, "\n")
cat("RMSE(eta_bam_ref, eta_rtmb) :", rmse_eta, "\n")

if (cor_eta < 0.999) {
  warning(paste0(
    "Le predicteur lineaire RTMB (modele nul) ne reproduit pas bam_ref d'assez pres ",
    "(cor = ", round(cor_eta, 4), "). Ne pas interpreter le modele complet avant d'avoir ",
    "diagnostique cet ecart (alignement colonnes X_fixed/penalty_list, echelle du lien, ",
    "parametrisation intercept/cohorte)."
  ))
}

# ----------------------------------------------------------------
# 12. Modele complet - fenetre thermique softmax lissee (RW2 sur logits)
#     + reponse lineaire slope_cohort + residu cohorte
# ----------------------------------------------------------------

D2 <- diff(diag(N_MONTHS), differences = 2)  # (N_MONTHS-2) x N_MONTHS, opere sur a_full (19 logits)

data_full_w <- list(
  y             = data_sp$Numeric_sex,
  X_fixed       = X_fixed_no_intercept,
  cohort_id     = cohort_id_per_obs,
  temp_extended = temp_extended,
  penalty_list  = penalty_list_adjusted,
  lambda_fixed  = lambda_bam_ref_aligned,
  D2            = D2
)

check_finite <- function(x, label) {
  if (any(!is.finite(x))) {
    stop(sprintf("%s contient %d valeur(s) non finie(s) (NA/NaN/Inf) - corriger avant de construire l'AD.",
                  label, sum(!is.finite(x))))
  }
}
check_finite(data_full_w$y, "data_full_w$y")
check_finite(data_full_w$X_fixed, "data_full_w$X_fixed")
check_finite(data_full_w$cohort_id, "data_full_w$cohort_id")
check_finite(data_full_w$temp_extended, "data_full_w$temp_extended")
check_finite(data_full_w$lambda_fixed, "data_full_w$lambda_fixed")
check_finite(data_full_w$D2, "data_full_w$D2")
for (k in seq_along(data_full_w$penalty_list)) {
  check_finite(data_full_w$penalty_list[[k]]$S, sprintf("data_full_w$penalty_list[[%d]]$S", k))
}

# a_month : 18 logits libres (le 1er mois est ancre a 0 dans la fonction).
# a_month a maintenant un prior de rugosite RW2 -> passe en effet aleatoire (section 13).
# log_sigma_rw : ecart-type des differences secondes des logits (controle le lissage).
# init depuis le modele nul (source fiable : opt_null$par, present dans le cache)
beta_null <- opt_null$par[names(opt_null$par) == "beta_fixed"]

set.seed(42)
parameters_full_w <- list(
  beta_fixed             = beta_null,                  # part de l'optimum GAM du nul
  intercept_cohort       = 0,
  # a_month                = rep(0, N_MONTHS - 1),
  a_month                = rnorm(N_MONTHS-1,0,0.2),
  log_sigma_rw           = -1,
  slope_cohort           = 0.3,                          # test : reproduit exactement le nul
  b_cohort_resid         = rep_null$b_cohort,
  log_sigma_cohort_resid = log(rep_null$sigma_cohort)
)

make_f_full_w <- function(data_full_w) {
  function(parms) {
    getAll(parms, data_full_w)

    sigma_cohort_resid <- exp(log_sigma_cohort_resid)
    sigma_rw           <- exp(log_sigma_rw)
    lambda             <- lambda_fixed

    # --- Fenetre softmax ancree ---
    # a_full : on prefixe un 0 (ancrage de la translation). N_MONTHS logits au total.
    a_full  <- c(0, a_month)
    a_ctr   <- a_full - mean(a_full)
    w_exp   <- exp(a_ctr)
    w_month <- w_exp / sum(w_exp)

    thermal_index <- as.vector(temp_extended %*% w_month)

    cohort_pred_thermal <- intercept_cohort + slope_cohort * thermal_index
    cohort_effect_total <- cohort_pred_thermal + b_cohort_resid

    eta <- as.vector(X_fixed %*% beta_fixed) + cohort_effect_total[cohort_id]

    log_prob    <- -log1p(exp(-eta))
    log_1m_prob <- -log1p(exp(eta))
    nll_obs <- -sum(y * log_prob + (1 - y) * log_1m_prob)

    nll_penalty <- 0
    for (k in seq_len(length(penalty_list))) {
      cols_k <- penalty_list[[k]]$cols_local
      beta_k <- beta_fixed[cols_k]
      S_k    <- penalty_list[[k]]$S
      nll_penalty <- nll_penalty + 0.5 * lambda[k] * as.numeric(t(beta_k) %*% S_k %*% beta_k)
    }

    nll_resid_cohort <- -sum(dnorm(b_cohort_resid, mean = 0, sd = sigma_cohort_resid, log = TRUE))

    # --- Prior de rugosite RW2 sur les logits (lissage de la fenetre) ---
    # opere sur a_full (19 logits, ancrage inclus) : la forme lissee inclut
    # le mois de reference. Retient les logits voisins ensemble -> empeche
    # un logit isole de filer sur le plateau plat du softmax (cause du pdHess=FALSE).
    delta2  <- as.vector(D2 %*% a_full)
    nll_rw  <- -sum(dnorm(delta2, mean = 0, sd = sigma_rw, log = TRUE))

    # ridge sur les logits : empeche la divergence vers +/- l'infini
    nll_a_ridge <- -sum(dnorm(a_month, mean = 0, sd = 3, log = TRUE))

    nll <- nll_obs + nll_penalty + nll_resid_cohort + nll_rw + nll_a_ridge

    prob <- plogis(eta)

    REPORT(w_month)
    REPORT(a_full)
    REPORT(slope_cohort)
    REPORT(sigma_cohort_resid)
    REPORT(sigma_rw)
    REPORT(thermal_index)
    REPORT(cohort_pred_thermal)
    REPORT(b_cohort_resid)
    REPORT(prob)
    REPORT(eta)
    ADREPORT(w_month)
    ADREPORT(slope_cohort)
    ADREPORT(sigma_cohort_resid)
    ADREPORT(sigma_rw)

    nll
  }
}

f_full_w <- make_f_full_w(data_full_w)
# ----------------------------------------------------------------
# 13. Optimisation avec redemarrages multiples
# ----------------------------------------------------------------

fit_with_restarts <- function(obj, n_starts = 3, jitter_sd = 0.5, seed = 42) {
  set.seed(seed)
  par0 <- obj$par
  starts <- c(list(par0), lapply(seq_len(n_starts - 1), function(i) {
    par0 + rnorm(length(par0), 0, jitter_sd)
  }))

  fits <- lapply(starts, function(p0) {
    o1 <- tryCatch(
      nlminb(p0, obj$fn, obj$gr, control = list(trace = 1, iter.max = 3000, eval.max = 6000)),
      error = function(e) NULL
    )
    if (is.null(o1)) return(NULL)
    tryCatch(
      nlminb(o1$par, obj$fn, obj$gr,
             control = list(trace = 1, iter.max = 3000, eval.max = 6000, rel.tol = 1e-10)),
      error = function(e) NULL
    )
  })

  ok <- !sapply(fits, is.null)
  if (!any(ok)) stop("Tous les redemarrages ont echoue.")
  fits <- fits[ok]
  objectives <- sapply(fits, `[[`, "objective")

  list(best = fits[[which.min(objectives)]], all = fits, objectives = objectives)
}

cat("\n--- Construction objet AD (fenetre thermique lissee, effets aleatoires) ---\n")
# obj_full_w <- RTMB::MakeADFun(
#   f_full_w, parameters_full_w,
#   random = c("b_cohort_resid", "w_month"),
#   silent = TRUE
# )
obj_full_w <- RTMB::MakeADFun(
  f_full_w, parameters_full_w,
  random = c("b_cohort_resid", "a_month"),
  silent = TRUE
)
p0 <- obj_full_w$par
fn0 <- obj_full_w$fn(p0)
gr0 <- obj_full_w$gr(p0)
cat("fn0 :", fn0, "| fini :", is.finite(fn0), "\n")
cat("NaN gradient :", sum(!is.finite(gr0)), "/", length(gr0), "\n")
cat("Max |gradient| :", max(abs(gr0)), "\n")
cat("Gradient sur slope_cohort :", gr0[names(p0) == "slope_cohort"], "\n")

cat("\n--- Optimisation (3 redemarrages) ---\n")
res_full <- fit_with_restarts(obj_full_w, n_starts = 1)
cat("Objectifs des redemarrages :", paste(round(res_full$objectives, 3), collapse = ", "), "\n")
if (diff(range(res_full$objectives)) > 1e-2) {
  warning("Les redemarrages convergent vers des optima sensiblement differents - ",
          "la surface de vraisemblance est probablement plate/multi-modale ici. ",
          "Inspecter avant d'interpreter w_month.")
}

opt_full_w <- res_full$best
cat("Convergence modele complet :", opt_full_w$convergence, "| Message :", opt_full_w$message, "\n")

obj_full_w$fn(opt_full_w$par)
sd_rep_w <- sdreport(obj_full_w)
cat("\npdHess :", sd_rep_w$pdHess, "\n")
if (!isTRUE(sd_rep_w$pdHess)) {
  warning(paste0(
    "Hessienne non definie positive : erreurs-types et comparaison de vraisemblance non fiables. ",
    "Ne pas interpreter w_month / le test de reduction de variance sans corriger ",
    "(reparametrisation, plus de redemarrages, resserrer les priors)."
  ))
}

# rep_final_w <- obj_full_w$report(obj_full_w$env$last.par.best)

# # --- Verification de bordure sur les log_sigma_* estimes ---
# final_par <- opt_full_w$par
# check_boundary <- function(x, label, lo = -8, hi = 8) {
#   if (!is.na(x) && (x < lo || x > hi)) {
#     warning(sprintf(
#       "%s = %.2f est en bordure du domaine plausible (log-echelle) - verifier la convergence.",
#       label, x
#     ))
#   }
# }
# invisible(mapply(check_boundary,
#   final_par[names(final_par) %in% c("log_sigma_w", "log_sigma_rw", "log_sigma_cohort_resid")],
#   names(final_par)[names(final_par) %in% c("log_sigma_w", "log_sigma_rw", "log_sigma_cohort_resid")]
# ))

# cat("\nsigma_w  (ridge, magnitude fenetre)   :", rep_final_w$sigma_w, "\n")
# cat("sigma_rw (rugosite, lissage fenetre)  :", rep_final_w$sigma_rw, "\n")
# cat("sigma_cohort_resid (residu, non plafonne) :", rep_final_w$sigma_cohort_resid, "\n")
# cat("Profil des poids w_month (mois -3 a 15) :\n")
# print(round(rep_final_w$w_month, 4))

# print(summary(sd_rep_w, select = "report"))

rep_final_w <- obj_full_w$report(obj_full_w$env$last.par.best)

# --- Verification de bordure sur les log_sigma_* estimes ---
final_par <- opt_full_w$par
check_boundary <- function(x, label, lo = -8, hi = 8) {
  if (!is.na(x) && (x < lo || x > hi)) {
    warning(sprintf(
      "%s = %.2f est en bordure du domaine plausible - verifier la convergence.",
      label, x
    ))
  }
}
if ("log_sigma_cohort_resid" %in% names(final_par)) {
  check_boundary(final_par["log_sigma_cohort_resid"], "log_sigma_cohort_resid")
}

cat("\nslope_cohort (effet lineaire de thermal_index) :", rep_final_w$slope_cohort, "\n")
cat("sigma_cohort_resid (residu) :", rep_final_w$sigma_cohort_resid, "\n")
cat("sigma_rw (rugosite logits, lissage fenetre) :", rep_final_w$sigma_rw, "\n")
cat("Profil de la fenetre w_month (mois -3 a 15, softmax lissee) :\n")
print(round(rep_final_w$w_month, 4))
cat("Somme des poids (doit valoir 1) :", sum(rep_final_w$w_month), "\n")

print(summary(sd_rep_w, select = "report"))

# ----------------------------------------------------------------
# 14. Comparaison formelle modele nul vs modele complet
#     (AIC marginal via Laplace ; reduction de sigma_cohort comme
#     mesure descriptive de "part expliquee")
# ----------------------------------------------------------------

k_null <- length(obj_null$par)
k_full <- length(obj_full_w$par)

AIC_null <- 2 * opt_null$objective + 2 * k_null
AIC_full <- 2 * opt_full_w$objective + 2 * k_full
delta_AIC <- AIC_null - AIC_full  # > 0 : le modele complet est favorise

sigma_cohort_null_est <- rep_null$sigma_cohort
sigma_cohort_resid_full_est <- rep_final_w$sigma_cohort_resid
pseudo_R2_cohort <- 1 - (sigma_cohort_resid_full_est / sigma_cohort_null_est)^2

cat("\n--- Comparaison modele nul vs modele complet ---\n")
cat("AIC nul (cohorte IID)        :", round(AIC_null, 2), " (k =", k_null, ")\n")
cat("AIC complet (fenetre + resid):", round(AIC_full, 2), " (k =", k_full, ")\n")
cat("Delta AIC (nul - complet)    :", round(delta_AIC, 2),
    "(> 2 : evidence faible-moderee pour la fenetre thermique)\n")
cat("sigma_cohort (modele nul)        :", sigma_cohort_null_est, "\n")
cat("sigma_cohort_resid (modele complet) :", sigma_cohort_resid_full_est, "\n")
cat("Pseudo-R2 cohorte (reduction de variance sur l'echelle du lien) :",
    round(pseudo_R2_cohort, 3), "\n")

# ----------------------------------------------------------------
# 15. Diagnostics de colinearite post-fit
# ----------------------------------------------------------------

diag_cor <- data.frame(
  cohort_year   = cohort_years_sp,
  thermal_index = rep_final_w$thermal_index,
  b_resid       = rep_final_w$b_cohort_resid
)

cor_thermal_year  <- cor(diag_cor$thermal_index, diag_cor$cohort_year)
cor_thermal_resid <- cor(diag_cor$thermal_index, diag_cor$b_resid)
cor_year_resid    <- cor(diag_cor$cohort_year, diag_cor$b_resid)

cat("\n--- Diagnostics de colinearite (echelle cohorte, n =", n_cohort, ") ---\n")
cat("Cor(thermal_index, annee de cohorte)   :", round(cor_thermal_year, 3), "\n")
cat("Cor(thermal_index, residu cohorte)     :", round(cor_thermal_resid, 3), "\n")
cat("Cor(annee de cohorte, residu cohorte)  :", round(cor_year_resid, 3), "\n")

if (abs(cor_thermal_year) > 0.6) {
  warning(paste0(
    "thermal_index est fortement correle a l'annee de cohorte (cor = ", round(cor_thermal_year, 2), "). ",
    "Risque de confondre 'fenetre thermique specifique' et simple tendance interannuelle residuelle ",
    "(l'anomalie GAM peut ne pas avoir totalement retire la tendance basse frequence). ",
    "Interpreter w_month avec prudence ; envisager un detrending plus flexible de temp_anomaly_gam."
  ))
}
