# ================================================================
# PIPELINE COMPLET — Merlangius merlangus
# GAM sex-ratio (te Age/Lngt, s spatial, s saisonnier, re cohorte)# + noyau gaussien thermique sur anomalie GAM (tendance+saison retirées)
# + lambda fixés aux valeurs REML de bam_ref (résout la déficience de rang)
# + b_cohort_resid en effet aléatoire (Laplace, RTMB)
# ================================================================

library(dplyr)
library(mgcv)
library(lubridate)
library(RTMB)
library(Matrix)

stopifnot(exists("df_long"))
stopifnot(exists("data_expanded_1991_2023"))

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
#    L'anomalie (résidu) élimine la tendance interannuelle,
#    ce qui évite la colinéarité cohorte <-> index thermique
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
  dplyr::filter(Species == "Merlangius merlangus", !is.na(Numeric_sex)) %>%
  droplevels()

cat("N =", nrow(data_sp), "\n")

k_age  <- min(15, n_distinct(data_sp$Age_sc) - 1)
k_lngt <- min(20, n_distinct(data_sp$LngtClassGrouped_sc) - 1)
k_space <- 150
k_time  <- 24   # Merlangius merlangus : k_time_double

# ----------------------------------------------------------------
# 4. GAM de référence (bam) — sert à extraire les lambda REML
# ----------------------------------------------------------------

formula_sp <- as.formula(bquote(Numeric_sex ~
  te(Age_sc, LngtClassGrouped_sc,
     k = c(.(k_age), .(k_lngt)), bs = c("cr", "cr")) +
  s(Latitude, Longitude, k = .(k_space), bs = "sos") +
  s(julian_day,          k = .(k_time),  bs = "cc") +
  s(Cohort_fact,         bs = "re")
))

bam_ref <- bam(
  formula_sp,
  family   = binomial(link = "logit"),
  data     = data_sp,
  method   = "fREML",
  discrete = TRUE,
  keepData = TRUE
)

summary(bam_ref)$s.table

lambda_bam_ref <- bam_ref$sp
print(lambda_bam_ref)

# ----------------------------------------------------------------
# 5. Extraction de la structure GAM (matrices de design + pénalités)
# ----------------------------------------------------------------

extract_gam_structure <- function(data_sp, k_age, k_lngt, k_space, k_time) {
  formula_sp <- as.formula(bquote(Numeric_sex ~
    te(Age_sc, LngtClassGrouped_sc,
       k = c(.(k_age), .(k_lngt)), bs = c("cr", "cr")) +
    s(Latitude, Longitude, k = .(k_space), bs = "sos") +
    s(julian_day,          k = .(k_time),  bs = "cc") +
    s(Cohort_fact,         bs = "re")
  ))

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
    X_fixed           = gam_setup$X[, cols_fixed, drop = FALSE],
    penalty_list      = penalty_list,
    cohort_levels     = levels(data_sp$Cohort_fact),
    cohort_id_per_obs = as.integer(data_sp$Cohort_fact)
  )
}

gam_struct_full <- extract_gam_structure(data_sp, k_age, k_lngt, k_space, k_time)

cat("Dim X_fixed :", paste(dim(gam_struct_full$X_fixed), collapse = " x "), "\n")
cat("Colonne 1 == intercept :", all(gam_struct_full$X_fixed[, 1] == 1), "\n")

# ----------------------------------------------------------------
# 6. Retrait de l'intercept redondant (colinéaire avec intercept_cohort)
# ----------------------------------------------------------------

X_fixed_no_intercept <- gam_struct_full$X_fixed[, -1, drop = FALSE]

penalty_list_adjusted <- lapply(gam_struct_full$penalty_list, function(p) {
  p$cols_local <- p$cols_local - 1
  p
})

cat("Dim X_fixed_no_intercept :", paste(dim(X_fixed_no_intercept), collapse = " x "), "\n")

# ----------------------------------------------------------------
# 7. Alignement des lambda REML avec les 4 blocs de penalty_list
#    (on exclut s(Cohort_fact), géré séparément via b_cohort_resid)
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

# --- Vérification du rang avec pénalité fixée (doit être complet) ---
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
# 8. Construction de l'index thermique étendu (sur l'anomalie GAM)
# ----------------------------------------------------------------

build_temp_extended <- function(monthly_series, cohort_years) {
  month_offsets <- -3:15
  n_cohort <- length(cohort_years)
  temp_extended <- matrix(NA_real_, nrow = n_cohort, ncol = length(month_offsets))

  for (i in seq_along(cohort_years)) {
    yr <- cohort_years[i]
    target_dates <- as.Date(paste0(yr, "-01-01")) %m+% months(month_offsets)
    idx_match <- match(target_dates, monthly_series$date)
    temp_extended[i, ] <- monthly_series$temp_moy_bassin[idx_match]
  }

  list(temp_extended = temp_extended, month_offsets = month_offsets)
}

cohort_years_sp <- as.numeric(as.character(gam_struct_full$cohort_levels))

res_build_sp <- build_temp_extended(
  monthly_series = temp_surface_mensuelle %>%
    dplyr::select(date, temp_anomaly_gam) %>%
    dplyr::rename(temp_moy_bassin = temp_anomaly_gam),
  cohort_years   = cohort_years_sp
)

cat("NA dans temp_extended :", sum(is.na(res_build_sp$temp_extended)), "\n")

w_uniform <- rep(1/19, 19)
thermal_index_check <- as.vector(res_build_sp$temp_extended %*% w_uniform)
cat("Corrélation cohorte vs anomalie thermique (approx uniforme) :",
    cor(cohort_years_sp, thermal_index_check), "\n")

# ----------------------------------------------------------------
# Étapes 1-8 : identiques au pipeline précédent
# (anomalie GAM, extraction structure, retrait intercept,
#  lambda fixés REML, construction temp_extended)
# On repart directement de res_build_sp$temp_extended déjà construit
# ----------------------------------------------------------------

month_offsets <- -3:15  # 19 mois

data_full_w <- list(
  y                 = data_sp$Numeric_sex,
  X_fixed           = X_fixed_no_intercept,
  cohort_id         = gam_struct_full$cohort_id_per_obs,
  temp_extended     = res_build_sp$temp_extended,   # anomalie GAM, n_cohort x 19
  penalty_list      = penalty_list_adjusted,
  lambda_fixed      = lambda_bam_ref_aligned
)

parameters_full_w <- list(
  beta_fixed          = rep(0, ncol(X_fixed_no_intercept)),
  intercept_cohort    = 0,
  w_month             = rep(0, 19),         # poids libres, un par mois (-3 à 15)
  log_sigma_w         = 0,                  # échelle de la pénalité sur w_month (ridge adaptatif)
  b_cohort_resid      = rep(0, length(gam_struct_full$cohort_levels)),
  log_sigma_cohort_resid = 0
)

f_full_w <- function(parms) {
  getAll(parms, data_full_w)

  sigma_w             <- exp(log_sigma_w)
  sigma_cohort_resid  <- 0.01 + 0.6 / (1 + exp(-log_sigma_cohort_resid))
  lambda              <- lambda_fixed

  # --- Index thermique par cohorte : somme pondérée sur les 19 mois ---
  # w_month est GLOBAL, partagé par toutes les cohortes.
  # Seule temp_extended[c, ] (les vraies anomalies vécues) varie par cohorte.
  thermal_index <- as.vector(temp_extended %*% w_month)

  # --- Effet cohorte : part thermique (linéaire en thermal_index) + résidu ---
  cohort_pred_thermal <- intercept_cohort + thermal_index
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

  # Effet aléatoire cohorte (résidu)
  nll_resid_cohort <- -sum(dnorm(b_cohort_resid, mean = 0, sd = sigma_cohort_resid, log = TRUE))

  # Effet aléatoire sur les poids mensuels (= pénalité ridge adaptative, L2)
  nll_resid_weights <- -sum(dnorm(w_month, mean = 0, sd = sigma_w, log = TRUE))

  nll <- nll_obs + nll_penalty + nll_resid_cohort + nll_resid_weights

  prob <- plogis(eta)

  REPORT(w_month)
  REPORT(sigma_w)
  REPORT(sigma_cohort_resid)
  REPORT(thermal_index)
  REPORT(cohort_pred_thermal)
  REPORT(b_cohort_resid)
  REPORT(prob)
  REPORT(eta)
  ADREPORT(w_month)
  ADREPORT(sigma_w)

  nll
}

# ----------------------------------------------------------------
# Construction et optimisation
# ----------------------------------------------------------------

cat("\n--- Construction objet AD (poids mensuels libres, effets aléatoires) ---\n")
obj_full_w <- RTMB::MakeADFun(
  f_full_w, parameters_full_w,
  random = c("b_cohort_resid", "w_month"),
  silent = TRUE
)

fn0 <- obj_full_w$fn(obj_full_w$par)
gr0 <- obj_full_w$gr(obj_full_w$par)
cat("fn(par_init) =", fn0, "| fini :", is.finite(fn0), "\n")
cat("NA/NaN gradient :", sum(!is.finite(gr0)), "sur", length(gr0), "\n")

cat("\n--- PASSE 1 ---\n")
opt1_w <- nlminb(
  obj_full_w$par, obj_full_w$fn, obj_full_w$gr,
  control = list(trace = 1, iter.max = 3000, eval.max = 6000)
)
cat("Convergence :", opt1_w$convergence, "| Message :", opt1_w$message, "\n")

cat("\n--- PASSE 2 ---\n")
opt2_w <- nlminb(
  opt1_w$par, obj_full_w$fn, obj_full_w$gr,
  control = list(trace = 1, iter.max = 3000, eval.max = 6000, rel.tol = 1e-10)
)
cat("Convergence :", opt2_w$convergence, "| Message :", opt2_w$message, "\n")

obj_full_w$fn(opt2_w$par)
sd_rep_w <- sdreport(obj_full_w)
cat("\npdHess :", sd_rep_w$pdHess, "\n")

rep_final_w <- obj_full_w$report(obj_full_w$env$last.par.best)
cat("\nsigma_w final :", rep_final_w$sigma_w, "\n")
cat("Profil des poids w_month (mois -3 à 15) :\n")
print(round(rep_final_w$w_month, 4))

print(summary(sd_rep_w, select = "report"))