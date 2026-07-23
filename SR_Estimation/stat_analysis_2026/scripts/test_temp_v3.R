# ================================================================
# PIPELINE COMPLET v3
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

MONTH_OFFSETS <- -3:17
N_MONTHS      <- length(MONTH_OFFSETS)

SPECIES_NAME <- "Merlangius merlangus"   # à changer pour chaque espèce traitée

# k_time par espèce
K_TIME_BY_SPECIES <- list(
  "Merlangius merlangus"  = 24,
  "Trisopterus esmarkii"  = 24,
  "Sprattus sprattus"     = 24,
  "Pleuronectes platessa" = 24
)                                     
k_time_default <- 12

# k_space par espèce
K_SPACE_BY_SPECIES <- list(
  "Pleuronectes platessa" = 240
)
k_space_default <- 120

# k_age par espèce
K_AGE_OVERRIDE <- list(
  "Melanogrammus aeglefinus" = 13
)

# Cache disque pour bam_ref et le modèle nul
# Mettre FORCE_REFIT_* à TRUE pour forcer un recalcul (ex. après avoir changé
# k_age/k_lngt/k_space/k_time, la formule, ou les données en entrée).
CACHE_DIR <- "cache"
if (!dir.exists(CACHE_DIR)) dir.create(CACHE_DIR, recursive = TRUE)
FORCE_REFIT_BAM_REF <- FALSE
FORCE_REFIT_NULL    <- FALSE

# ----------------------------------------------------------------
# 1. Série temporelle mensuelle agrégée
# ----------------------------------------------------------------

temp_surface_mensuelle <- df_long %>%
  dplyr::group_by(time) %>%
  dplyr::summarise(temp_moy_bassin = mean(temperature, na.rm = TRUE)) %>%
  dplyr::rename(date = time) %>%
  dplyr::arrange(date)

# ----------------------------------------------------------------
# 2. Anomalie thermique : GAM tendance + saisonnalité
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

k_age <- if (!is.null(K_AGE_OVERRIDE[[SPECIES_NAME]])) {
  K_AGE_OVERRIDE[[SPECIES_NAME]]
} else {
  min(15, n_distinct(data_sp$Age_sc) - 1)
}
k_lngt  <- min(20, n_distinct(data_sp$LngtClassGrouped_sc) - 1)
k_lngt  <- min(20, n_distinct(data_sp$LngtClassGrouped_sc) - 1)
k_space <- if (!is.null(K_SPACE_BY_SPECIES[[SPECIES_NAME]])) {
  K_SPACE_BY_SPECIES[[SPECIES_NAME]]
} else {
  k_space_default
}
k_time  <- if (!is.null(K_TIME_BY_SPECIES[[SPECIES_NAME]])) {
  K_TIME_BY_SPECIES[[SPECIES_NAME]]
} else {
  k_time_default
}

# ----------------------------------------------------------------
# 4. Formule GAM construite une seule fois
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
# 5. GAM de référence (bam) pour extraire les 
# hyperparamètres REML (lambda) et la structure du modèle
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
# 6. Extraction de la structure GAM (matrices des beta + pénalités)
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
# 7. Retrait de l'intercept GAM redondant
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
# 9. Index thermique étendu & NA gérés explicitement
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
# 10. Modèle NUL pour valider la NLL de RTMB contre bam_ref
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
                        control = list(trace = 1, iter.max = 5000, eval.max = 10000,
                                       rel.tol = 1e-12, x.tol = 1e-10))
  opt_null <- nlminb(opt_null_1$par, obj_null$fn, obj_null$gr,
                      control = list(trace = 1, iter.max = 5000, eval.max = 10000,
                                     rel.tol = 1e-12, x.tol = 1e-10))
  rep_null <- obj_null$report(obj_null$env$last.par.best)
  saveRDS(list(opt_null = opt_null, rep_null = rep_null), null_model_cache_file)
  cat("Modèle nul sauvegardé dans :", null_model_cache_file, "\n")
}
cat("Convergence modèle nul :", opt_null$convergence, "| Message :", opt_null$message, "\n")
g_null <- obj_null$gr(opt_null$par)
cat("Max |gradient| modele nul :", max(abs(g_null)), "\n")
# ----------------------------------------------------------------
# 11. La NLL RTMB reproduit-il bam_ref ?
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
# 12. Scenario simule : fenetre vraie connue + generation de y_sim
# ----------------------------------------------------------------

SIM_TAG <- "_SIM"

# --- Parametres de la verite simulee (a faire varier dans la boucle de puissance) ---
true_mu     <- 3       # offset du pic de la fenetre (mois relatif a janvier)
true_sigma  <- 2       # etendue de la fenetre (largeur du noyau gaussien)
true_slope  <- 0.3     # amplitude de l'effet thermique a detecter

true_w <- dnorm(MONTH_OFFSETS, true_mu, true_sigma)
true_w <- true_w / sum(true_w)                     # normalisee, somme = 1

true_intercept_cohort   <- 0
true_sigma_cohort_resid <- rep_null$sigma_cohort    # meme bruit residuel que le reel

# --- Construction de la verite au niveau cohorte ---
set.seed(1)   # reproductibilite de CE scenario ; variera par replicat dans la boucle

true_thermal_index <- as.vector(temp_extended %*% true_w)
true_b_resid       <- rnorm(n_cohort, 0, true_sigma_cohort_resid)

true_cohort_effect <- true_intercept_cohort +
                      true_slope * true_thermal_index +
                      true_b_resid

# --- Predicteur lineaire simule : socle GAM REEL + effet cohorte SIMULE ---
beta_null <- opt_null$par[names(opt_null$par) == "beta_fixed"]

eta_sim <- as.vector(X_fixed_no_intercept %*% beta_null) +
           true_cohort_effect[cohort_id_per_obs]

# --- Tirage binomial : reproduit le bruit d'observation individuel reel ---
prob_sim <- plogis(eta_sim)
y_sim    <- rbinom(length(prob_sim), size = 1, prob = prob_sim)

cat("Proportion simulee de males :", mean(y_sim), "\n")
cat("Proportion reelle de males  :", mean(data_sp$Numeric_sex), "\n")

# ----------------------------------------------------------------
# 13. BOUCLE : 4 formes de fenetre thermique candidates
# ----------------------------------------------------------------

data_full_real <- list(
  y             = data_sp$Numeric_sex,
  X_fixed       = X_fixed_no_intercept,
  cohort_id     = cohort_id_per_obs,
  temp_extended = temp_extended,
  penalty_list  = penalty_list_adjusted,
  lambda_fixed  = lambda_bam_ref_aligned,
  month_offsets = MONTH_OFFSETS
)

beta_null <- opt_null$par[names(opt_null$par) == "beta_fixed"]

build_parameters <- function(shape) {
  base <- list(
    beta_fixed             = beta_null,
    intercept_cohort       = 0,
    slope_cohort            = 0.3,
    b_cohort_resid          = rep_null$b_cohort,
    log_sigma_cohort_resid = log(rep_null$sigma_cohort)
  )
  shape_params <- switch(shape,
    "gaussian"    = list(mu = mean(MONTH_OFFSETS), log_sigma = log(3)),
    "uniform"     = list(),
    "linear"      = list(slope_w = 0),
    "skewnormal"  = list(xi = mean(MONTH_OFFSETS), log_omega = log(3), alpha = 0)
  )
  c(base, shape_params)
}

make_f_full_shape <- function(data_full_w, shape) {
  function(parms) {
    getAll(parms, data_full_w)

    sigma_cohort_resid <- exp(log_sigma_cohort_resid)
    lambda              <- lambda_fixed
    N_MONTHS_local       <- length(month_offsets)

    if (shape == "gaussian") {
      sigma_w <- exp(log_sigma)
      w_raw   <- dnorm(month_offsets, mu, sigma_w)
      nll_prior_shape <- -dnorm(log_sigma, mean = log(3), sd = 1.5, log = TRUE)

    } else if (shape == "uniform") {
      w_raw   <- rep(1, N_MONTHS_local)
      nll_prior_shape <- 0

    } else if (shape == "linear") {
      offset_ctr <- month_offsets - mean(month_offsets)
      w_raw <- exp(slope_w * offset_ctr)
      nll_prior_shape <- -dnorm(slope_w, mean = 0, sd = 0.5, log = TRUE)

    } else if (shape == "skewnormal") {
      omega <- exp(log_omega)
      z     <- (month_offsets - xi) / omega
      w_raw <- (2 / omega) * dnorm(z) * pnorm(alpha * z)
      nll_prior_shape <- -dnorm(log_omega, mean = log(3), sd = 1.5, log = TRUE) +
                          (-dnorm(alpha, mean = 0, sd = 3, log = TRUE))
    }

    w_month <- w_raw / sum(w_raw)
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

    nll <- nll_obs + nll_penalty + nll_resid_cohort + nll_prior_shape

    REPORT(w_month)
    REPORT(slope_cohort)
    REPORT(sigma_cohort_resid)
    REPORT(thermal_index)
    ADREPORT(w_month)
    ADREPORT(slope_cohort)
    if (shape == "gaussian")   { REPORT(mu); REPORT(sigma_w); ADREPORT(mu); ADREPORT(sigma_w) }
    if (shape == "linear")     { REPORT(slope_w); ADREPORT(slope_w) }
    if (shape == "skewnormal") { REPORT(xi); REPORT(omega); REPORT(alpha)
                                 ADREPORT(xi); ADREPORT(omega); ADREPORT(alpha) }

    nll
  }
}

# --- Boucle sur les 4 formes ---
shapes <- c("gaussian", "uniform", "linear", "skewnormal")
resultats_par_forme <- list()

for (shape in shapes) {

  cat("\n=== Forme :", shape, "===\n")

  set.seed(42)
  parameters_shape <- build_parameters(shape)
  f_shape <- make_f_full_shape(data_full_real, shape)

  obj_shape <- RTMB::MakeADFun(
    f_shape, parameters_shape,
    random = "b_cohort_resid",
    silent = TRUE
  )

  opt_shape <- tryCatch(
    nlminb(obj_shape$par, obj_shape$fn, obj_shape$gr,
           control = list(trace = 1, iter.max = 3000, eval.max = 6000)),
    error = function(e) { cat("  ECHEC optimisation :", conditionMessage(e), "\n"); NULL }
  )

  if (is.null(opt_shape)) {
    resultats_par_forme[[shape]] <- list(shape = shape, echec = TRUE)
    next
  }

  rep_shape <- obj_shape$report(obj_shape$env$last.par.best)
  sd_shape  <- tryCatch(sdreport(obj_shape), error = function(e) NULL)

  k_shape   <- length(opt_shape$par)
  AIC_shape <- 2 * opt_shape$objective + 2 * k_shape

  z_slope <- NA; p_slope <- NA; se_slope <- NA
  if (!is.null(sd_shape) && isTRUE(sd_shape$pdHess)) {
    tab_shape <- summary(sd_shape, select = "report")
    se_slope <- tab_shape["slope_cohort", "Std. Error"]
    z_slope  <- rep_shape$slope_cohort / se_slope
    p_slope  <- 2 * pnorm(-abs(z_slope))
  }

  resultats_par_forme[[shape]] <- list(
    shape             = shape,
    convergence       = opt_shape$convergence,
    message           = opt_shape$message,
    pdHess            = if (!is.null(sd_shape)) sd_shape$pdHess else NA,
    k                 = k_shape,
    objective         = opt_shape$objective,
    AIC               = AIC_shape,
    slope_est         = rep_shape$slope_cohort,
    se_slope          = se_slope,
    z                 = z_slope,
    p                 = p_slope,
    w_month           = rep_shape$w_month,
    sd_report_summary = if (!is.null(sd_shape) && isTRUE(sd_shape$pdHess))
                         as.data.frame(summary(sd_shape, select = "report")) else NULL,
    echec        = FALSE
  )

  cat("  convergence :", opt_shape$convergence, "| pdHess :",
      if (!is.null(sd_shape)) sd_shape$pdHess else NA, "\n")
  cat("  slope :", round(rep_shape$slope_cohort, 4),
      "| z :", round(z_slope, 3), "| p :", round(p_slope, 4),
      "| AIC :", round(AIC_shape, 2), "\n")

  # sauvegarde incrementale, forme par forme
  saveRDS(resultats_par_forme,
          file.path(CACHE_DIR, paste0("resultats_par_forme_", model_sig, ".rds")))
  
  # --- LIBERATION MEMOIRE, avant de passer a la forme suivante ---
  rm(obj_shape, opt_shape, rep_shape, sd_shape, parameters_shape, f_shape)
  gc(verbose = FALSE)
  cat("  [memoire liberee]\n")
}

# ----------------------------------------------------------------
# 14. Tableau comparatif des 4 formes
# ----------------------------------------------------------------

k_null <- length(opt_null$par)
AIC_null <- 2 * opt_null$objective + 2 * k_null

tab_comparatif <- do.call(rbind, lapply(resultats_par_forme, function(r) {
  if (isTRUE(r$echec)) {
    data.frame(shape = r$shape, convergence = NA, pdHess = NA, k = NA,
               AIC = NA, delta_AIC_vs_null = NA, slope = NA, z = NA, p = NA)
  } else {
    data.frame(shape = r$shape, convergence = r$convergence, pdHess = r$pdHess,
               k = r$k, AIC = round(r$AIC, 2),
               delta_AIC_vs_null = round(AIC_null - r$AIC, 2),
               slope = round(r$slope_est, 4), z = round(r$z, 3), p = round(r$p, 4))
  }
}))
rownames(tab_comparatif) <- NULL

cat("\n=== TABLEAU COMPARATIF DES 4 FORMES ===\n")
cat("AIC modele nul :", round(AIC_null, 2), "\n")
print(tab_comparatif)

saveRDS(tab_comparatif, file.path(CACHE_DIR, paste0("tab_comparatif_formes_", model_sig, ".rds")))

# ----------------------------------------------------------------
# 15. Visualisation comparative des 4 fenetres
# ----------------------------------------------------------------

par(mfrow = c(2, 2))
for (shape in shapes) {
  r <- resultats_par_forme[[shape]]
  if (isTRUE(r$echec)) { plot.new(); title(paste(shape, "- ECHEC")); next }
  plot(MONTH_OFFSETS, r$w_month, type = "b", pch = 19, col = "red",
       xlab = "Offset mensuel", ylab = "Poids",
       main = paste0(shape, " (p=", round(r$p, 3), ")"))
  abline(h = 1/N_MONTHS, lty = 3, col = "grey50")
}
par(mfrow = c(1, 1))

# ----------------------------------------------------------------
# 16. Sauvegarde finale — synthese de la boucle des 4 formes
# ----------------------------------------------------------------

resultats_finaux <- list(
  species             = SPECIES_NAME,
  MONTH_OFFSETS       = MONTH_OFFSETS,
  N_MONTHS            = N_MONTHS,
  AIC_null            = AIC_null,
  tab_comparatif      = tab_comparatif,
  resultats_par_forme = resultats_par_forme
)

saveRDS(resultats_finaux, file.path(CACHE_DIR, paste0("resultats_finaux_toutes_formes_", model_sig, ".rds")))
cat("\n=== Resultats finaux (4 formes) sauvegardes dans cache/resultats_finaux_toutes_formes_", model_sig, ".rds ===\n")