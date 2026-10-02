# ================================================================
# PIPELINE COMPLET v4 — fonctions appelables par methode / espece(s)
# ================================================================

library(dplyr)
library(mgcv)
library(lubridate)
library(RTMB)
library(Matrix)

# ================================================================
# 1. prepare_temp_series : construit temp_surface_mensuelle une seule
#    fois (independant de l'espece et de la methode)
# ================================================================

prepare_temp_series <- function(df_long, K_HARMONICS = 5) {

  temp_surface_mensuelle <- df_long %>%
    dplyr::group_by(time) %>%
    dplyr::summarise(temp_moy_bassin = mean(temperature, na.rm = TRUE)) %>%
    dplyr::rename(date = time) %>%
    dplyr::arrange(date)

  temp_surface_mensuelle <- temp_surface_mensuelle %>%
    dplyr::mutate(
      mois           = as.numeric(format(date, "%m")),
      annee          = as.numeric(format(date, "%Y")),
      annee_continue = annee + (mois - 1) / 12
    )

  harm_terms <- unlist(lapply(seq_len(K_HARMONICS), function(k) {
    c(sprintf("sin(2*pi*%d*annee_continue)", k),
      sprintf("cos(2*pi*%d*annee_continue)", k))
  }))

  formule_decomp <- as.formula(
    paste("temp_moy_bassin ~ annee_continue +", paste(harm_terms, collapse = " + "))
  )

  lm_decomp <- lm(formule_decomp, data = temp_surface_mensuelle)
  coefs <- coef(lm_decomp)

  temp_surface_mensuelle <- temp_surface_mensuelle %>%
    dplyr::mutate(
      trend_component   = coefs["(Intercept)"] + coefs["annee_continue"] * annee_continue,
      temp_anomaly_harm  = residuals(lm_decomp)
    )

  cat("R2 detrending harmonique (K =", K_HARMONICS, ") :", summary(lm_decomp)$r.squared, "\n")
  cat("SD anomalie harmonique :", sd(temp_surface_mensuelle$temp_anomaly_harm), "\n")

  temp_surface_mensuelle
}

# ================================================================
# Helpers
# ================================================================

build_parameters <- function(method, shape, MONTH_OFFSETS, beta_null, rep_null) {
  base <- list(
    beta_fixed = beta_null,
    b_cohort_resid = rep_null$b_cohort, log_sigma_cohort_resid = log(rep_null$sigma_cohort)
  )
  method_params <- if (method == "detrend") {
    list(beta_trend = 0, beta_sd_abs = 0, beta_anom_mean = 0, beta_anom_sd = 0)
  } else {
    list(beta_mean = 0, beta_sd = 0)
  }
  shape_params <- switch(shape,
    "gaussian"   = list(mu = mean(MONTH_OFFSETS), log_sigma = log(3)),
    "uniform"    = list(),
    "linear"     = list(slope_w = 0),
    "skewnormal" = list(xi = mean(MONTH_OFFSETS), log_omega = log(3), alpha = 0)
  )
  c(base, method_params, shape_params)
}

make_f_full <- function(data_full_w, method, shape) {
  function(parms) {
    getAll(parms, data_full_w)
    sigma_cohort_resid <- exp(log_sigma_cohort_resid)
    lambda <- lambda_fixed
    N_MONTHS_local <- length(month_offsets)

    if (shape == "gaussian") {
      sigma_w <- exp(log_sigma)
      w_raw   <- dnorm(month_offsets, mu, sigma_w)
      nll_prior_shape <- -dnorm(log_sigma, mean = log(3), sd = 1.5, log = TRUE)
    } else if (shape == "uniform") {
      w_raw <- rep(1, N_MONTHS_local)
      nll_prior_shape <- 0
    } else if (shape == "linear") {
      offset_ctr <- month_offsets - mean(month_offsets)
      w_raw <- exp(slope_w * offset_ctr)
      nll_prior_shape <- -dnorm(slope_w, mean = 0, sd = 0.5, log = TRUE)
    } else if (shape == "skewnormal") {
      omega <- exp(log_omega)
      z <- (month_offsets - xi) / omega
      w_raw <- (2 / omega) * dnorm(z) * pnorm(alpha * z)
      nll_prior_shape <- -dnorm(log_omega, mean = log(3), sd = 1.5, log = TRUE) +
                          (-dnorm(alpha, mean = 0, sd = 3, log = TRUE))
    }

    w_month <- w_raw / sum(w_raw)

    wmean <- function(mat) as.vector(mat %*% w_month)
    wsd   <- function(mat, m) sqrt(as.vector(((mat - m)^2) %*% w_month))

    if (method == "detrend") {
      Trend_mean_w   <- wmean(temp_extended_trend)
      T_sd_abs_w     <- wsd(temp_extended_raw, wmean(temp_extended_raw))
      Anomaly_mean_w <- wmean(temp_extended_anomaly)
      Anomaly_sd_w   <- wsd(temp_extended_anomaly, Anomaly_mean_w)

      cohort_pred_thermal <- beta_trend * Trend_mean_w + beta_sd_abs * T_sd_abs_w +
        beta_anom_mean * Anomaly_mean_w + beta_anom_sd * Anomaly_sd_w

    } else {
      T_mean_w <- wmean(temp_extended_raw)
      T_sd_w   <- wsd(temp_extended_raw, T_mean_w)

      cohort_pred_thermal <- beta_mean * T_mean_w + beta_sd * T_sd_w
    }

    cohort_effect_total <- cohort_pred_thermal + b_cohort_resid
    eta <- as.vector(X_fixed %*% beta_fixed) + cohort_effect_total[cohort_id]

    p_hat <- 1 / (1 + exp(-eta))
    REPORT(eta)
    REPORT(p_hat)
    REPORT(cohort_pred_thermal)
    REPORT(cohort_effect_total)

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

    REPORT(w_month); REPORT(sigma_cohort_resid)
    ADREPORT(w_month)

    if (method == "detrend") {
      REPORT(Trend_mean_w); REPORT(T_sd_abs_w); REPORT(Anomaly_mean_w); REPORT(Anomaly_sd_w)
      REPORT(beta_trend); REPORT(beta_sd_abs); REPORT(beta_anom_mean); REPORT(beta_anom_sd)
      ADREPORT(beta_trend); ADREPORT(beta_sd_abs); ADREPORT(beta_anom_mean); ADREPORT(beta_anom_sd)
    } else {
      REPORT(T_mean_w); REPORT(T_sd_w)
      REPORT(beta_mean); REPORT(beta_sd)
      ADREPORT(beta_mean); ADREPORT(beta_sd)
    }

    if (shape == "gaussian")   { REPORT(mu); REPORT(sigma_w); ADREPORT(mu); ADREPORT(sigma_w) }
    if (shape == "linear")     { REPORT(slope_w); ADREPORT(slope_w) }
    if (shape == "skewnormal") { REPORT(xi); REPORT(omega); REPORT(alpha)
                                  ADREPORT(xi); ADREPORT(omega); ADREPORT(alpha) }
    nll
  }
}

# ================================================================
# 2. run_species_thermal : traite UNE espece, UNE methode
#    ("detrend" ou "raw"), boucle sur les 4 formes de fenetre.
#    Arborescence de cache :
#    results_rtmb/{detrend|raw}/{bam_null_models,rtmb_null_models,
#    rtmb_models}/{espece}/
# ================================================================

run_species_thermal <- function(sp,
                                 data_expanded,
                                 temp_surface_mensuelle,
                                 method = c("detrend", "raw"),
                                 MONTH_OFFSETS = 0:11,
                                 BASE_DIR = "results_rtmb",
                                 FORCE_REFIT_BAM_REF = FALSE,
                                 FORCE_REFIT_NULL    = FALSE) {

  method <- match.arg(method)
  N_MONTHS <- length(MONTH_OFFSETS)

  # ---- Hyperparametres verrouilles par espece (issus des tests k.check
  #      repetes, voir synthese de la session de calibration) ----
  K_TENSOR_BY_SPECIES <- list(
    "Trisopterus esmarkii"      = c(7, 8, 8),
    "Pleuronectes platessa"     = c(6, 6, 8),
    "Clupea harengus"           = c(6, 6, 8),
    "Merlangius merlangus"      = c(6, 6, 8),
    "Melanogrammus aeglefinus"  = c(6, 6, 8)

  )
  k_tensor_default <- c(8, 8, 8)

  K_SPACE_BY_SPECIES <- list(
    "Pollachius virens"     = 120,
    "Pleuronectes platessa" = 240,
    "Trisopterus esmarkii"  = 240,
    "Sprattus sprattus"     = 240
  )
  k_space_default <- 120

  k_time_default <- 12   # uniforme pour toutes les especes

  terms_removed <- list("Pollachius virens" = c("f4", "bc"))

  # ---- Arborescence de cache : pointe vers les modeles deja calcules ----
  bam_ref_dir <- file.path(BASE_DIR, method, "bam_null_models", sp)
  rtmb_null_dir   <- file.path(BASE_DIR, method, "rtmb_null_models", sp)
  rtmb_models_dir <- file.path(BASE_DIR, method, "rtmb_models", sp)

  for (d in c(bam_ref_dir, rtmb_null_dir, rtmb_models_dir)) {
    if (!dir.exists(d)) dir.create(d, recursive = TRUE)
  }

  cat("\n\n########## ESPECE :", sp, "| METHODE :", method, "##########\n")

  # ---- Bloc 3 : donnees espece ----
  data_sp <- data_expanded %>%
    dplyr::filter(Species == sp, !is.na(Numeric_sex)) %>%
    droplevels()

  cat("N =", nrow(data_sp), "\n")

  removed_sp <- terms_removed[[sp]]

  k_tensor <- if (!is.null(K_TENSOR_BY_SPECIES[[sp]])) K_TENSOR_BY_SPECIES[[sp]] else k_tensor_default
  k_age    <- k_tensor[1]
  k_lngt   <- k_tensor[2]
  k_fs     <- k_tensor[3]   # k_cohort, nomme k_fs pour coherence avec le reste du pipeline

  k_space <- if (!is.null(K_SPACE_BY_SPECIES[[sp]])) K_SPACE_BY_SPECIES[[sp]] else k_space_default
  k_time  <- k_time_default

  # ---- Bloc 4 : formule BAM (tenseur 3D) ----
  build_formula_sp <- function(k_age, k_lngt, k_space, k_time, k_cohort, removed) {
    terms <- list()
    terms[["te3d"]] <- bquote(
      te(Age_sc, LngtClassGrouped_sc, Cohort_num_sc,
         bs = c("cr", "cr", "cs"),
         k  = c(.(k_age), .(k_lngt), .(k_cohort)))
    )
    terms[["f3_space"]] <- bquote(s(Latitude, Longitude, k = .(k_space), bs = "sos"))
    if (!"f4" %in% removed) terms[["f4"]] <- bquote(s(julian_day, bs = "cc", k = .(k_time)))
    if (!"bc" %in% removed) terms[["bc"]] <- quote(s(Cohort_fact, bs = "re"))
    rhs <- Reduce(function(a, b) call("+", a, b), terms)
    as.formula(bquote(Numeric_sex ~ .(rhs)))
  }

  formula_sp <- build_formula_sp(k_age, k_lngt, k_space, k_time, k_fs, removed_sp)
  print(formula_sp)

  # ---- Bloc 5 : bam_ref (cache dans bam_null_models_te3d, format cs3) ----
  model_sig_bam <- paste(sp, k_age, k_lngt, k_space, k_time, k_fs, "cs3", nrow(data_sp), sep = "_")
  bam_ref_cache_file <- file.path(bam_ref_dir, paste0("bam_ref_", model_sig_bam, ".rds"))

  if (!FORCE_REFIT_BAM_REF && file.exists(bam_ref_cache_file)) {
    cat("bam_ref charge depuis le cache :", bam_ref_cache_file, "\n")
    bam_ref <- readRDS(bam_ref_cache_file)
  } else {
    bam_ref <- bam(formula_sp, family = binomial(link = "logit"), data = data_sp,
                    method = "ML", discrete = FALSE, keepData = TRUE)
    saveRDS(bam_ref, bam_ref_cache_file)
    cat("bam_ref sauvegarde dans :", bam_ref_cache_file, "\n")
  }

  lambda_bam_ref <- bam_ref$sp

  # ---- Bloc 6 : structure BAM (matrices beta + penalites) ----
  extract_bam_structure <- function(data_sp, formula_sp, bam_ref) {
    bam_setup <- mgcv::gam(formula_sp, family = binomial(link = "logit"), data = data_sp, fit = FALSE)
    smooth_info <- bam_setup$smooth
    re_idx <- which(sapply(smooth_info, function(s) s$term[1] == "Cohort_fact" &&
                              inherits(s, "random.effect")))
    cols_re <- if (length(re_idx) > 0) {
      smooth_info[[re_idx]]$first.para:smooth_info[[re_idx]]$last.para
    } else integer(0)
    cols_fixed <- setdiff(seq_len(ncol(bam_setup$X)), cols_re)

    Xp_bam <- predict(bam_ref, type = "lpmatrix")
    X_fixed <- Xp_bam[, cols_fixed, drop = FALSE]

    penalty_list <- list()
    for (i in seq_along(smooth_info)) {
      if (i %in% re_idx) next
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
    list(X_fixed = X_fixed, penalty_list = penalty_list, na_action = bam_setup$na.action)
  }

  bam_struct_full <- extract_bam_structure(data_sp, formula_sp, bam_ref)

  if (!is.null(bam_struct_full$na_action)) {
    cat("ATTENTION :", length(bam_struct_full$na_action), "lignes supprimees par na.omit.\n")
    data_sp <- data_sp[-bam_struct_full$na_action, , drop = FALSE]
    data_sp$Cohort_fact <- droplevels(data_sp$Cohort_fact)
  }

  stopifnot(nrow(bam_struct_full$X_fixed) == nrow(data_sp))

  cohort_levels     <- levels(data_sp$Cohort_fact)
  cohort_id_per_obs <- as.integer(data_sp$Cohort_fact)
  n_cohort          <- length(cohort_levels)

  # ---- Extraction : X_fixed_full et penalty_list_full, intercept inclus ----
  X_fixed_full      <- bam_struct_full$X_fixed
  penalty_list_full <- bam_struct_full$penalty_list

  # ---- Bloc 8 : alignement lambda ----
  LAMBDA_NAMES_STANDARD <- c(
    "te(Age_sc,LngtClassGrouped_sc,Cohort_num_sc)1",
    "te(Age_sc,LngtClassGrouped_sc,Cohort_num_sc)2",
    "te(Age_sc,LngtClassGrouped_sc,Cohort_num_sc)3",
    "s(Latitude,Longitude)", "s(julian_day)"
  )
  LAMBDA_NAMES_NO_F4 <- c(
    "te(Age_sc,LngtClassGrouped_sc,Cohort_num_sc)1",
    "te(Age_sc,LngtClassGrouped_sc,Cohort_num_sc)2",
    "te(Age_sc,LngtClassGrouped_sc,Cohort_num_sc)3",
    "s(Latitude,Longitude)"
  )
  lambda_names_sp <- if (!is.null(removed_sp) && "f4" %in% removed_sp) {
    LAMBDA_NAMES_NO_F4
  } else {
    LAMBDA_NAMES_STANDARD
  }

  lambda_bam_ref_aligned <- as.numeric(lambda_bam_ref[lambda_names_sp])
  stopifnot(!anyNA(lambda_bam_ref_aligned))
  stopifnot(length(lambda_bam_ref_aligned) == length(penalty_list_full))

  # ---- Bloc 9 : covariables thermiques ----
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

  temp_extended_raw <- build_temp_extended(
    monthly_series = temp_surface_mensuelle %>% dplyr::select(date, temp_moy_bassin),
    cohort_years = cohort_years_sp, month_offsets = MONTH_OFFSETS
  )
  temp_extended_anomaly <- build_temp_extended(
    monthly_series = temp_surface_mensuelle %>%
      dplyr::select(date, temp_anomaly_harm) %>%
      dplyr::rename(temp_moy_bassin = temp_anomaly_harm),
    cohort_years = cohort_years_sp, month_offsets = MONTH_OFFSETS
  )
  temp_extended_trend <- build_temp_extended(
    monthly_series = temp_surface_mensuelle %>%
      dplyr::select(date, trend_component) %>%
      dplyr::rename(temp_moy_bassin = trend_component),
    cohort_years = cohort_years_sp, month_offsets = MONTH_OFFSETS
  )

  for (nm in c("temp_extended_raw", "temp_extended_anomaly", "temp_extended_trend")) {
    m <- get(nm)
    n_na <- sum(is.na(m))
    if (n_na > 0) {
      na_cohorts <- cohort_years_sp[apply(m, 1, anyNA)]
      stop(sprintf("%s contient %d NA. Cohortes concernees : %s.",
                    nm, n_na, paste(na_cohorts, collapse = ", ")))
    }
  }

  model_sig_rtmb <- paste(model_sig_bam, paste(range(MONTH_OFFSETS), collapse = "-"), sep = "_")

  # ---- Bloc 10 : modele nul (cache dans rtmb_null_models) ----
  data_null <- list(
    y = data_sp$Numeric_sex, X_fixed = X_fixed_full,
    cohort_id = cohort_id_per_obs, penalty_list = penalty_list_full,
    lambda_fixed = lambda_bam_ref_aligned
  )

  beta_fixed_init <- as.numeric(coef(bam_ref))[seq_len(ncol(X_fixed_full))]

  has_bc_term <- is.null(removed_sp) || !("bc" %in% removed_sp)

  if (has_bc_term) {

    b_cohort_init <- as.numeric(coef(bam_ref)[grep("^s\\(Cohort_fact\\)", names(coef(bam_ref)))])
    stopifnot(length(b_cohort_init) == n_cohort)

    # eta_reconstruit_bam <- as.vector(X_fixed_full %*% beta_fixed_init) + b_cohort_init[cohort_id_per_obs]
    # eta_bam_reference    <- bam_ref$linear.predictors

    # max_diff_b_cohort <- max(abs(eta_reconstruit_bam - eta_bam_reference))
    # cat("\n---- Verification b_cohort_init (ordre vs cohort_id_per_obs) ----\n")
    # cat("Ecart max |eta reconstruit (bam) - bam_ref$linear.predictors| :", max_diff_b_cohort, "\n")

    # if (max_diff_b_cohort > 1e-8) {
    #   stop(sprintf("DESACCORD CRITIQUE : b_cohort_init n'est pas dans le bon ordre par rapport a cohort_id_per_obs (ecart %.8f).", max_diff_b_cohort))
    # }
    # cat("---- b_cohort_init confirme aligne avec cohort_id_per_obs ----\n\n")

    log_sigma_cohort_init <- log(sd(b_cohort_init))

  } else {

    cat("\n---- Pas de terme s(Cohort_fact, bs='re') dans bam_ref (espece :", sp, ") ----\n")
    cat("b_cohort initialise a zero, pas de verification d'alignement possible.\n\n")

    b_cohort_init <- rep(0, n_cohort)
    log_sigma_cohort_init <- 0

  }

  parameters_null <- list(
    beta_fixed = beta_fixed_init,
    b_cohort = b_cohort_init,
    log_sigma_cohort = log_sigma_cohort_init
  )

  make_f_null_iid <- function(data_null) {
    function(parms) {
      getAll(parms, data_null)
      sigma_cohort <- exp(log_sigma_cohort)
      eta <- as.vector(X_fixed %*% beta_fixed) + b_cohort[cohort_id]

      # ---- Ajout : probabilites predites individuelles, pour DHARMa/residus ----
      p_hat <- 1 / (1 + exp(-eta))

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

      REPORT(eta); REPORT(sigma_cohort); REPORT(b_cohort)
      REPORT(p_hat)

      nll
    }
  }

  obj_null <- RTMB::MakeADFun(make_f_null_iid(data_null), parameters_null,
                               random = "b_cohort", silent = TRUE)

  null_model_cache_file <- file.path(rtmb_null_dir, paste0("null_model_", model_sig_rtmb, ".rds"))

  if (!FORCE_REFIT_NULL && file.exists(null_model_cache_file)) {
    cat("Modele nul charge depuis le cache :", null_model_cache_file, "\n")
    null_cache <- readRDS(null_model_cache_file)
    opt_null   <- null_cache$opt_null
    rep_null   <- null_cache$rep_null
  } else {
    opt_null_1 <- nlminb(obj_null$par, obj_null$fn, obj_null$gr,
                          control = list(trace = 10, iter.max = 5000, eval.max = 10000,
                                         rel.tol = 1e-12, x.tol = 1e-10))
    opt_null <- nlminb(opt_null_1$par, obj_null$fn, obj_null$gr,
                        control = list(trace = 10, iter.max = 5000, eval.max = 10000,
                                       rel.tol = 1e-12, x.tol = 1e-10))
    rep_null <- obj_null$report(obj_null$env$last.par.best)
    saveRDS(list(
      opt_null = opt_null, rep_null = rep_null,
      parameters_init = parameters_null,
      data_null_used = data_null,
      data_sp_used = data_sp,
      last_par_best = obj_null$env$last.par.best
    ), null_model_cache_file)
    cat("Modele nul sauvegarde dans :", null_model_cache_file, "\n")
  }
  cat("Convergence modele nul :", opt_null$convergence, "\n")

  # # ---- Bloc 11 : validation RTMB vs bam_ref ----
  # eta_bam_ref   <- bam_ref$linear.predictors
  # eta_rtmb_null <- rep_null$eta
  # cor_eta  <- cor(eta_bam_ref, eta_rtmb_null)
  # cat("Cor(eta_bam_ref, eta_rtmb) :", cor_eta, "\n")
  # if (cor_eta < 0.99) {
  #   warning(paste0(sp, " (", method, ") : cor RTMB/bam_ref = ", round(cor_eta, 4),
  #                   " < 0.99, diagnostic requis avant interpretation."))
  # }

  # ---- Bloc 13 : boucle sur les 4 formes (cache dans rtmb_models) ----
  data_full_real <- list(
    y = data_sp$Numeric_sex, X_fixed = X_fixed_full, cohort_id = cohort_id_per_obs,
    temp_extended_raw = temp_extended_raw, temp_extended_anomaly = temp_extended_anomaly,
    temp_extended_trend = temp_extended_trend,
    penalty_list = penalty_list_full, lambda_fixed = lambda_bam_ref_aligned,
    month_offsets = MONTH_OFFSETS
  )

  beta_null <- opt_null$par[names(opt_null$par) == "beta_fixed"]

  shapes <- c("gaussian", "uniform", "linear", "skewnormal")
  resultats_par_forme <- list()

  for (shape in shapes) {

    # ---- Verification de reprise : forme deja calculee et sauvegardee ? ----
    shape_file <- file.path(rtmb_models_dir, paste0("resultat_", shape, "_", model_sig_rtmb, ".rds"))

    if (!FORCE_REFIT_NULL && file.exists(shape_file)) {
      cat("\n=== Forme :", shape, "(deja en cache, chargee depuis", shape_file, ") ===\n")
      resultats_par_forme[[shape]] <- readRDS(shape_file)

      r <- resultats_par_forme[[shape]]
      if (!isTRUE(r$echec)) {
        cat("  convergence :", r$convergence, "|", r$coef_interet, ":", round(r$slope_est, 4),
            "| p :", round(r$p, 4), "| AIC :", round(r$AIC, 2), "\n")
      } else {
        cat("  (forme marquee comme echec dans le cache)\n")
      }

      next
    }

    cat("\n=== Forme :", shape, "===\n")

    set.seed(42)
    parameters_shape <- build_parameters(method, shape, MONTH_OFFSETS, beta_null, rep_null)
    f_shape <- make_f_full(data_full_real, method, shape)

    obj_shape <- RTMB::MakeADFun(f_shape, parameters_shape, random = "b_cohort_resid", silent = TRUE)

    opt_shape <- tryCatch(
      nlminb(obj_shape$par, obj_shape$fn, obj_shape$gr,
             control = list(trace = 10, iter.max = 5000, eval.max = 10000)),
      error = function(e) { cat("  ECHEC :", conditionMessage(e), "\n"); NULL }
    )

    if (is.null(opt_shape)) {
      resultats_par_forme[[shape]] <- list(shape = shape, echec = TRUE)
      saveRDS(resultats_par_forme[[shape]], shape_file)
      next
    }

    rep_shape <- obj_shape$report(obj_shape$env$last.par.best)

    cat("  -- Diagnostic post-optimisation --\n")
    cat("  convergence (nlminb) :", opt_shape$convergence, "\n")
    cat("  message (nlminb)     :", opt_shape$message, "\n")

    grad_final <- tryCatch(obj_shape$gr(opt_shape$par), error = function(e) { cat("  ECHEC calcul gradient :", conditionMessage(e), "\n"); NA })
    cat("  norme max du gradient final :", max(abs(grad_final)), "\n")

    # sd_shape  <- tryCatch(sdreport(obj_shape), error = function(e) { cat("  ECHEC sdreport :", conditionMessage(e), "\n"); NULL })
    # cat("  sdreport pdHess :", if (!is.null(sd_shape)) sd_shape$pdHess else "sd_shape est NULL", "\n")

    # k_shape   <- length(opt_shape$par)
    # AIC_shape <- 2 * opt_shape$objective + 2 * k_shape

    # coef_interet <- if (method == "detrend") "beta_anom_mean" else "beta_mean"
    # est_interet <- if (method == "detrend") rep_shape$beta_anom_mean else rep_shape$beta_mean

    # z_interet <- NA; p_interet <- NA; se_interet <- NA
    # if (!is.null(sd_shape) && isTRUE(sd_shape$pdHess)) {
    #   tab_shape <- summary(sd_shape, select = "report")
    #   se_interet <- tab_shape[coef_interet, "Std. Error"]
    #   z_interet  <- est_interet / se_interet
    #   p_interet  <- 2 * pnorm(-abs(z_interet))
    # }

    # ---- sdreport desactive pour cette etape (AIC uniquement) ----
    sd_shape <- NULL
    cat("  sdreport : desactive pour cette etape (AIC seulement)\n")

    k_shape   <- length(opt_shape$par)
    AIC_shape <- 2 * opt_shape$objective + 2 * k_shape

    coef_interet <- if (method == "detrend") "beta_anom_mean" else "beta_mean"
    est_interet <- if (method == "detrend") rep_shape$beta_anom_mean else rep_shape$beta_mean

    z_interet <- NA; p_interet <- NA; se_interet <- NA

    resultats_par_forme[[shape]] <- list(
      shape = shape, convergence = opt_shape$convergence, message = opt_shape$message,
      pdHess = if (!is.null(sd_shape)) sd_shape$pdHess else NA,
      k = k_shape, objective = opt_shape$objective, AIC = AIC_shape,
      coef_interet = coef_interet,
      slope_est = est_interet, se_slope = se_interet, z = z_interet, p = p_interet,
      w_month = rep_shape$w_month,
      beta_trend   = if (method == "detrend") rep_shape$beta_trend else NA,
      beta_sd_abs  = if (method == "detrend") rep_shape$beta_sd_abs else NA,
      beta_anom_mean = if (method == "detrend") rep_shape$beta_anom_mean else NA,
      beta_anom_sd   = if (method == "detrend") rep_shape$beta_anom_sd else NA,
      beta_mean = if (method == "raw") rep_shape$beta_mean else NA,
      beta_sd   = if (method == "raw") rep_shape$beta_sd else NA,
      sd_report_summary = if (!is.null(sd_shape) && isTRUE(sd_shape$pdHess))
                           as.data.frame(summary(sd_shape, select = "report")) else NULL,

      # ---- Tout ce qu'il faut pour reconstruire obj_shape sans repartir de bam_ref ----
      opt_par_full       = opt_shape$par,                  # vecteur complet des parametres optimaux
      parameters_init    = parameters_shape,               # point de depart utilise
      rep_shape_full     = rep_shape,                       # tout le REPORT(), pas seulement l'extrait
      sd_shape_full       = sd_shape,                        # objet sdreport() complet (NULL si echec)
      last_par_best      = obj_shape$env$last.par.best,    # etat interne complet (fixe + random)
      data_full_used     = data_full_real,                    # les donnees passees a make_f_full pour cette forme
      method_used        = method,
      shape_used         = shape,

      data_sp_used = data_sp,

      echec = FALSE
    )

    cat("  convergence :", opt_shape$convergence, "|", coef_interet, ":", round(est_interet, 4),
        "| p :", round(p_interet, 4), "| AIC :", round(AIC_shape, 2), "\n")

    # ---- Sauvegarde individuelle de cette forme (mecanisme de reprise) ----
    saveRDS(resultats_par_forme[[shape]], shape_file)
    cat("  Forme", shape, "sauvegardee individuellement :", shape_file, "\n")

    # ---- Sauvegarde groupee, mise a jour au fur et a mesure (comme avant) ----
    saveRDS(resultats_par_forme,
            file.path(rtmb_models_dir, paste0("resultats_par_forme_", model_sig_rtmb, ".rds")))

    rm(obj_shape, opt_shape, rep_shape, sd_shape, parameters_shape, f_shape)
    gc(verbose = FALSE)
    cat("  [memoire liberee]\n")
  }

  # ---- Bloc 14 : tableau comparatif ----
  k_null <- length(opt_null$par)
  AIC_null <- 2 * opt_null$objective + 2 * k_null

  tab_comparatif <- do.call(rbind, lapply(resultats_par_forme, function(r) {
    if (isTRUE(r$echec)) {
      data.frame(shape = r$shape, convergence = NA, pdHess = NA, k = NA,
                 AIC = NA, delta_AIC_vs_null = NA, coef_interet = NA, slope = NA, z = NA, p = NA)
    } else {
      data.frame(shape = r$shape, convergence = r$convergence, pdHess = r$pdHess,
                 k = r$k, AIC = round(r$AIC, 2),
                 delta_AIC_vs_null = round(AIC_null - r$AIC, 2),
                 coef_interet = r$coef_interet,
                 slope = round(r$slope_est, 4), z = round(r$z, 3), p = round(r$p, 4))
    }
  }))
  rownames(tab_comparatif) <- NULL
  tab_comparatif <- tab_comparatif[order(tab_comparatif$AIC), ]

  cat("\n=== TABLEAU COMPARATIF (", sp, "-", method, ") ===\n")
  cat("AIC modele nul :", round(AIC_null, 2), "\n")
  print(tab_comparatif)

  saveRDS(tab_comparatif,
          file.path(rtmb_models_dir, paste0("tab_comparatif_", model_sig_rtmb, ".rds")))

  # ---- Bloc 15 : visualisation ----
  par(mfrow = c(2, 2))
  for (shape in shapes) {
    r <- resultats_par_forme[[shape]]
    if (isTRUE(r$echec)) { plot.new(); title(paste(shape, "- ECHEC")); next }
    plot(MONTH_OFFSETS, r$w_month, type = "b", pch = 19,
         col = if (method == "detrend") "red" else "blue",
         xlab = "Offset mensuel", ylab = "Poids",
         main = paste0(shape, " (p=", round(r$p, 3), ")"))
    abline(h = 1/N_MONTHS, lty = 3, col = "grey50")
  }
  par(mfrow = c(1, 1))

  # ---- Bloc 16 : sauvegarde finale ----
  resultats_finaux <- list(
    species = sp, method = method, MONTH_OFFSETS = MONTH_OFFSETS,
    AIC_null = AIC_null, tab_comparatif = tab_comparatif,
    resultats_par_forme = resultats_par_forme
  )

  saveRDS(resultats_finaux,
          file.path(rtmb_models_dir, paste0("resultats_finaux_", model_sig_rtmb, ".rds")))
  cat("\n=== Resultats finaux (", sp, "-", method, ") sauvegardes dans", rtmb_models_dir, "===\n")

  resultats_finaux
}

# ================================================================
# 3. run_thermal_pipeline : point d'entree unique
#    data_ind      : donnees individuelles (data_expanded_1991_2023)
#    data_temp_raw : temperature brute (df_long)
#    method        : "detrend" ou "raw"
#    species       : nom d'espece unique OU vecteur d'especes
# ================================================================

run_thermal_pipeline <- function(data_ind,
                                  data_temp_raw,
                                  method = c("detrend", "raw"),
                                  species,
                                  MONTH_OFFSETS = 0:11,
                                  BASE_DIR = "results_rtmb",
                                  ...) {

  method <- match.arg(method)
  stopifnot(is.character(species), length(species) >= 1)

  temp_surface_mensuelle <- prepare_temp_series(data_temp_raw)

  results_by_species <- list()

  for (sp in species) {
    results_by_species[[sp]] <- run_species_thermal(
      sp                      = sp,
      data_expanded           = data_ind,
      temp_surface_mensuelle  = temp_surface_mensuelle,
      method                  = method,
      MONTH_OFFSETS           = MONTH_OFFSETS,
      BASE_DIR                = BASE_DIR,
      ...
    )
  }

  results_by_species
}
