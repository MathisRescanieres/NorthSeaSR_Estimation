# ================================================================
# Reconstruction complete (structure identique a resultats_par_forme
# dans run_species_thermal) des modeles finaux de la selection
# backward LRT, pour les 4 especes, avec uniquement les covariables
# thermiques retenues. Jeu a 4 covariables (sans T_min/T_max).
# ================================================================

library(dplyr)
library(mgcv)
library(lubridate)
library(RTMB)
library(Matrix)
library(rprojroot)

proj_root <- find_root(has_file(".git") | is_rstudio_project)
setwd(file.path(proj_root, "SR_Estimation", "stat_analysis_2026", "data"))

source("../scripts/test_temp_v4.R")

BASE_OUT <- "results_rtmb/backward_selection_LRT"
method <- "detrend"
MONTH_OFFSETS <- 0:11

especes_cibles <- c("Clupea harengus", "Melanogrammus aeglefinus",
                    "Merlangius merlangus", "Trisopterus esmarkii")

resultats_complets <- list()

for (sp in especes_cibles) {

  cat("\n\n########## RECONSTRUCTION COMPLETE :", sp, "-", method, "##########\n")

  fichier_selection <- file.path(BASE_OUT, method, sp, "selection_finale_LRT.rds")
  r_selection <- readRDS(fichier_selection)
  modele_final <- r_selection$modele_final
  termes_retires <- r_selection$termes_retires

  cat("Termes retenus :", paste(r_selection$termes_retenus, collapse = ", "), "\n")
  cat("Termes retires  :", paste(termes_retires, collapse = ", "), "\n")

  null_dir <- file.path("results_rtmb", method, "rtmb_null_models", sp)
  fichiers_null <- list.files(null_dir, pattern = "^null_model_.*\\.rds$", full.names = TRUE)
  null_cache <- readRDS(fichiers_null[which.max(file.info(fichiers_null)$mtime)])
  beta_null <- null_cache$opt_null$par[names(null_cache$opt_null$par) == "beta_fixed"]
  rep_null  <- null_cache$rep_null

  data_full_real <- modele_final$data_full_used
  shape <- "uniform"

  parameters_shape <- build_parameters(method, shape, MONTH_OFFSETS, beta_null, rep_null)

  opt_par_named <- modele_final$opt_par_full
  for (nom in unique(names(opt_par_named))) {
    if (nom %in% names(parameters_shape) && !(nom %in% termes_retires)) {
      parameters_shape[[nom]] <- unname(opt_par_named[names(opt_par_named) == nom])
    }
  }

  map_list <- list()
  for (terme in termes_retires) {
    parameters_shape[[terme]] <- 0
    map_list[[terme]] <- factor(NA)
  }

  f_shape <- make_f_full(data_full_real, method, shape)

  set.seed(42)
  obj_shape <- RTMB::MakeADFun(f_shape, parameters_shape, random = "b_cohort_resid",
                                map = map_list, silent = TRUE)

  opt_shape <- tryCatch(
    nlminb(obj_shape$par, obj_shape$fn, obj_shape$gr,
           control = list(trace = 10, iter.max = 5000, eval.max = 10000)),
    error = function(e) { cat("  ECHEC :", conditionMessage(e), "\n"); NULL }
  )

  if (is.null(opt_shape)) {
    resultats_complets[[sp]] <- list(shape = shape, echec = TRUE)
    next
  }

  rep_shape <- obj_shape$report(obj_shape$env$last.par.best)

  cat("  convergence (nlminb) :", opt_shape$convergence, "\n")

  grad_final <- tryCatch(obj_shape$gr(opt_shape$par),
                          error = function(e) { cat("  ECHEC calcul gradient :", conditionMessage(e), "\n"); NA })
  cat("  norme max du gradient final :", max(abs(grad_final)), "\n")

  sd_shape <- tryCatch(sdreport(obj_shape),
                        error = function(e) { cat("  ECHEC sdreport :", conditionMessage(e), "\n"); NULL })
  cat("  sdreport pdHess :", if (!is.null(sd_shape)) sd_shape$pdHess else "sd_shape est NULL", "\n")

  k_shape   <- length(opt_shape$par)
  AIC_shape <- 2 * opt_shape$objective + 2 * k_shape

  coef_interet <- "beta_anom_mean"
  est_interet  <- rep_shape$beta_anom_mean

  z_interet <- NA; p_interet <- NA; se_interet <- NA
  if (!is.null(sd_shape) && isTRUE(sd_shape$pdHess) && !("beta_anom_mean" %in% termes_retires)) {
    tab_shape <- summary(sd_shape, select = "report")
    if (coef_interet %in% rownames(tab_shape)) {
      se_interet <- tab_shape[coef_interet, "Std. Error"]
      z_interet  <- est_interet / se_interet
      p_interet  <- 2 * pnorm(-abs(z_interet))
    }
  }

  rtmb_models_dir <- file.path("results_rtmb", method, "rtmb_models", sp)
  fichiers_uniform <- list.files(rtmb_models_dir, pattern = "^resultat_uniform_.*\\.rds$", full.names = TRUE)
  r_uniform_pipeline <- readRDS(fichiers_uniform[which.max(file.info(fichiers_uniform)$mtime)])
  data_sp_used <- r_uniform_pipeline$data_sp_used

  # ---- IC 95% pour tous les termes retenus ----
  tab_coefs <- NULL
  if (!is.null(sd_shape) && isTRUE(sd_shape$pdHess)) {
    tab_summary <- summary(sd_shape, select = "report")
    tab_coefs <- do.call(rbind, lapply(r_selection$termes_retenus, function(terme) {
      if (terme %in% rownames(tab_summary)) {
        est <- tab_summary[terme, "Estimate"]
        se  <- tab_summary[terme, "Std. Error"]
        z   <- est / se
        p   <- 2 * pnorm(-abs(z))
        data.frame(species = sp, terme = terme, estimate = est, se = se,
                   IC_low = est - qnorm(0.975) * se, IC_high = est + qnorm(0.975) * se, z = z, p = p)
      } else {
        data.frame(species = sp, terme = terme, estimate = NA, se = NA,
                   IC_low = NA, IC_high = NA, z = NA, p = NA)
      }
    }))
  }

  resultats_complets[[sp]] <- list(
    shape = shape, convergence = opt_shape$convergence, message = opt_shape$message,
    pdHess = if (!is.null(sd_shape)) sd_shape$pdHess else NA,
    k = k_shape, objective = opt_shape$objective, AIC = AIC_shape,
    coef_interet = coef_interet,
    slope_est = est_interet, se_slope = se_interet, z = z_interet, p = p_interet,
    w_month = rep_shape$w_month,
    beta_trend     = rep_shape$beta_trend,
    beta_sd_abs    = rep_shape$beta_sd_abs,
    beta_anom_mean = rep_shape$beta_anom_mean,
    beta_anom_sd   = rep_shape$beta_anom_sd,
    sd_report_summary = if (!is.null(sd_shape) && isTRUE(sd_shape$pdHess))
                         as.data.frame(summary(sd_shape, select = "report")) else NULL,
    tab_coefs = tab_coefs,

    opt_par_full       = opt_shape$par,
    parameters_init    = parameters_shape,
    rep_shape_full     = rep_shape,
    sd_shape_full      = sd_shape,
    last_par_best      = obj_shape$env$last.par.best,
    data_full_used     = data_full_real,
    method_used        = method,
    shape_used         = shape,

    data_sp_used = data_sp_used,

    termes_retenus = r_selection$termes_retenus,
    termes_retires = termes_retires,

    echec = FALSE
  )

  cat("  convergence :", opt_shape$convergence, "| AIC :", round(AIC_shape, 2), "\n")
  print(tab_coefs)

  saveRDS(resultats_complets[[sp]],
          file.path(BASE_OUT, method, sp, "modele_final_LRT_complet.rds"))

  rm(obj_shape, opt_shape, rep_shape, sd_shape, parameters_shape, f_shape)
  gc(verbose = FALSE)
}

saveRDS(resultats_complets, file.path(BASE_OUT, "resultats_finaux_LRT_complets_4especes.rds"))

cat("\n\n########## SYNTHESE ##########\n")
tab_global <- do.call(rbind, lapply(resultats_complets, function(r) r$tab_coefs))
rownames(tab_global) <- NULL
print(tab_global, row.names = FALSE)