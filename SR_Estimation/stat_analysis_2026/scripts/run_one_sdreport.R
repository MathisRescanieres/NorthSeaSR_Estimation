# ================================================================
# run_one_sdreport.R
# Reconstruit le modele final retenu (alpha=0.10) pour UNE espece,
# calcule le sdreport, sauvegarde le resultat, puis quitte.
# Usage : Rscript run_one_sdreport.R "<espece>" "<fichier.rds>" "<termes_fixes_csv>"
# ================================================================

args <- commandArgs(trailingOnly = TRUE)
sp             <- args[1]
fichier_modele <- args[2]
termes_fixes_csv <- args[3]

termes_fixes <- if (termes_fixes_csv == "VIDE") character(0) else strsplit(termes_fixes_csv, ",")[[1]]

library(RTMB)
library(rprojroot)

proj_root <- find_root(has_file(".git") | is_rstudio_project)
setwd(file.path(proj_root, "SR_Estimation", "stat_analysis_2026", "data"))

source("../scripts/test_temp_v4.R")  # doit definir make_f_full()

BASE_OUT <- "/home/mathis/NorthSeaSR_Estimation/SR_Estimation/stat_analysis_2026/data/results_rtmb/backward_selection_LRT"
termes_thermiques_detrend <- c("beta_trend", "beta_sd_abs", "beta_anom_mean", "beta_anom_sd")

f <- file.path(BASE_OUT, "detrend", sp, fichier_modele)
r <- readRDS(f)

data_full <- r$data_full_used
method <- r$method_used
shape  <- r$shape_used

op <- r$opt_par_full

beta_fixed_est <- op[names(op) == "beta_fixed"]
b_cohort_est   <- r$last_par_best[names(r$last_par_best) == "b_cohort_resid"]

parameters_final <- r$parameters_init
parameters_final$beta_fixed     <- as.numeric(beta_fixed_est)
parameters_final$b_cohort_resid <- as.numeric(b_cohort_est)

for (terme in termes_thermiques_detrend) {
  if (terme %in% names(op)) {
    parameters_final[[terme]] <- as.numeric(op[names(op) == terme])
  }
}

f_shape <- make_f_full(data_full, method, shape)

map_list <- list()
for (terme in termes_fixes) {
  map_list[[terme]] <- factor(NA)
  parameters_final[[terme]] <- 0
}

obj_final <- RTMB::MakeADFun(f_shape, parameters_final,
                              random = "b_cohort_resid",
                              map = map_list,
                              silent = TRUE)

opt_final <- nlminb(obj_final$par, obj_final$fn, obj_final$gr,
                     control = list(iter.max = 2000, eval.max = 4000))

cat("Objective apres reoptimisation :", round(opt_final$objective, 2),
    "| Objective sauvegarde :", round(r$objective, 2), "\n")

sd_final <- tryCatch(
  sdreport(obj_final),
  error = function(e) { cat("Echec sdreport :", conditionMessage(e), "\n"); NULL }
)

tab_sd <- if (!is.null(sd_final)) {
  as.data.frame(summary(sd_final, select = "report"))
} else {
  NULL
}

resultat <- list(
  species = sp,
  fichier_source = fichier_modele,
  termes_fixes = termes_fixes,
  objective_final = opt_final$objective,
  convergence = opt_final$convergence,
  tab_sd = tab_sd,
  pdHess = if (!is.null(sd_final)) sd_final$pdHess else NA,
  sdreport_full = sd_final
)

out_dir <- file.path(BASE_OUT, "sdreport_finaux")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

sp_slug <- gsub(" ", "_", sp)
saveRDS(resultat, file.path(out_dir, paste0("sdreport_", sp_slug, ".rds")))

cat("Termine pour", sp, "\n")
