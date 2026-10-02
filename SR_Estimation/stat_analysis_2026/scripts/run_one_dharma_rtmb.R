# ================================================================
# run_one_dharma_rtmb.R
# Reconstruit le modele final (alpha=0.10) pour UNE espece,
# simule des residus DHARMa manuellement, sauvegarde les deux plots.
# Usage : Rscript run_one_dharma_rtmb.R "<espece>" "<fichier.rds>" "<termes_fixes_csv>"
# ================================================================

args <- commandArgs(trailingOnly = TRUE)
sp               <- args[1]
fichier_modele   <- args[2]
termes_fixes_csv <- args[3]

termes_fixes <- if (termes_fixes_csv == "VIDE") character(0) else strsplit(termes_fixes_csv, ",")[[1]]

library(RTMB)
library(DHARMa)
library(rprojroot)

proj_root <- find_root(has_file(".git") | is_rstudio_project)
setwd(file.path(proj_root, "SR_Estimation", "stat_analysis_2026", "data"))

source("../scripts/test_temp_v4.R")

BASE_OUT <- "/home/mathis/NorthSeaSR_Estimation/SR_Estimation/stat_analysis_2026/data/results_rtmb/backward_selection_LRT"
termes_thermiques_detrend <- c("beta_trend", "beta_sd_abs", "beta_anom_mean", "beta_anom_sd")

out_root <- "/home/mathis/NorthSeaSR_Estimation/SR_Estimation/rapport/rapport_final/figures/results/dharma_rtmb"
sp_slug <- gsub(" ", "_", sp)
out_dir <- file.path(out_root, sp_slug)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ---- Reconstruction du modele (identique au script sdreport) ----
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

rep_final <- obj_final$report(obj_final$env$last.par.best)

# ---- Simulation manuelle des residus DHARMa ----
y_obs <- data_full$y
p_hat <- rep_final$p_hat

n_sim <- 500
n_obs <- length(y_obs)

set.seed(42)
sim_matrix <- matrix(rbinom(n_obs * n_sim, size = 1, prob = rep(p_hat, n_sim)),
                      nrow = n_obs, ncol = n_sim)

dharma_obj <- createDHARMa(
  simulatedResponse = sim_matrix,
  observedResponse  = y_obs,
  fittedPredictedResponse = p_hat,
  integerResponse = TRUE
)

# ---- QQ-plot pur ----
pdf(file.path(out_dir, "01_qqplot.pdf"), width = 8, height = 7)
qqplot(
  ppoints(length(dharma_obj$scaledResiduals)),
  dharma_obj$scaledResiduals,
  xlab = "Quantiles théoriques (loi uniforme)",
  ylab = "Quantiles observés (résidus simulés)",
  main = paste("QQ-plot -", sp),
  pch  = 16, cex = 0.5, col = rgb(0, 0, 0, 0.3)
)
abline(0, 1, col = "red", lty = 2)
dev.off()

# ---- Residus vs valeurs ajustees ----
pdf(file.path(out_dir, "02_residuals_fitted.pdf"), width = 8, height = 7)
plotResiduals(dharma_obj,
              xlab = "Probabilité prédite d'être mâle",
              ylab = "Résidu simulé (échelle uniforme)",
              main = paste("Résidus vs valeurs ajustées -", sp))
dev.off()

cat("Termine pour", sp, "\n")
