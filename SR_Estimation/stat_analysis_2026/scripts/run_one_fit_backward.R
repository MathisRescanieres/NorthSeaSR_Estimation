# ================================================================
# Fit d'un seul modele reduit, dans un process R isole
# Usage : Rscript run_one_fit_backward.R <espece> <methode> <termes_a_fixer_separes_par_virgule_ou_VIDE> <out_dir_base>
# ================================================================

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 4) {
  stop("Usage: Rscript run_one_fit.R <espece> <methode> <termes_csv_ou_VIDE> <out_dir_base>")
}

sp              <- args[1]
method          <- args[2]
termes_csv      <- args[3]
out_dir_base    <- args[4]

termes_a_fixer <- if (termes_csv == "VIDE") character(0) else strsplit(termes_csv, ",")[[1]]

library(dplyr)
library(mgcv)
library(lubridate)
library(RTMB)
library(Matrix)
library(rprojroot)

proj_root <- find_root(has_file(".git") | is_rstudio_project)
setwd(file.path(proj_root, "SR_Estimation", "stat_analysis_2026", "data"))

source("../scripts/test_temp_v4.R")

MONTH_OFFSETS <- 0:11

out_dir <- file.path(out_dir_base, method, sp)
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

signature <- if (length(termes_a_fixer) == 0) "modele_complet" else
  paste0("sans_", paste(sort(termes_a_fixer), collapse = "_"))
out_file <- file.path(out_dir, paste0(signature, ".rds"))

if (file.exists(out_file)) {
  cat("Deja en cache :", signature, "\n")
  quit(save = "no", status = 0)
}

# ---- Cache du modele nul ----
null_dir <- file.path("results_rtmb", method, "rtmb_null_models", sp)
fichiers_null <- list.files(null_dir, pattern = "^null_model_.*\\.rds$", full.names = TRUE)
null_cache <- readRDS(fichiers_null[which.max(file.info(fichiers_null)$mtime)])
beta_null <- null_cache$opt_null$par[names(null_cache$opt_null$par) == "beta_fixed"]
rep_null  <- null_cache$rep_null

# ---- Cache du modele uniform (donnees completes) ----
rtmb_models_dir <- file.path("results_rtmb", method, "rtmb_models", sp)
fichiers_uniform <- list.files(rtmb_models_dir, pattern = "^resultat_uniform_.*\\.rds$", full.names = TRUE)
r_uniform <- readRDS(fichiers_uniform[which.max(file.info(fichiers_uniform)$mtime)])
data_full_real <- r_uniform$data_full_used

# ---- Fit ----
parameters_fit <- build_parameters(method, "uniform", MONTH_OFFSETS, beta_null, rep_null)

map_list <- list()
for (terme in termes_a_fixer) {
  parameters_fit[[terme]] <- 0
  map_list[[terme]] <- factor(NA)
}

f_shape <- make_f_full(data_full_real, method, "uniform")

obj <- RTMB::MakeADFun(f_shape, parameters_fit, random = "b_cohort_resid",
                        map = map_list, silent = TRUE)

opt <- nlminb(obj$par, obj$fn, obj$gr,
              control = list(trace = 10, iter.max = 5000, eval.max = 10000))

grad_final <- max(abs(obj$gr(opt$par)))
k <- length(obj$par)
AIC <- 2 * opt$objective + 2 * k

resultat <- list(
  signature = signature, termes_fixes = termes_a_fixer,
  convergence = opt$convergence, message = opt$message,
  objective = opt$objective, k = k, AIC = AIC,
  grad_max = grad_final,
  opt_par_full = opt$par, last_par_best = obj$env$last.par.best,
  parameters_init = parameters_fit, data_full_used = data_full_real,
  method_used = method, shape_used = "uniform"
)

saveRDS(resultat, out_file)
cat("[fit] ", signature, "| AIC =", round(AIC, 2),
    "| convergence =", opt$convergence, "| grad_max =", signif(grad_final, 3), "\n")

quit(save = "no", status = 0)
