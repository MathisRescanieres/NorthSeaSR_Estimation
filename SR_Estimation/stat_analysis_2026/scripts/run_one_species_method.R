# ================================================================
# Lance UNE espece + UNE methode, dans un process R isole
# Usage : Rscript run_one_species_method.R <espece> <methode>
# ================================================================

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2) {
  stop("Usage: Rscript run_one_species_method.R <espece> <methode (detrend|raw)>")
}

sp_target   <- args[1]
meth_target <- args[2]

library(dplyr)
library(mgcv)
library(lubridate)
library(RTMB)
library(Matrix)
library(rprojroot)

proj_root <- find_root(has_file(".git") | is_rstudio_project)
setwd(file.path(proj_root, "SR_Estimation", "stat_analysis_2026", "data"))

load("temp_v4.RData")
source("../scripts/test_temp_v4.R")

cat("\n\n>>>> ", sp_target, "-", meth_target, "<<<<\n")

resultat <- tryCatch(
  run_thermal_pipeline(
    data_ind      = data_expanded_1991_2023,
    data_temp_raw = df_long,
    method        = meth_target,
    species       = sp_target
  )[[sp_target]],
  error = function(e) {
    cat("\n!!!! ERREUR pour", sp_target, "-", meth_target, ":", conditionMessage(e), "!!!!\n")
    list(species = sp_target, method = meth_target, echec = TRUE, erreur = conditionMessage(e))
  }
)

out_dir <- "resultats_9especes_par_run"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

out_file <- file.path(out_dir, paste0("resultat_", sp_target, "_", meth_target, ".rds"))
saveRDS(resultat, out_file)
cat("Sauvegarde :", out_file, "\n")

cat("=== Fin :", sp_target, "-", meth_target, "===\n")
quit(save = "no", status = 0)
