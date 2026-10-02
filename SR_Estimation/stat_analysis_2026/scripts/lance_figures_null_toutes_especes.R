# ================================================================
# lance_figures_null_toutes_especes.R
# ================================================================

base_dir_bam <- "/home/mathis/NorthSeaSR_Estimation/SR_Estimation/stat_analysis_2026/data/results_rtmb/detrend/bam_null_models"
out_root_fig <- "/home/mathis/NorthSeaSR_Estimation/SR_Estimation/rapport/rapport_final/figures/results/gratia_rtmb_null"

species_all <- basename(list.dirs(base_dir_bam, recursive = FALSE))

for (sp in species_all) {
  
  sp_slug <- gsub(" ", "_", sp)
  out_dir <- file.path(out_root_fig, sp_slug)
  marker_file <- file.path(out_dir, "01_tenseur.pdf")
  
  if (file.exists(marker_file)) {
    cat(">>> Deja fait, on saute :", sp, "\n")
    next
  }
  
  cat("\n########################################\n")
  cat("Lancement figures :", sp, "\n")
  cat("########################################\n")
  
  status <- system2("Rscript",
                     args = c("run_one_figures_null.R", shQuote(sp)),
                     stdout = "", stderr = "")
  
  if (status != 0) {
    cat("!!! ECHEC pour", sp, "\n")
  } else {
    cat(">>> OK pour", sp, "\n")
  }
  
  Sys.sleep(2)
}

cat("\nTermine.\n")
