# ================================================================
# lance_figures_thermal_toutes_especes.R
# ================================================================

out_root <- "/home/mathis/NorthSeaSR_Estimation/SR_Estimation/rapport/rapport_final/figures/results/gratia_rtmb_thermal"

especes_candidates <- c("Clupea harengus", "Merlangius merlangus",
                         "Melanogrammus aeglefinus", "Trisopterus esmarkii")

for (sp in especes_candidates) {
  
  sp_slug <- gsub(" ", "_", sp)
  out_dir <- file.path(out_root, sp_slug)
  marker_file <- file.path(out_dir, "01_tenseur.pdf")
  
  if (file.exists(marker_file)) {
    cat(">>> Deja fait, on saute :", sp, "\n")
    next
  }
  
  cat("\n########################################\n")
  cat("Lancement figures thermiques :", sp, "\n")
  cat("########################################\n")
  
  status <- system2("Rscript",
                     args = c("run_one_figures_thermal.R", shQuote(sp)),
                     stdout = "", stderr = "")
  
  if (status != 0) {
    cat("!!! ECHEC pour", sp, "\n")
  } else {
    cat(">>> OK pour", sp, "\n")
  }
  
  Sys.sleep(2)
}

cat("\nTermine.\n")
