# ================================================================
# lance_dharma_rtmb.R
# Lance run_one_dharma_rtmb.R en sous-process, une espece a la fois.
# ================================================================

BASE_OUT <- "/home/mathis/NorthSeaSR_Estimation/SR_Estimation/stat_analysis_2026/data/results_rtmb/backward_selection_LRT"

modeles_finaux_010 <- list(
  "Clupea harengus"          = list(fichier = "modele_complet.rds",
                                     termes_fixes = c("beta_anom_mean", "beta_sd_abs", "beta_anom_sd")),
  "Merlangius merlangus"     = list(fichier = "sans_beta_anom_sd_beta_sd_abs.rds",
                                     termes_fixes = c("beta_sd_abs", "beta_anom_sd")),
  "Melanogrammus aeglefinus" = list(fichier = "sans_beta_sd_abs.rds",
                                     termes_fixes = c("beta_sd_abs")),
  "Trisopterus esmarkii"     = list(fichier = "sans_beta_anom_sd_beta_sd_abs.rds",
                                     termes_fixes = c("beta_sd_abs", "beta_anom_sd"))
)

out_root <- "/home/mathis/NorthSeaSR_Estimation/SR_Estimation/rapport/rapport_final/figures/results/dharma_rtmb"

for (sp in names(modeles_finaux_010)) {
  
  sp_slug <- gsub(" ", "_", sp)
  out_dir <- file.path(out_root, sp_slug)
  fichier_qq <- file.path(out_dir, "01_qqplot.pdf")
  
  if (file.exists(fichier_qq)) {
    cat(">>> Deja fait, on saute :", sp, "\n")
    next
  }
  
  cat("\n########################################\n")
  cat("Lancement DHARMa RTMB :", sp, "\n")
  cat("########################################\n")
  
  config <- modeles_finaux_010[[sp]]
  termes_csv <- if (length(config$termes_fixes) == 0) "VIDE" else paste(config$termes_fixes, collapse = ",")
  
  status <- system2("Rscript",
                     args = c("run_one_dharma_rtmb.R", shQuote(sp), shQuote(config$fichier), shQuote(termes_csv)),
                     stdout = "", stderr = "")
  
  if (status != 0) {
    cat("!!! ECHEC pour", sp, "\n")
  } else {
    cat(">>> OK pour", sp, "\n")
  }
  
  Sys.sleep(2)
}

cat("\nTermine.\n")
