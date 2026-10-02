# ================================================================
# lance_sdreport_toutes_especes.R
# Lance run_one_sdreport.R en sous-process, une espece a la fois,
# avec reprise automatique si deja calcule.
# ================================================================

BASE_OUT <- "/home/mathis/NorthSeaSR_Estimation/SR_Estimation/stat_analysis_2026/data/results_rtmb/backward_selection_LRT"

modeles_finaux_010 <- list(
  "Clupea harengus"          = list(fichier = "sans_beta_anom_mean_beta_anom_sd_beta_sd_abs.rds",
                                     termes_fixes = c("beta_anom_mean", "beta_sd_abs", "beta_anom_sd")),
  "Merlangius merlangus"     = list(fichier = "sans_beta_anom_sd_beta_sd_abs.rds",
                                     termes_fixes = c("beta_sd_abs", "beta_anom_sd")),
  "Melanogrammus aeglefinus" = list(fichier = "sans_beta_sd_abs.rds",
                                     termes_fixes = c("beta_sd_abs")),
  "Trisopterus esmarkii"     = list(fichier = "sans_beta_anom_sd_beta_sd_abs.rds",
                                     termes_fixes = c("beta_sd_abs", "beta_anom_sd"))
)

out_dir <- file.path(BASE_OUT, "sdreport_finaux")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

resultats <- list()

for (sp in names(modeles_finaux_010)) {
  
  sp_slug <- gsub(" ", "_", sp)
  out_file <- file.path(out_dir, paste0("sdreport_", sp_slug, ".rds"))
  
  if (file.exists(out_file)) {
    cat(">>> Deja fait, on saute :", sp, "\n")
    resultats[[sp]] <- readRDS(out_file)
    next
  }
  
  cat("\n########################################\n")
  cat("Lancement :", sp, "\n")
  cat("########################################\n")
  
  config <- modeles_finaux_010[[sp]]
  termes_csv <- if (length(config$termes_fixes) == 0) "VIDE" else paste(config$termes_fixes, collapse = ",")
  
  status <- system2("Rscript",
                     args = c("run_one_sdreport.R", shQuote(sp), shQuote(config$fichier), shQuote(termes_csv)),
                     stdout = "", stderr = "")
  
  if (status != 0 || !file.exists(out_file)) {
    cat("!!! ECHEC pour", sp, "\n")
    resultats[[sp]] <- list(species = sp, echec = TRUE)
  } else {
    resultats[[sp]] <- readRDS(out_file)
    cat(">>> OK pour", sp, "\n")
  }
  
  Sys.sleep(2)
}

saveRDS(resultats, file.path(BASE_OUT, "sdreport_toutes_especes.rds"))

cat("\n\n########## SYNTHESE ##########\n")
for (sp in names(resultats)) {
  r <- resultats[[sp]]
  if (isTRUE(r$echec)) {
    cat(sp, ": ECHEC\n")
  } else {
    cat(sp, ": OK | pdHess =", r$pdHess, "\n")
  }
}
