# ================================================================
# lance_sdreport_null_toutes_especes.R
# Lance run_one_sdreport_null.R en sous-process, une espece a la fois,
# avec reprise automatique si deja calcule.
# ================================================================

base_dir_bam  <- "/home/mathis/NorthSeaSR_Estimation/SR_Estimation/stat_analysis_2026/data/results_rtmb/detrend/bam_null_models"
out_root_sdreport <- "/home/mathis/NorthSeaSR_Estimation/SR_Estimation/stat_analysis_2026/data/results_rtmb/rtmb_null_sdreport"

if (!dir.exists(out_root_sdreport)) dir.create(out_root_sdreport, recursive = TRUE)

species_all <- basename(list.dirs(base_dir_bam, recursive = FALSE))

resultats <- list()

for (sp in species_all) {
  
  sp_slug <- gsub(" ", "_", sp)
  out_dir_sp <- file.path(out_root_sdreport, sp_slug)
  out_file <- file.path(out_dir_sp, paste0("sdreport_null_", sp_slug, ".rds"))
  
  if (file.exists(out_file)) {
    cat(">>> Deja fait, on saute :", sp, "\n")
    resultats[[sp]] <- readRDS(out_file)
    next
  }
  
  cat("\n########################################\n")
  cat("Lancement :", sp, "\n")
  cat("########################################\n")
  
  status <- system2("Rscript",
                     args = c("run_one_sdreport_null.R", shQuote(sp)),
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

saveRDS(resultats, file.path(out_root_sdreport, "sdreport_null_toutes_especes.rds"))

cat("\n\n########## SYNTHESE ##########\n")
for (sp in names(resultats)) {
  r <- resultats[[sp]]
  if (isTRUE(r$echec)) {
    cat(sp, ": ECHEC\n")
  } else {
    cat(sp, ": OK | pdHess =", r$pdHess, "\n")
  }
}
