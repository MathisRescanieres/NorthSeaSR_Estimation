# ================================================================
# Lance les 4 especes en sous-process, une a une, pour eviter l'OOM
# ================================================================

especes <- list(
  list(sp = "Pleuronectes platessa", method = "detrend",
       k_age = 6, k_lngt = 6, k_fs = 8, k_space = 240, k_time = 12,
       bam_ref_file = "results_rtmb/detrend/bam_null_models/Pleuronectes platessa/bam_ref_Pleuronectes platessa_6_6_240_12_8_cs3_114458.rds"),
  list(sp = "Clupea harengus", method = "detrend",
       k_age = 6, k_lngt = 6, k_fs = 8, k_space = 120, k_time = 12,
       bam_ref_file = "results_rtmb/detrend/bam_null_models/Clupea harengus/bam_ref_Clupea harengus_6_6_120_12_8_cs3_234508.rds"),
  list(sp = "Melanogrammus aeglefinus", method = "detrend",
       k_age = 6, k_lngt = 6, k_fs = 8, k_space = 120, k_time = 12,
       bam_ref_file = "results_rtmb/detrend/bam_null_models/Melanogrammus aeglefinus/bam_ref_Melanogrammus aeglefinus_6_6_120_12_8_cs3_212799.rds"),
  list(sp = "Merlangius merlangus", method = "detrend",
       k_age = 6, k_lngt = 6, k_fs = 8, k_space = 120, k_time = 12,
       bam_ref_file = "results_rtmb/detrend/bam_null_models/Merlangius merlangus/bam_ref_Merlangius merlangus_6_6_120_12_8_cs3_225143.rds")
)

log_mem <- function(label) {
  if (Sys.which("free") != "") {
    cat("--- Memoire (", label, ") ---\n")
    system("free -h")
  }
}

purge_caches <- function() {
  # sync force l'ecriture des buffers disque
  # sans acces root, on ne peut pas vider le swap depuis R
  # mais sync + une pause laisse le noyau reclamer les pages libres
  if (Sys.which("sync") != "") system("sync")
  Sys.sleep(2)
}

for (i in seq_along(especes)) {
  e <- especes[[i]]
  cat("\n########################################\n")
  cat("Espece", i, "/", length(especes), ":", e$sp, "\n")
  cat("########################################\n")

  log_mem(paste("avant", e$sp))

  status <- system2(
    "Rscript",
    args = c(
      "run_one_species.R",
      shQuote(e$sp), shQuote(e$method),
      e$k_age, e$k_lngt, e$k_fs, e$k_space, e$k_time,
      shQuote(e$bam_ref_file)
    ),
    stdout = "", stderr = ""
  )

  if (status != 0) {
    cat("!!! ECHEC pour", e$sp, "(code retour", status, "), on continue quand meme\n")
  } else {
    cat(">>> OK pour", e$sp, "\n")
  }

  purge_caches()
  log_mem(paste("apres", e$sp))
}

cat("\nTermine. 4 especes traitees.\n")
