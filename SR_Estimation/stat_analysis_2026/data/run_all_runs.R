# ================================================================
# Lance tous les runs cibles, un process R par run, pour eviter l'OOM
# ================================================================

especes <- c("Clupea harengus", "Merlangius merlangus",
             "Melanogrammus aeglefinus", "Pleuronectes platessa")
methodes <- c("detrend", "raw")

runs <- expand.grid(sp = especes, meth = methodes, stringsAsFactors = FALSE)

log_mem <- function(label) {
  if (Sys.which("free") != "") {
    cat("--- Memoire (", label, ") ---\n")
    system("free -h")
  }
}

purge_caches <- function() {
  if (Sys.which("sync") != "") system("sync")
  Sys.sleep(2)
}

resultats_cibles <- list()

for (i in seq_len(nrow(runs))) {

  sp   <- runs$sp[i]
  meth <- runs$meth[i]
  key  <- paste(sp, meth, sep = "_")

  cat("\n########################################\n")
  cat("Run", i, "/", nrow(runs), ":", key, "\n")
  cat("########################################\n")

  log_mem(paste("avant", key))

  status <- system2(
    "Rscript",
    args = c("run_one_run.R", shQuote(sp), shQuote(meth)),
    stdout = "", stderr = ""
  )

  if (status != 0) {
    cat("!!! ECHEC process pour", key, "(code retour", status, "), on continue\n")
    resultats_cibles[[key]] <- list(species = sp, method = meth, echec = TRUE,
                                     erreur = "echec du sous-process (voir logs)")
  } else {
    out_file <- file.path("resultats_cibles_par_run", paste0("resultat_", sp, "_", meth, ".rds"))
    if (file.exists(out_file)) {
      resultats_cibles[[key]] <- readRDS(out_file)
      cat(">>> OK pour", key, "\n")
    } else {
      cat("!!! Fichier de sortie manquant pour", key, "\n")
      resultats_cibles[[key]] <- list(species = sp, method = meth, echec = TRUE,
                                       erreur = "fichier de sortie absent")
    }
  }

  saveRDS(resultats_cibles, "resultats_cibles_partiel.rds")

  purge_caches()
  log_mem(paste("apres", key))
}

cat("\n\n########## SYNTHESE ##########\n")
for (key in names(resultats_cibles)) {
  r <- resultats_cibles[[key]]
  if (is.null(r) || isTRUE(r$echec)) {
    cat(key, ": ECHEC\n")
  } else {
    cat(key, ": OK | AIC_null =", round(r$AIC_null, 2), "\n")
  }
}
