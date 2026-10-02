# ================================================================
# Lance les 9 especes x 2 methodes, un process R par run,
# reprise automatique sur cache RTMB (via test_temp_v4.R) et
# sur fichier de sortie individuel (si le script plante)
# ================================================================

especes <- c( "Gadus morhua", "Clupea harengus", "Pleuronectes platessa", "Sprattus sprattus", "Merlangius merlangus", "Trisopterus esmarkii", "Scomber scombrus", "Pollachius virens", "Melanogrammus aeglefinus")

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

  out_file <- file.path("resultats_9especes_par_run", paste0("resultat_", sp, "_", meth, ".rds"))

  if (file.exists(out_file)) {
    cat("\n>>> Deja fait, on saute :", key, "\n")
    resultats_cibles[[key]] <- readRDS(out_file)
    next
  }

  cat("\n########################################\n")
  cat("Run", i, "/", nrow(runs), ":", key, "\n")
  cat("########################################\n")

  log_mem(paste("avant", key))

  status <- system2(
    "Rscript",
    args = c("../scripts/run_one_species_method.R", shQuote(sp), shQuote(meth)),
    stdout = "", stderr = ""
  )

  if (status != 0) {
    cat("!!! ECHEC process pour", key, "(code retour", status, "), on continue\n")
    resultats_cibles[[key]] <- list(species = sp, method = meth, echec = TRUE,
                                     erreur = "echec du sous-process (voir logs)")
  } else if (file.exists(out_file)) {
    resultats_cibles[[key]] <- readRDS(out_file)
    cat(">>> OK pour", key, "\n")
  } else {
    cat("!!! Fichier de sortie manquant pour", key, "\n")
    resultats_cibles[[key]] <- list(species = sp, method = meth, echec = TRUE,
                                     erreur = "fichier de sortie absent")
  }

  saveRDS(resultats_cibles, "resultats_9especes_partiel.rds")

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
