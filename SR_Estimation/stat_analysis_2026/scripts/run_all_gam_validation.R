# ================================================================
# Lance la selection backward LRT du GAM (penalisation libre)
# pour les 4 especes, une session R par espece
# ================================================================

especes_cibles <- c(
  "Gadus morhua", "Clupea harengus", "Pleuronectes platessa",
  "Sprattus sprattus", "Merlangius merlangus", "Trisopterus esmarkii",
  "Scomber scombrus", "Melanogrammus aeglefinus"
)

for (sp in especes_cibles) {

  cat("\n########################################\n")
  cat("GAM selection :", sp, "\n")
  cat("########################################\n")

  out_file <- file.path("results_gam_validation", paste0("gam_selection_", sp, ".rds"))

  if (file.exists(out_file)) {
    cat(">>> Deja fait, on saute :", sp, "\n")
    next
  }

  status <- system2("Rscript",
                     args = c("../scripts/run_one_gam_species.R", shQuote(sp)),
                     stdout = "", stderr = "")

  if (status != 0) {
    cat("!!! ECHEC pour", sp, "\n")
  } else {
    cat(">>> OK pour", sp, "\n")
  }
}

cat("\n\n########## SYNTHESE ##########\n")
for (sp in especes_cibles) {
  out_file <- file.path("results_gam_validation", paste0("gam_selection_", sp, ".rds"))
  if (file.exists(out_file)) {
    r <- readRDS(out_file)
    cat("\n", sp, ": retenus =", paste(r$termes_retenus, collapse = ", "),
        "| retires =", paste(r$termes_retires, collapse = ", "), "\n")
  }
}
