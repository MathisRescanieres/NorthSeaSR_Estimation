# ================================================================
# Tableau AIC complet, detrend + raw, les 9 especes
# ================================================================
library(rprojroot)
proj_root <- find_root(has_file(".git") | is_rstudio_project)
setwd(file.path(proj_root, "SR_Estimation", "stat_analysis_2026", "data"))

especes_cibles <- c(
  "Gadus morhua", "Clupea harengus", "Pleuronectes platessa",
  "Sprattus sprattus", "Merlangius merlangus", "Trisopterus esmarkii",
  "Scomber scombrus", "Pollachius virens", "Melanogrammus aeglefinus"
)
methodes_cibles <- c("detrend", "raw")

tab_aic_liste <- list()

for (method in methodes_cibles) {
  for (sp in especes_cibles) {

    rtmb_models_dir <- file.path("results_rtmb", method, "rtmb_models", sp)
    if (!dir.exists(rtmb_models_dir)) {
      cat("Dossier absent pour", sp, "-", method, "\n")
      next
    }

    fichiers_tab <- list.files(rtmb_models_dir, pattern = "^tab_comparatif_.*\\.rds$", full.names = TRUE)
    if (length(fichiers_tab) == 0) {
      cat("Aucun tab_comparatif trouve pour", sp, "-", method, "\n")
      next
    }

    fichier <- fichiers_tab[which.max(file.info(fichiers_tab)$mtime)]
    tab <- readRDS(fichier)

    tab$species <- sp
    tab$method  <- method
    key <- paste(sp, method, sep = "_")
    tab_aic_liste[[key]] <- tab[, c("species", "method", "shape", "AIC", "delta_AIC_vs_null",
                                     "convergence", "coef_interet", "slope")]
  }
}

tab_aic_final <- do.call(rbind, tab_aic_liste)
rownames(tab_aic_final) <- NULL
tab_aic_final <- tab_aic_final[order(tab_aic_final$species, tab_aic_final$method, tab_aic_final$AIC), ]

cat("\n========== TABLEAU AIC — DETREND + RAW (9 especes) ==========\n")
print(tab_aic_final, row.names = FALSE)

# ---- Uniquement la meilleure forme par espece x methode ----
meilleure_forme <- do.call(rbind, lapply(split(tab_aic_final, paste(tab_aic_final$species, tab_aic_final$method)),
                                          function(d) d[which.min(d$AIC), ]))
rownames(meilleure_forme) <- NULL
meilleure_forme <- meilleure_forme[order(meilleure_forme$species, meilleure_forme$method), ]

cat("\n========== MEILLEURE FORME PAR ESPECE x METHODE ==========\n")
print(meilleure_forme, row.names = FALSE)

saveRDS(tab_aic_final, "tab_aic_detrend_raw_9especes.rds")
