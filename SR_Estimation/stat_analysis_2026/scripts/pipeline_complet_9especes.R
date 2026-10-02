library(conflicted)
library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)
library(readr)
library(corrplot)
library(metR)
library(broom)
library(ggridges)
library(glmnet)
library(patchwork)
library(mgcv)
library(gratia)
library(pROC)
library(PRROC)
library(grid)
library(gridExtra)
library(infotheo)
library(lattice)
library(rprojroot)
library(GGally)
library(xgboost)
library(lubridate)
library(RTMB)
library(Matrix)

proj_root <- find_root(has_file(".git") | is_rstudio_project)
setwd(file.path(proj_root, "SR_Estimation", "stat_analysis_2026", "data"))

load("temp_v4.RData")

source("../scripts/test_temp_v4.R")

# ================================================================
# Lancement cible : Scomber scombrus (detrend + raw), Trisopterus esmarkii (raw seul)
# ================================================================

runs <- list(
  list(sp = "Sprattus sprattus", meth = "detrend")
)


resultats_cibles <- list()

for (r in runs) {

  cat("\n\n>>>> ", r$sp, "-", r$meth, "<<<<\n")

  key <- paste(r$sp, r$meth, sep = "_")

  resultats_cibles[[key]] <- tryCatch(
    run_thermal_pipeline(
      data_ind      = data_expanded_1991_2023,
      data_temp_raw = df_long,
      method        = r$meth,
      species       = r$sp
    )[[r$sp]],
    error = function(e) {
      cat("\n!!!! ERREUR pour", r$sp, "-", r$meth, ":", conditionMessage(e), "!!!!\n")
      list(species = r$sp, method = r$meth, echec = TRUE, erreur = conditionMessage(e))
    }
  )

  saveRDS(resultats_cibles, "resultats_cibles_partiel.rds")
  gc(verbose = FALSE)
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

# library(conflicted)
# library(dplyr)
# library(ggplot2)
# library(tidyr)
# library(purrr)
# library(readr)
# library(corrplot)
# library(metR)
# library(broom)
# library(ggridges)
# library(glmnet)
# library(patchwork)
# library(mgcv)
# library(gratia)
# library(pROC)
# library(PRROC)
# library(grid)
# library(gridExtra)
# library(infotheo)
# library(lattice)
# library(rprojroot)
# library(GGally)
# library(xgboost)

# proj_root <- find_root(has_file(".git") | is_rstudio_project)
# setwd(file.path(proj_root, "SR_Estimation", "stat_analysis_2026", "data"))

# library(dplyr)
# library(mgcv)
# library(lubridate)
# library(RTMB)
# library(Matrix)

# load("temp_v4.RData")
# source("../scripts/test_temp_v4.R")

# # ================================================================
# # Suppression chirurgicale des caches a refaire (convergence != 0
# # initialement, plus Sprattus sprattus detrend a reconfirmer avec
# # le control harmonise)
# # ================================================================

# fichiers_a_supprimer <- c(
#   "results_rtmb/detrend/rtmb_models/Pollachius virens/resultat_gaussian_*.rds",
#   "results_rtmb/raw/rtmb_models/Gadus morhua/resultat_uniform_*.rds",
#   "results_rtmb/raw/rtmb_models/Pollachius virens/resultat_uniform_*.rds",
#   "results_rtmb/raw/rtmb_models/Scomber scombrus/resultat_uniform_*.rds",
#   "results_rtmb/raw/rtmb_models/Trisopterus esmarkii/resultat_gaussian_*.rds",
#   "results_rtmb/detrend/rtmb_models/Sprattus sprattus/resultat_gaussian_*.rds",
#   "results_rtmb/detrend/rtmb_models/Sprattus sprattus/resultat_uniform_*.rds",
#   "results_rtmb/detrend/rtmb_models/Sprattus sprattus/resultat_linear_*.rds",
#   "results_rtmb/detrend/rtmb_models/Sprattus sprattus/resultat_skewnormal_*.rds"
# )

# for (pattern in fichiers_a_supprimer) {
#   f <- Sys.glob(pattern)
#   if (length(f) > 0) {
#     file.remove(f)
#     cat("Supprime :", f, "\n")
#   } else {
#     cat("Aucun fichier trouve pour :", pattern, "\n")
#   }
# }

# # ================================================================
# # Lancement cible : les combinaisons espece/methode dont au moins
# # une forme a ete supprimee ci-dessus
# # ================================================================

# runs <- list(
#   list(sp = "Pollachius virens",    meth = "detrend"),
#   list(sp = "Gadus morhua",         meth = "raw"),
#   list(sp = "Pollachius virens",    meth = "raw"),
#   list(sp = "Scomber scombrus",     meth = "raw"),
#   list(sp = "Trisopterus esmarkii", meth = "raw"),
#   list(sp = "Sprattus sprattus",    meth = "detrend")
# )

# resultats_cibles <- list()

# for (r in runs) {

#   cat("\n\n>>>> ", r$sp, "-", r$meth, "<<<<\n")

#   key <- paste(r$sp, r$meth, sep = "_")

#   resultats_cibles[[key]] <- tryCatch(
#     run_thermal_pipeline(
#       data_ind      = data_expanded_1991_2023,
#       data_temp_raw = df_long,
#       method        = r$meth,
#       species       = r$sp
#     )[[r$sp]],
#     error = function(e) {
#       cat("\n!!!! ERREUR pour", r$sp, "-", r$meth, ":", conditionMessage(e), "!!!!\n")
#       list(species = r$sp, method = r$meth, echec = TRUE, erreur = conditionMessage(e))
#     }
#   )

#   saveRDS(resultats_cibles, "resultats_cibles_partiel.rds")
#   gc(verbose = FALSE)
# }

# cat("\n\n########## SYNTHESE ##########\n")
# for (key in names(resultats_cibles)) {
#   r <- resultats_cibles[[key]]
#   if (is.null(r) || isTRUE(r$echec)) {
#     cat(key, ": ECHEC\n")
#   } else {
#     cat(key, ": OK | AIC_null =", round(r$AIC_null, 2), "\n")
#   }
# }
