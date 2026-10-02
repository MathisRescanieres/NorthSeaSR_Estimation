# =============================================================================
#  Diagnostics DHARMa - QQ-plot et résidus vs valeurs ajustées, par espèce
#  Modèles chargés depuis le cache bam_ref (un par espèce)
#  Sortie : /figures/results/dharma/<espece>/
# =============================================================================

library(DHARMa)
library(mgcv)

base_dir <- "/home/mathis/NorthSeaSR_Estimation/SR_Estimation/stat_analysis_2026/data/results_rtmb/detrend/bam_null_models"
out_root <- "/home/mathis/NorthSeaSR_Estimation/SR_Estimation/rapport/rapport_final/figures/results/dharma"

species_dirs <- list.dirs(base_dir, recursive = FALSE)

for (sp_dir in species_dirs) {

  sp <- basename(sp_dir)
  cat("DHARMa :", sp, "...\n")

  rds_files <- list.files(sp_dir, pattern = "^bam_ref_.*\\.rds$", full.names = TRUE)

  if (length(rds_files) == 0) {
    cat("  Aucun modèle trouvé pour", sp, "- ignoré.\n\n")
    next
  }

  if (length(rds_files) > 1) {
    info <- file.info(rds_files)
    rds_file <- rds_files[which.max(info$mtime)]
    cat("  Plusieurs modèles trouvés, utilisation du plus récent :", basename(rds_file), "\n")
  } else {
    rds_file <- rds_files[1]
  }

  bam_ref <- readRDS(rds_file)

  sp_slug <- gsub(" ", "_", sp)
  out_dir <- file.path(out_root, sp_slug)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  sim_res <- simulateResiduals(
    fittedModel = bam_ref,
    n           = 500,
    plot        = FALSE,
    seed        = 42
  )

  # ── 1. QQ-plot ────────
  pdf(file.path(out_dir, "01_qqplot.pdf"), width = 8, height = 7)
  qqplot(
    ppoints(length(sim_res$scaledResiduals)),
    sim_res$scaledResiduals,
    xlab = "Quantiles théoriques",
    ylab = "Quantiles observés",
    main = paste("QQ-plot -", sp),
    pch  = 16,
    cex  = 0.5,
    col  = rgb(0, 0, 0, 0.3)
  )
  abline(0, 1, col = "red", lty = 2)
  dev.off()

  # ── 2. Résidus vs valeurs ajustées ─────────────────────────────────────────
  pdf(file.path(out_dir, "02_residuals_fitted.pdf"), width = 8, height = 7)
  plotResiduals(sim_res,
                xlab = "Probabilité prédite d'être mâle",
                ylab = "Résidu simulé",
                main = paste("Résidus vs valeurs ajustées -", sp))
  dev.off()

  cat("  Done :", out_dir, "\n\n")
}