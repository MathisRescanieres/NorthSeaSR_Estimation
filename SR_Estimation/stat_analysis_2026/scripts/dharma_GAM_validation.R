# =============================================================================
#  Diagnostics DHARMa - plots individuels par espèce (PDF)
#  Résidus produits pour TOUS les prédicteurs retenus dans chaque modèle.
#  Sortie : /figures/results/dharma/<espece>/
# =============================================================================

library(DHARMa)
library(ggplot2)

out_root <- "/home/mathis/NorthSeaSR_Estimation/SR_Estimation/rapport/rapport_miproj/figures/results/dharma"

# Prédicteurs continus : nom dans data_expanded → label lisible
continuous_preds <- list(
  Age_sc               = "Age",
  LngtClassGrouped_sc  = "Taille",
  Cohort_num_sc        = "Cohorte",
  julian_day           = "Jour julien",
  Latitude             = "Latitude",
  Longitude            = "Longitude"
)

# Prédicteurs facteurs (résidus agrégés par groupe)
factor_preds <- list(
  Cohort_fact = "Cohorte (agrégé)"
)

for (sp in c("Pollachius virens")) {

  cat("DHARMa :", sp, "...\n")

  sp_slug <- gsub(" ", "_", sp)
  out_dir <- file.path(out_root, sp_slug)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  data_model <- model.frame(gam_models_final[[sp]])
  vars_model <- names(data_model)

  sim_res <- simulateResiduals(
    fittedModel = gam_models_final[[sp]],
    n           = 500,
    plot        = FALSE,
    seed        = 42
  )

  # ── 1. QQ-plot ─────────────────────────────────────────────────────────────
  pdf(file.path(out_dir, "01_qqplot.pdf"), width = 8, height = 7)
  plotQQunif(sim_res,
             testUniformity = FALSE,
             testOutliers   = FALSE,
             testDispersion = FALSE,
             main           = paste("QQ-plot -", sp))
  dev.off()

  # ── 2. Résidus vs valeurs ajustées ─────────────────────────────────────────
  pdf(file.path(out_dir, "02_residuals_fitted.pdf"), width = 8, height = 7)
  plotResiduals(sim_res,
                main = paste("Résidus vs ajustées -", sp))
  dev.off()

  # ── 3. Résidus vs prédicteurs continus retenus ─────────────────────────────
  plot_idx <- 3L

  for (var in names(continuous_preds)) {
    if (!var %in% vars_model) next

    label    <- continuous_preds[[var]]
    fname    <- sprintf("%02d_residuals_%s.pdf", plot_idx, tolower(gsub(" ", "_", label)))
    plot_idx <- plot_idx + 1L

    pdf(file.path(out_dir, fname), width = 8, height = 7)
    plotResiduals(sim_res,
                  form              = data_model[[var]],
                  absoluteDeviation = TRUE,
                  quantreg          = TRUE,
                  xlab              = label,
                  main              = paste("Résidus vs", label, "-", sp))
    dev.off()
  }

  # ── 4. Résidus agrégés par facteurs retenus ────────────────────────────────
  for (var in names(factor_preds)) {
    if (!var %in% vars_model) next

    label    <- factor_preds[[var]]
    fname    <- sprintf("%02d_residuals_%s.pdf", plot_idx, tolower(gsub("[() ]", "_", label)))
    plot_idx <- plot_idx + 1L

    sim_res_grp <- recalculateResiduals(sim_res, group = data_model[[var]])
    pdf(file.path(out_dir, fname), width = 9, height = 7)
    plotResiduals(sim_res_grp,
                  form = unique(data_model[[var]]),
                  main = paste("Résidus -", label, "-", sp))
    dev.off()
  }

  cat("  Done →", out_dir, "\n\n")
}