# =============================================================================
#  Importance des variables XGBoost — baseline vs EOF
#  25 premiers termes par Gain, côte à côte par espèce
# =============================================================================

library(dplyr)
library(ggplot2)
library(patchwork)
library(xgboost)

dir_results <- "model_data/xgb_results"
out_dir     <- "/home/mathis/NorthSeaSR_Estimation/SR_Estimation/rapport/rapport_miproj/figures/results/xgboost"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

species_list <- c(
  "Clupea_harengus", "Gadus_morhua", "Melanogrammus_aeglefinus",
  "Merlangius_merlangus", "Pleuronectes_platessa", "Pollachius_virens",
  "Scomber_scombrus", "Sprattus_sprattus", "Trisopterus_esmarkii"
)

for (sp_slug in species_list) {

  rds_path <- file.path(dir_results, paste0(sp_slug, "_results.rds"))
  if (!file.exists(rds_path)) {
    cat("Résultats introuvables :", sp_slug, "— ignoré.\n")
    next
  }

  res <- readRDS(rds_path)
  sp_label <- gsub("_", " ", sp_slug)

  # ── Fonction : barplot importance ─────────────────────────────────────────
  make_importance_plot <- function(importance, title, n_top = 25) {

    top <- importance %>%
      arrange(desc(Gain)) %>%
      slice_head(n = n_top) %>%
      mutate(Feature = forcats::fct_reorder(Feature, Gain))

    ggplot(top, aes(x = Gain, y = Feature)) +
      geom_col(fill = "steelblue", alpha = 0.85) +
      labs(title = title,
           x = "Gain", y = NULL) +
      theme_minimal(base_size = 9) +
      theme(plot.title = element_text(face = "bold", size = 9))
  }

  p_base <- make_importance_plot(res$baseline$importance,
                                  title = "Baseline")
  p_eof  <- make_importance_plot(res$eof$importance,
                                  title = "EOF")

  auc_base <- round(res$baseline$auc_mean, 4)
  auc_eof  <- round(res$eof$auc_mean,      4)
  delta    <- round(auc_eof - auc_base,    4)

  p_combined <- (p_base | p_eof) +
    plot_annotation(
      title    = paste0(sp_label),
      subtitle =       paste0(
        "AUC baseline = ", auc_base,
        "  |  AUC EOF = ", auc_eof,
        "  |  dAUC = ", delta
      ),
      theme = theme(
        plot.title    = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(size = 9, color = "grey40")
      )
    )

  fname <- file.path(out_dir, paste0("importance_", sp_slug, ".pdf"))
  ggsave(fname, plot = p_combined,
         width = 14, height = 7, device = "pdf")
  cat("Sauvé :", fname, "\n")
}