# =============================================================================
#  Comparaison GAM mgcv (référence) vs GAM RTMB (reproduction, lambda fixés)
#  Alignement du prédicteur linéaire eta, corrélation, R², déviance expliquée,
#  coloré par sexe observé
#  Sortie : /figures/results/validation_rtmb/<espece>/
# =============================================================================

library(mgcv)
library(dplyr)
library(ggplot2)
library(purrr)

base_dir_bam  <- "/home/mathis/NorthSeaSR_Estimation/SR_Estimation/stat_analysis_2026/data/results_rtmb/detrend/bam_null_models"
base_dir_rtmb <- "/home/mathis/NorthSeaSR_Estimation/SR_Estimation/stat_analysis_2026/data/results_rtmb/detrend/rtmb_null_models"
out_root <- "/home/mathis/NorthSeaSR_Estimation/SR_Estimation/rapport/rapport_final/figures/results/validation_rtmb"

load_bam_ref <- function(sp, base_dir) {
  sp_dir <- file.path(base_dir, sp)
  rds_files <- list.files(sp_dir, pattern = "^bam_ref_.*\\.rds$", full.names = TRUE)
  if (length(rds_files) == 0) return(NULL)
  if (length(rds_files) > 1) {
    info <- file.info(rds_files)
    rds_file <- rds_files[which.max(info$mtime)]
  } else {
    rds_file <- rds_files[1]
  }
  readRDS(rds_file)
}

load_rtmb_null <- function(sp, base_dir) {
  sp_dir <- file.path(base_dir, sp)
  rds_files <- list.files(sp_dir, pattern = "^null_model_.*\\.rds$", full.names = TRUE)
  if (length(rds_files) == 0) return(NULL)
  if (length(rds_files) > 1) {
    info <- file.info(rds_files)
    rds_file <- rds_files[which.max(info$mtime)]
  } else {
    rds_file <- rds_files[1]
  }
  readRDS(rds_file)
}

compute_deviance_explained <- function(y, p_hat) {
  eps <- 1e-10
  p_hat <- pmin(pmax(p_hat, eps), 1 - eps)
  
  dev_model <- -2 * sum(y * log(p_hat) + (1 - y) * log(1 - p_hat))
  
  p_null <- mean(y)
  p_null <- pmin(pmax(p_null, eps), 1 - eps)
  dev_null <- -2 * sum(y * log(p_null) + (1 - y) * log(1 - p_null))
  
  1 - dev_model / dev_null
}

species_all <- basename(list.dirs(base_dir_bam, recursive = FALSE))

comparison_summary <- list()

for (sp in species_all) {

  cat("\n==========", sp, "==========\n")

  bam_ref <- load_bam_ref(sp, base_dir_bam)
  rtmb_cache <- load_rtmb_null(sp, base_dir_rtmb)

  if (is.null(bam_ref) || is.null(rtmb_cache)) {
    cat("  Modèle manquant (bam ou RTMB) - ignoré.\n")
    next
  }

  sp_slug <- gsub(" ", "_", sp)
  out_dir <- file.path(out_root, sp_slug)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  # ---- Prédicteurs linéaires ----
  eta_bam  <- bam_ref$linear.predictors
  eta_rtmb <- rtmb_cache$rep_null$eta

  if (length(eta_bam) != length(eta_rtmb)) {
    cat("  ATTENTION : longueurs différentes (bam:", length(eta_bam), ", RTMB:", length(eta_rtmb), ") - ignoré.\n")
    next
  }

  # ---- Corrélation et R² ----
  cor_val <- cor(eta_bam, eta_rtmb)
  r2_val  <- cor_val^2

  # ---- Déviance expliquée mgcv ----
  dev_expl_bam <- summary(bam_ref)$dev.expl * 100

  # ---- Sexe observé et probabilités prédites RTMB ----
  y_obs <- bam_ref$y
  if (is.null(y_obs) || length(y_obs) != length(eta_bam)) {
    y_obs <- rtmb_cache$data_sp_used$Numeric_sex
  }
  if (is.null(y_obs) || length(y_obs) != length(eta_bam)) {
    cat("  ATTENTION : sexe observé introuvable - graphe tracé sans couleur, déviance RTMB non calculable.\n")
    y_obs <- rep(NA, length(eta_bam))
    dev_expl_rtmb <- NA
  } else {
    p_hat_rtmb <- rtmb_cache$rep_null$p_hat
    if (is.null(p_hat_rtmb)) {
      p_hat_rtmb <- 1 / (1 + exp(-eta_rtmb))
    }
    dev_expl_rtmb <- compute_deviance_explained(y_obs, p_hat_rtmb) * 100
  }

  cat("  Corrélation eta_bam / eta_RTMB :", round(cor_val, 5), "\n")
  cat("  R² :", round(r2_val, 5), "\n")
  cat("  Dev. expliquée mgcv :", round(dev_expl_bam, 2), "% | RTMB :", round(dev_expl_rtmb, 2), "%\n")

  comparison_summary[[sp]] <- data.frame(
    species = sp, n = length(eta_bam), cor = cor_val, r2 = r2_val,
    dev_expl_mgcv = dev_expl_bam, dev_expl_rtmb = dev_expl_rtmb
  )

  # ---- Graphe d'alignement, coloré par sexe observé ----
  comp_df <- data.frame(eta_bam = eta_bam, eta_rtmb = eta_rtmb, sex = y_obs)
  comp_df$sex_label <- factor(comp_df$sex, levels = c(0, 1), labels = c("Femelle", "Mâle"))

  p_align <- ggplot(comp_df, aes(x = eta_bam, y = eta_rtmb, colour = sex_label)) +
    geom_point(alpha = 0.15, size = 0.4) +
    geom_abline(intercept = 0, slope = 1, colour = "black", linetype = "dashed") +
    scale_colour_manual(name = "Sexe observé",
                         values = c("Femelle" = "#F4A4B0", "Mâle" = "#89C4E1")) +
    guides(colour = guide_legend(override.aes = list(alpha = 1, size = 2))) +
    labs(subtitle = paste0("R² = ", round(r2_val, 5)),
         x = expression(eta[mgcv]~"(log-odds)"),
         y = expression(eta[RTMB]~"(log-odds)")) +
    theme_minimal(base_size = 10) +
    coord_fixed()

  ggsave(file.path(out_dir, "01_alignement_eta.pdf"), plot = p_align, width = 14, height = 10, units = "cm")

  cat("  Done →", out_dir, "\n")
}

# ---- Table récapitulative pour toutes les espèces ----
comparison_table <- bind_rows(comparison_summary)
comparison_table <- comparison_table[order(-comparison_table$n), ]
print(comparison_table)
