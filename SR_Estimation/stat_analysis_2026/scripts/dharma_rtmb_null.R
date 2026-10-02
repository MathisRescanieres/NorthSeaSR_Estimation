library(DHARMa)
library(mgcv)
library(rprojroot)

proj_root <- find_root(has_file(".git") | is_rstudio_project)
setwd(file.path(proj_root, "SR_Estimation", "stat_analysis_2026", "data"))

rtmb_null_dir <- "/home/mathis/NorthSeaSR_Estimation/SR_Estimation/stat_analysis_2026/data/results_rtmb/detrend/rtmb_null_models"
base_dir_bam  <- "/home/mathis/NorthSeaSR_Estimation/SR_Estimation/stat_analysis_2026/data/results_rtmb/detrend/bam_null_models"
out_root <- "/home/mathis/NorthSeaSR_Estimation/SR_Estimation/rapport/rapport_final/figures/results/dharma_rtmb_null"

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

species_all <- basename(list.dirs(rtmb_null_dir, recursive = FALSE))

for (sp in species_all) {
  
  cat("\n==========", sp, "==========\n")
  
  r_null <- load_rtmb_null(sp, rtmb_null_dir)
  if (is.null(r_null)) { cat("  Aucun modele trouve - ignore.\n"); next }
  
  sp_slug <- gsub(" ", "_", sp)
  out_dir <- file.path(out_root, sp_slug)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  # ---- Recuperation de y_obs, avec repli sur bam_ref si data_null_used absent ----
  y_obs <- r_null$data_null_used$y
  
  if (is.null(y_obs)) {
    cat("  data_null_used absent, repli sur bam_ref$y\n")
    bam_ref <- load_bam_ref(sp, base_dir_bam)
    if (is.null(bam_ref)) {
      cat("  bam_ref introuvable non plus - ignore.\n")
      next
    }
    y_obs <- bam_ref$y
  }
  
  p_hat <- r_null$rep_null$p_hat
  if (is.null(p_hat)) {
    eta <- r_null$rep_null$eta
    if (is.null(eta)) {
      cat("  Ni p_hat ni eta trouves - ignore.\n")
      next
    }
    p_hat <- 1 / (1 + exp(-eta))
  }
  
  if (is.null(y_obs) || length(y_obs) != length(p_hat)) {
    cat("  y_obs introuvable ou incoherent avec p_hat (longueurs :",
        length(y_obs), "vs", length(p_hat), ") - ignore.\n")
    next
  }
  
  n_sim <- 500
  n_obs <- length(y_obs)
  
  set.seed(42)
  sim_matrix <- matrix(rbinom(n_obs * n_sim, size = 1, prob = rep(p_hat, n_sim)),
                        nrow = n_obs, ncol = n_sim)
  
  dharma_obj <- createDHARMa(
    simulatedResponse = sim_matrix,
    observedResponse  = y_obs,
    fittedPredictedResponse = p_hat,
    integerResponse = TRUE
  )
  
  pdf(file.path(out_dir, "01_qqplot.pdf"), width = 8, height = 7)
  qqplot(
    ppoints(length(dharma_obj$scaledResiduals)),
    dharma_obj$scaledResiduals,
    xlab = "Quantiles théoriques (loi uniforme)",
    ylab = "Quantiles observés (résidus simulés)",
    main = paste("QQ-plot -", sp),
    pch  = 16, cex = 0.5, col = rgb(0, 0, 0, 0.3)
  )
  abline(0, 1, col = "red", lty = 2)
  dev.off()
  
  pdf(file.path(out_dir, "02_residuals_fitted.pdf"), width = 8, height = 7)
  plotResiduals(dharma_obj,
                xlab = "Probabilité prédite d'être mâle",
                ylab = "Résidu simulé (échelle uniforme)",
                main = paste("Résidus vs valeurs ajustées -", sp))
  dev.off()
  
  cat("  Done ->", out_dir, "\n")
}

cat("\nTermine.\n")
