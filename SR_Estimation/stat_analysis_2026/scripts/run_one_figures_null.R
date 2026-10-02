# ================================================================
# run_one_figures_null.R
# Trace les 6 figures du modele NUL RTMB pour UNE espece.
# Usage : Rscript run_one_figures_null.R "<espece>"
# ================================================================

args <- commandArgs(trailingOnly = TRUE)
sp <- args[1]

library(mgcv)
library(ggplot2)
library(patchwork)
library(rnaturalearth)
library(sf)
library(rprojroot)

proj_root <- find_root(has_file(".git") | is_rstudio_project)
setwd(file.path(proj_root, "SR_Estimation", "stat_analysis_2026", "data"))

base_dir_bam <- "/home/mathis/NorthSeaSR_Estimation/SR_Estimation/stat_analysis_2026/data/results_rtmb/detrend/bam_null_models"
out_root_sdreport <- "/home/mathis/NorthSeaSR_Estimation/SR_Estimation/stat_analysis_2026/data/results_rtmb/rtmb_null_sdreport"
out_root_fig <- "/home/mathis/NorthSeaSR_Estimation/SR_Estimation/rapport/rapport_final/figures/results/gratia_rtmb_null"

pal_low  <- "#F4A4B0"
pal_mid  <- "#808080"
pal_high <- "#89C4E1"

sf_use_s2(FALSE)
coast <- ne_countries(scale = "medium", returnclass = "sf")

cohort_years_target <- c(1995, 2005, 2015)

# =============================================================================
#  Fonctions utilitaires (reprises et adaptees du pipeline thermique)
# =============================================================================

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

get_term_indices <- function(bam_ref) {
  smooth_info <- bam_ref$smooth
  re_idx <- which(sapply(smooth_info, function(s) {
    length(s$term) >= 1 && s$term[1] == "Cohort_fact" && inherits(s, "random.effect")
  }))
  cols_re <- if (length(re_idx) > 0) {
    unlist(lapply(re_idx, function(i) smooth_info[[i]]$first.para:smooth_info[[i]]$last.para))
  } else integer(0)
  cols_fixed <- setdiff(seq_len(length(coef(bam_ref))), cols_re)

  term_indices <- list()
  for (i in seq_along(smooth_info)) {
    if (i %in% re_idx) next
    s <- smooth_info[[i]]
    cols_abs <- s$first.para:s$last.para
    cols_rel <- match(cols_abs, cols_fixed)
    stopifnot(!anyNA(cols_rel))
    label <- if (inherits(s, "tensor.smooth")) "tenseur"
             else if (inherits(s, "sos.smooth")) "spatial"
             else if (inherits(s, "cyclic.smooth")) "saisonnier"
             else paste(s$term, collapse = ",")
    term_indices[[label]] <- cols_rel
  }
  list(cols_fixed = cols_fixed, term_indices = term_indices)
}

get_cohort_id_correspondance <- function(data_full, data_sp) {
  correspondance <- unique(data.frame(
    cohort_id = data_full$cohort_id,
    Cohort_fact = as.character(data_sp$Cohort_fact)
  ))
  correspondance[order(correspondance$cohort_id), ]
}

# =============================================================================
#  1. Tenseur, 3 cohortes
# =============================================================================

plot_tenseur_3cohortes <- function(sp, bam_ref, cols_fixed, idx_tenseur,
                                    beta_fixed_rtmb, data_sp, data_sp_full,
                                    cohort_years_target) {
  age_lm  <- lm(Age ~ as.numeric(Age_sc), data = data.frame(Age = data_sp_full$Age, Age_sc = data_sp$Age_sc))
  lngt_lm <- lm(Lngt ~ as.numeric(LngtClassGrouped_sc), data = data.frame(Lngt = data_sp_full$LngtClassGrouped, LngtClassGrouped_sc = data_sp$LngtClassGrouped_sc))
  sc_to_real_age  <- function(x) { co <- coef(age_lm);  co[1] + co[2] * as.numeric(x) }
  sc_to_real_lngt <- function(x) { co <- coef(lngt_lm); co[1] + co[2] * as.numeric(x) }

  cohorts_annees <- as.numeric(as.character(levels(data_sp$Cohort_fact)))
  available_targets <- cohort_years_target[cohort_years_target %in% cohorts_annees]

  cohort_lookup <- unique(data.frame(Cohort_fact = data_sp$Cohort_fact, Cohort_num_sc = data_sp$Cohort_num_sc))
  get_cohort_sc <- function(year) {
    cohort_lookup$Cohort_num_sc[as.character(cohort_lookup$Cohort_fact) == as.character(as.integer(year))][1]
  }

  Xp_obs <- predict(bam_ref, type = "lpmatrix")
  Xp_obs_fixed <- Xp_obs[, cols_fixed, drop = FALSE]
  Xp_tenseur_obs <- Xp_obs_fixed[, idx_tenseur, drop = FALSE]
  beta_tenseur <- beta_fixed_rtmb[idx_tenseur]
  f_obs_mean <- mean(as.vector(Xp_tenseur_obs %*% beta_tenseur))

  compute_one <- function(cohort_value_sc, year_label) {
    age_seq  <- seq(min(data_sp$Age_sc), max(data_sp$Age_sc), length.out = 60)
    lngt_seq <- seq(min(data_sp$LngtClassGrouped_sc), max(data_sp$LngtClassGrouped_sc), length.out = 60)
    newdata <- expand.grid(Age_sc = age_seq, LngtClassGrouped_sc = lngt_seq)
    newdata$Cohort_num_sc <- cohort_value_sc
    newdata$Latitude    <- median(data_sp$Latitude, na.rm = TRUE)
    newdata$Longitude   <- median(data_sp$Longitude, na.rm = TRUE)
    if ("julian_day" %in% names(data_sp)) newdata$julian_day  <- median(data_sp$julian_day, na.rm = TRUE)
    newdata$Cohort_fact <- factor(levels(data_sp$Cohort_fact)[1], levels = levels(data_sp$Cohort_fact))

    Xp_grid <- predict(bam_ref, newdata = newdata, type = "lpmatrix")
    Xp_tenseur_grid <- Xp_grid[, cols_fixed, drop = FALSE][, idx_tenseur, drop = FALSE]
    f_grid <- as.vector(Xp_tenseur_grid %*% beta_tenseur) - f_obs_mean

    newdata$Age_real  <- sc_to_real_age(newdata$Age_sc)
    newdata$Lngt_real <- sc_to_real_lngt(newdata$LngtClassGrouped_sc)
    newdata$estimate  <- f_grid
    newdata$year_label <- year_label
    newdata
  }

  df_list <- lapply(available_targets, function(yr) compute_one(get_cohort_sc(yr), yr))
  max_abs_common <- max(sapply(df_list, function(d) max(abs(d$estimate), na.rm = TRUE)))

  obs_points_list <- lapply(available_targets, function(yr) {
    idx <- as.character(data_sp$Cohort_fact) == as.character(as.integer(yr))
    data.frame(Age_real = data_sp_full$Age[idx], Lngt_real = data_sp_full$LngtClassGrouped[idx], year_label = yr)
  })

  plots <- Map(function(d, obs, yr) {
    ggplot(d, aes(x = Age_real, y = Lngt_real, fill = estimate)) +
      geom_raster() +
      geom_point(data = obs, aes(x = Age_real, y = Lngt_real), inherit.aes = FALSE,
                 shape = 21, colour = "black", fill = "white", size = 0.6, alpha = 0.5, stroke = 0.2) +
      scale_fill_gradient2(name = "log-odds", low = pal_low, mid = pal_mid, high = pal_high,
                            midpoint = 0, limits = c(-max_abs_common, max_abs_common)) +
      labs(title = paste("Cohorte", yr), x = "Âge", y = "Longueur (mm)") +
      theme_minimal(base_size = 9) +
      coord_fixed(ratio = diff(range(d$Age_real)) / diff(range(d$Lngt_real)))
  }, df_list, obs_points_list, available_targets)

  wrap_plots(plots, ncol = length(plots)) + plot_layout(guides = "collect") & theme(legend.position = "right")
}

# =============================================================================
#  2. Effet spatial
# =============================================================================

plot_spatial_partiel <- function(sp, bam_ref, cols_fixed, idx_spatial,
                                  beta_fixed_rtmb, data_sp, coast) {
  lon_seq <- seq(min(data_sp$Longitude), max(data_sp$Longitude), length.out = 80)
  lat_seq <- seq(min(data_sp$Latitude), max(data_sp$Latitude), length.out = 80)

  newdata <- expand.grid(Longitude = lon_seq, Latitude = lat_seq)
  newdata$Age_sc <- median(data_sp$Age_sc, na.rm = TRUE)
  newdata$LngtClassGrouped_sc <- median(data_sp$LngtClassGrouped_sc, na.rm = TRUE)
  newdata$Cohort_num_sc <- median(data_sp$Cohort_num_sc, na.rm = TRUE)
  if ("julian_day" %in% names(data_sp)) newdata$julian_day <- median(data_sp$julian_day, na.rm = TRUE)
  newdata$Cohort_fact <- factor(levels(data_sp$Cohort_fact)[1], levels = levels(data_sp$Cohort_fact))

  Xp_grid <- predict(bam_ref, newdata = newdata, type = "lpmatrix")
  Xp_spatial_grid <- Xp_grid[, cols_fixed, drop = FALSE][, idx_spatial, drop = FALSE]
  beta_spatial <- beta_fixed_rtmb[idx_spatial]
  f_grid <- as.vector(Xp_spatial_grid %*% beta_spatial)

  Xp_obs <- predict(bam_ref, type = "lpmatrix")
  Xp_spatial_obs <- Xp_obs[, cols_fixed, drop = FALSE][, idx_spatial, drop = FALSE]
  f_obs_mean <- mean(as.vector(Xp_spatial_obs %*% beta_spatial))

  newdata$estimate <- f_grid - f_obs_mean
  max_abs <- max(abs(newdata$estimate), na.rm = TRUE)

  bbox_sp <- st_bbox(c(xmin = min(lon_seq), xmax = max(lon_seq),
                        ymin = min(lat_seq), ymax = max(lat_seq)), crs = st_crs(4326))
  coast_crop <- suppressWarnings(st_crop(coast, bbox_sp))

  ggplot() +
    geom_raster(data = newdata, aes(x = Longitude, y = Latitude, fill = estimate)) +
    geom_sf(data = coast_crop, fill = "grey40", color = NA, inherit.aes = FALSE) +
    scale_fill_gradient2(name = "Log-odds", low = pal_low, mid = pal_mid, high = pal_high,
                          midpoint = 0, limits = c(-max_abs, max_abs)) +
    coord_sf(xlim = c(min(lon_seq), max(lon_seq)), ylim = c(min(lat_seq), max(lat_seq)), expand = FALSE) +
    labs(title = paste("Effet spatial -", sp), x = "Longitude", y = "Latitude") +
    theme_minimal(base_size = 9)
}

# =============================================================================
#  3. Effet saisonnier
# =============================================================================

plot_saisonnier_gratia_style <- function(sp, bam_ref, cols_fixed, idx_saison,
                                          beta_fixed_rtmb, cov_beta_fixed, data_sp) {
  jday_seq <- seq(1, 365, length.out = 100)
  newdata <- data.frame(julian_day = jday_seq)
  newdata$Age_sc <- median(data_sp$Age_sc, na.rm = TRUE)
  newdata$LngtClassGrouped_sc <- median(data_sp$LngtClassGrouped_sc, na.rm = TRUE)
  newdata$Cohort_num_sc <- median(data_sp$Cohort_num_sc, na.rm = TRUE)
  newdata$Latitude <- median(data_sp$Latitude, na.rm = TRUE)
  newdata$Longitude <- median(data_sp$Longitude, na.rm = TRUE)
  newdata$Cohort_fact <- factor(levels(data_sp$Cohort_fact)[1], levels = levels(data_sp$Cohort_fact))

  Xp_grid <- predict(bam_ref, newdata = newdata, type = "lpmatrix")
  Xp_saison_grid <- Xp_grid[, cols_fixed, drop = FALSE][, idx_saison, drop = FALSE]
  beta_saison <- beta_fixed_rtmb[idx_saison]
  f_grid <- as.vector(Xp_saison_grid %*% beta_saison)

  Xp_obs <- predict(bam_ref, type = "lpmatrix")
  Xp_saison_obs <- Xp_obs[, cols_fixed, drop = FALSE][, idx_saison, drop = FALSE]
  f_obs_mean <- mean(as.vector(Xp_saison_obs %*% beta_saison))

  newdata$estimate <- f_grid - f_obs_mean

  if (!is.null(cov_beta_fixed)) {
    cov_saison <- cov_beta_fixed[idx_saison, idx_saison, drop = FALSE]
    var_pointwise <- rowSums((Xp_saison_grid %*% cov_saison) * Xp_saison_grid)
    newdata$se <- sqrt(pmax(var_pointwise, 0))
  } else {
    newdata$se <- NA
  }

  rug_data <- data.frame(julian_day = data_sp$julian_day)

  p <- ggplot(newdata, aes(x = julian_day, y = estimate))
  if (!all(is.na(newdata$se))) {
    p <- p + geom_ribbon(aes(ymin = estimate - 1.96 * se, ymax = estimate + 1.96 * se),
                          fill = "grey70", alpha = 0.4)
  }
  p <- p +
    geom_rug(data = rug_data, aes(x = julian_day), inherit.aes = FALSE, sides = "b", alpha = 0.1) +
    geom_line(colour = "black", linewidth = 0.7) +
    labs(title = paste("Effet saisonnier -", sp),
         x = "Jour julien", y = "Log-odds") +
    theme_minimal(base_size = 9)
  p
}

# =============================================================================
#  4. QQ-plot des effets aleatoires b_cohort
# =============================================================================

plot_qqplot_re <- function(sp, b_cohort, sigma_cohort) {
  b_scaled <- b_cohort / sigma_cohort
  df <- data.frame(
    theorique  = sort(qnorm(ppoints(length(b_scaled)))),
    empirique  = sort(b_scaled)
  )
  ggplot(df, aes(x = theorique, y = empirique)) +
    geom_point(size = 1.2, colour = "black") +
    geom_abline(intercept = 0, slope = 1, colour = "black", linetype = "dashed") +
    labs(title = paste("QQ-plot -", sp),
         x = "Quantiles théoriques", y = "Quantiles empiriques") +
    theme_minimal(base_size = 9)
}

# =============================================================================
#  5. Effet cohorte du tenseur (AME + ruban)
# =============================================================================

plot_tenseur_AME_CI <- function(sp, bam_ref, cols_fixed, idx_tenseur,
                                 beta_fixed_rtmb, cov_beta_fixed,
                                 data_sp, data_full) {
  correspondance <- get_cohort_id_correspondance(data_full, data_sp)
  cohorts_years_all <- as.numeric(correspondance$Cohort_fact)

  cohort_lookup_sc <- unique(data.frame(Cohort_fact = data_sp$Cohort_fact, Cohort_num_sc = data_sp$Cohort_num_sc))
  cohort_lookup_sc$Cohort_fact_num <- as.numeric(as.character(cohort_lookup_sc$Cohort_fact))
  match_idx <- match(cohorts_years_all, cohort_lookup_sc$Cohort_fact_num)
  cohorts_sc_all <- cohort_lookup_sc$Cohort_num_sc[match_idx]

  beta_tenseur <- beta_fixed_rtmb[idx_tenseur]
  cov_tenseur <- if (!is.null(cov_beta_fixed)) cov_beta_fixed[idx_tenseur, idx_tenseur, drop = FALSE] else NULL

  compute_tenseur_ame_se <- function(cohort_value_sc) {
    newdata <- data.frame(
      Age_sc = data_sp$Age_sc,
      LngtClassGrouped_sc = data_sp$LngtClassGrouped_sc
    )
    newdata$Cohort_num_sc <- cohort_value_sc
    newdata$Latitude    <- median(data_sp$Latitude, na.rm = TRUE)
    newdata$Longitude   <- median(data_sp$Longitude, na.rm = TRUE)
    if ("julian_day" %in% names(data_sp)) newdata$julian_day <- median(data_sp$julian_day, na.rm = TRUE)
    newdata$Cohort_fact <- factor(levels(data_sp$Cohort_fact)[1], levels = levels(data_sp$Cohort_fact))

    Xp <- predict(bam_ref, newdata = newdata, type = "lpmatrix")
    Xp_tenseur <- Xp[, cols_fixed, drop = FALSE][, idx_tenseur, drop = FALSE]
    Xbar <- colMeans(Xp_tenseur)

    ame <- as.numeric(Xbar %*% beta_tenseur)
    se  <- if (!is.null(cov_tenseur)) sqrt(as.numeric(t(Xbar) %*% cov_tenseur %*% Xbar)) else NA
    c(ame = ame, se = se)
  }

  res <- sapply(cohorts_sc_all, compute_tenseur_ame_se)
  ame <- res["ame", ] - mean(res["ame", ])
  se  <- res["se", ]

  df <- data.frame(year = cohorts_years_all, ame = ame, se = se)
  df <- df[order(df$year), ]

  ggplot(df, aes(x = year, y = ame)) +
    geom_ribbon(aes(ymin = ame - 1.96 * se, ymax = ame + 1.96 * se), fill = "#2166AC", alpha = 0.2) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
    geom_line(colour = "#2166AC", linewidth = 0.8) +
    labs(title = paste("Effet cohorte du tenseur -", sp),
         x = "Cohorte", y = "Log-odds") +
    theme_minimal(base_size = 9)
}

# =============================================================================
#  6. Effets aleatoires realises, IC individuel, sigma en sous-titre
# =============================================================================

plot_re_realized_with_CI <- function(sp, data_sp, data_full,
                                      b_cohort, se_b_cohort, sigma_cohort) {
  correspondance <- get_cohort_id_correspondance(data_full, data_sp)
  cohorts_years_all <- as.numeric(correspondance$Cohort_fact)

  df <- data.frame(year = cohorts_years_all, b = b_cohort, se = se_b_cohort)
  df <- df[order(df$year), ]

  ggplot(df, aes(x = year, y = b)) +
    geom_errorbar(aes(ymin = b - 1.96 * se, ymax = b + 1.96 * se),
                  width = 0.3, colour = "grey30", alpha = 0.6) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
    geom_point(shape = 18, colour = "black", size = 2.5) +
    labs(title = paste("Effets aléatoires réalisés -", sp),
         subtitle = bquote(sigma[cohort] == .(round(sigma_cohort, 4))),
         x = "Cohorte", y = "Log-odds") +
    theme_minimal(base_size = 9)
}

# =============================================================================
#  Traitement pour l'espece sp
# =============================================================================

cat("\n==========", sp, "==========\n")

bam_ref <- load_bam_ref(sp, base_dir_bam)
if (is.null(bam_ref)) { cat("  bam_ref manquant.\n"); quit(status = 1) }

sp_slug <- gsub(" ", "_", sp)
r_null <- readRDS(file.path(out_root_sdreport, sp_slug, paste0("sdreport_null_", sp_slug, ".rds")))

data_sp <- r_null$data_sp_used
data_full <- r_null$data_null_used

struct <- get_term_indices(bam_ref)
cols_fixed <- struct$cols_fixed
idx_tenseur <- struct$term_indices[["tenseur"]]
idx_spatial <- struct$term_indices[["spatial"]]
idx_saison  <- struct$term_indices[["saisonnier"]]  # NULL pour P. virens

op <- r_null$last_par_best
beta_fixed_rtmb <- as.numeric(op[names(op) == "beta_fixed"])
b_cohort <- as.numeric(op[names(op) == "b_cohort"])  # vide pour P. virens
sigma_cohort <- exp(as.numeric(op[names(op) == "log_sigma_cohort"]))

sd_obj <- r_null$sdreport_full
cov_beta_fixed <- tryCatch({
  idx_bf <- which(names(sd_obj$par.fixed) == "beta_fixed")
  sd_obj$cov.fixed[idx_bf, idx_bf]
}, error = function(e) NULL)

se_b_cohort <- if (!is.null(sd_obj) && !is.null(sd_obj$diag.cov.random)) {
  sqrt(sd_obj$diag.cov.random)
} else {
  rep(NA_real_, length(b_cohort))
}

# ---- Recuperation de data_sp_full pour les axes reels (Age, LngtClassGrouped) ----
load("temp_v4.RData")
data_sp_full <- data_expanded_1991_2023 %>%
  dplyr::filter(Species == sp, !is.na(Numeric_sex)) %>% droplevels()

out_dir <- file.path(out_root_fig, sp_slug)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

has_saison <- !is.null(idx_saison) && length(idx_saison) > 0
has_re     <- length(b_cohort) > 0

# ---- 1. Tenseur ----
p1 <- tryCatch(
  plot_tenseur_3cohortes(sp, bam_ref, cols_fixed, idx_tenseur, beta_fixed_rtmb,
                          data_sp, data_sp_full, cohort_years_target),
  error = function(e) { cat("  Echec 1 :", conditionMessage(e), "\n"); NULL }
)
if (!is.null(p1)) ggsave(file.path(out_dir, "01_tenseur.pdf"), plot = p1, width = 24, height = 8, units = "cm")

# ---- 2. Spatial ----
p2 <- tryCatch(
  plot_spatial_partiel(sp, bam_ref, cols_fixed, idx_spatial, beta_fixed_rtmb, data_sp, coast),
  error = function(e) { cat("  Echec 2 :", conditionMessage(e), "\n"); NULL }
)
if (!is.null(p2)) ggsave(file.path(out_dir, "02_spatial.pdf"), plot = p2, width = 12, height = 10, units = "cm")

# ---- 3. Saisonnier (sauf P. virens) ----
if (has_saison) {
  p3 <- tryCatch(
    plot_saisonnier_gratia_style(sp, bam_ref, cols_fixed, idx_saison, beta_fixed_rtmb, cov_beta_fixed, data_sp),
    error = function(e) { cat("  Echec 3 :", conditionMessage(e), "\n"); NULL }
  )
  if (!is.null(p3)) ggsave(file.path(out_dir, "03_saisonnier.pdf"), plot = p3, width = 12, height = 8, units = "cm")
} else {
  cat("  Pas de terme saisonnier pour", sp, "- graphe 3 ignore.\n")
}

# ---- 4. QQ-plot RE (sauf P. virens) ----
if (has_re) {
  p4 <- tryCatch(
    plot_qqplot_re(sp, b_cohort, sigma_cohort),
    error = function(e) { cat("  Echec 4 :", conditionMessage(e), "\n"); NULL }
  )
  if (!is.null(p4)) ggsave(file.path(out_dir, "04_qqplot_re.pdf"), plot = p4, width = 10, height = 8, units = "cm")
} else {
  cat("  Pas d'effet aleatoire pour", sp, "- graphe 4 ignore.\n")
}

# ---- 5. Effet cohorte du tenseur (AME + ruban) ----
p5 <- tryCatch(
  plot_tenseur_AME_CI(sp, bam_ref, cols_fixed, idx_tenseur, beta_fixed_rtmb, cov_beta_fixed, data_sp, data_full),
  error = function(e) { cat("  Echec 5 :", conditionMessage(e), "\n"); NULL }
)
if (!is.null(p5)) ggsave(file.path(out_dir, "05_tenseur_AME.pdf"), plot = p5, width = 14, height = 8, units = "cm")

# ---- 6. Effets aleatoires realises (sauf P. virens) ----
if (has_re) {
  p6 <- tryCatch(
    plot_re_realized_with_CI(sp, data_sp, data_full, b_cohort, se_b_cohort, sigma_cohort),
    error = function(e) { cat("  Echec 6 :", conditionMessage(e), "\n"); NULL }
  )
  if (!is.null(p6)) ggsave(file.path(out_dir, "06_re_realized_CI.pdf"), plot = p6, width = 14, height = 8, units = "cm")
} else {
  cat("  Pas d'effet aleatoire pour", sp, "- graphe 6 ignore.\n")
}

cat("  Done ->", out_dir, "\n")
