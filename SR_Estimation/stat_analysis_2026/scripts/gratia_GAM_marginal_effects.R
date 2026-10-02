# =============================================================================
#  Effets estimés — pour chaque espèce : tenseur à 3 cohortes fixées,
#  effet spatial avec fond de carte, effet saisonnier, effet aléatoire cohorte
#  Échelle logit, axes en unités réelles (âge, longueur)
#  Modèles chargés depuis le cache bam_ref
#  Pollachius virens traité séparément (pas de Cohort_fact dans sa formule)
#  Sortie : /figures/results/gratia/<espece>/
# =============================================================================

library(mgcv)
library(gratia)
library(ggplot2)
library(patchwork)
library(rnaturalearth)
library(sf)
library(dplyr)
library(rprojroot)

# ---- Chargement des données sources (Age, LngtClassGrouped en unités réelles) ----
proj_root <- find_root(has_file(".git") | is_rstudio_project)
setwd(file.path(proj_root, "SR_Estimation", "stat_analysis_2026", "data"))
load("temp_v4.RData")

cat("Objets chargés depuis temp_v4.RData :\n")
print(ls())

base_dir <- "/home/mathis/NorthSeaSR_Estimation/SR_Estimation/stat_analysis_2026/data/results_rtmb/detrend/bam_null_models"
out_root <- "/home/mathis/NorthSeaSR_Estimation/SR_Estimation/rapport/rapport_final/figures/results/gratia"

cohort_years_target <- c(1995, 2005, 2015)

pal_low  <- "#F4A4B0"
pal_mid  <- "#808080"
pal_high <- "#89C4E1"

sf_use_s2(FALSE)
coast <- ne_countries(scale = "medium", returnclass = "sf")

# ---- Fonctions communes (réutilisées pour toutes les espèces) -------------

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
  cat("  Modèle utilisé :", basename(rds_file), "\n")
  readRDS(rds_file)
}

compute_surface_at_cohort <- function(model, te_label, data_model,
                                       cohort_value_sc, year_label,
                                       age_lm, lngt_lm) {
  lat_ref  <- if ("Latitude" %in% names(data_model)) median(data_model$Latitude, na.rm = TRUE) else NA
  lon_ref  <- if ("Longitude" %in% names(data_model)) median(data_model$Longitude, na.rm = TRUE) else NA
  jday_ref <- if ("julian_day" %in% names(data_model)) median(data_model$julian_day, na.rm = TRUE) else NA

  newdata <- expand.grid(
    Age_sc = seq(min(data_model$Age_sc), max(data_model$Age_sc), length.out = 60),
    LngtClassGrouped_sc = seq(min(data_model$LngtClassGrouped_sc), max(data_model$LngtClassGrouped_sc), length.out = 60)
  )
  newdata$Cohort_num_sc <- cohort_value_sc
  if ("Latitude" %in% names(data_model))  newdata$Latitude  <- lat_ref
  if ("Longitude" %in% names(data_model)) newdata$Longitude <- lon_ref
  if ("julian_day" %in% names(data_model)) newdata$julian_day <- jday_ref
  if ("Cohort_fact" %in% names(data_model)) {
    newdata$Cohort_fact <- factor(levels(data_model$Cohort_fact)[1], levels = levels(data_model$Cohort_fact))
  }

  sm <- smooth_estimates(model, smooth = te_label, data = newdata)

  coefs_age  <- coef(age_lm)
  coefs_lngt <- coef(lngt_lm)
  sm$Age_real  <- coefs_age[1]  + coefs_age[2]  * as.numeric(sm$Age_sc)
  sm$Lngt_real <- coefs_lngt[1] + coefs_lngt[2] * as.numeric(sm$LngtClassGrouped_sc)
  sm$year_label <- year_label
  sm
}

plot_surface_from_sm <- function(sm, year_label, max_abs, obs_points,
                                  pal_low, pal_mid, pal_high) {
  ggplot(sm, aes(x = Age_real, y = Lngt_real, fill = .estimate)) +
    geom_raster() +
    geom_point(data = obs_points, aes(x = Age_real, y = Lngt_real),
               inherit.aes = FALSE, shape = 21, colour = "black", fill = "white",
               size = 0.6, alpha = 0.5, stroke = 0.2) +
    scale_fill_gradient2(name = "Log-odds", low = pal_low, mid = pal_mid,
                          high = pal_high, midpoint = 0,
                          limits = c(-max_abs, max_abs)) +
    labs(title = paste("Cohorte", year_label), x = "Âge", y = "Longueur (mm)") +
    theme_minimal(base_size = 9) +
    coord_fixed(ratio = diff(range(sm$Age_real)) / diff(range(sm$Lngt_real)))
}

plot_spatial_effect <- function(bam_ref, spatial_label, data_model, sp,
                                 coast, pal_low, pal_mid, pal_high) {
  lon_seq <- seq(min(data_model$Longitude), max(data_model$Longitude), length.out = 80)
  lat_seq <- seq(min(data_model$Latitude), max(data_model$Latitude), length.out = 80)

  newdata_sp <- expand.grid(Longitude = lon_seq, Latitude = lat_seq)
  if ("Age_sc" %in% names(data_model)) newdata_sp$Age_sc <- median(data_model$Age_sc, na.rm = TRUE)
  if ("LngtClassGrouped_sc" %in% names(data_model)) newdata_sp$LngtClassGrouped_sc <- median(data_model$LngtClassGrouped_sc, na.rm = TRUE)
  if ("Cohort_num_sc" %in% names(data_model)) newdata_sp$Cohort_num_sc <- median(data_model$Cohort_num_sc, na.rm = TRUE)
  if ("julian_day" %in% names(data_model)) newdata_sp$julian_day <- median(data_model$julian_day, na.rm = TRUE)
  if ("Cohort_fact" %in% names(data_model)) newdata_sp$Cohort_fact <- factor(levels(data_model$Cohort_fact)[1], levels = levels(data_model$Cohort_fact))

  sm_sp <- tryCatch(
    smooth_estimates(bam_ref, smooth = spatial_label, data = newdata_sp),
    error = function(e) { cat("  Échec effet spatial :", conditionMessage(e), "\n"); NULL }
  )

  if (is.null(sm_sp)) return(NULL)

  max_abs_sp <- max(abs(sm_sp$.estimate), na.rm = TRUE)

  bbox_sp <- st_bbox(c(xmin = min(lon_seq), xmax = max(lon_seq),
                        ymin = min(lat_seq), ymax = max(lat_seq)), crs = st_crs(4326))
  coast_crop <- suppressWarnings(st_crop(coast, bbox_sp))

  ggplot() +
    geom_raster(data = sm_sp, aes(x = Longitude, y = Latitude, fill = .estimate)) +
    geom_sf(data = coast_crop, fill = "grey40", color = NA, inherit.aes = FALSE) +
    scale_fill_gradient2(name = "Log-odds", low = pal_low, mid = pal_mid,
                          high = pal_high, midpoint = 0,
                          limits = c(-max_abs_sp, max_abs_sp)) +
    coord_sf(xlim = c(min(lon_seq), max(lon_seq)), ylim = c(min(lat_seq), max(lat_seq)), expand = FALSE) +
    labs(title = paste("Effet spatial -", sp), x = "Longitude", y = "Latitude") +
    theme_minimal(base_size = 9)
}

# =============================================================================
#  Boucle principale : toutes les espèces sauf Pollachius virens
# =============================================================================

species_all <- basename(list.dirs(base_dir, recursive = FALSE))
species_main <- setdiff(species_all, "Pollachius virens")

for (sp in species_main) {

  cat("\n==========", sp, "==========\n")

  bam_ref <- load_bam_ref(sp, base_dir)
  if (is.null(bam_ref)) { cat("  Aucun modèle trouvé - ignoré.\n"); next }

  sp_slug <- gsub(" ", "_", sp)
  out_dir <- file.path(out_root, sp_slug)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  data_model  <- model.frame(bam_ref)
  term_labels <- gratia::smooths(bam_ref)

  data_sp_full <- data_expanded_1991_2023 %>%
    dplyr::filter(Species == sp, !is.na(Numeric_sex)) %>%
    droplevels()

  cat("  n data_sp_full :", nrow(data_sp_full), "| n data_model :", nrow(data_model), "\n")

  has_age_real  <- "Age" %in% names(data_sp_full) && nrow(data_sp_full) == nrow(data_model)
  has_lngt_real <- "LngtClassGrouped" %in% names(data_sp_full) && nrow(data_sp_full) == nrow(data_model)

  # ---- 1. Tenseur âge-longueur-cohorte à 3 cohortes fixées ----
  te_label <- grep("te\\(Age_sc,LngtClassGrouped_sc,Cohort_num_sc\\)", term_labels, value = TRUE)

  if (length(te_label) > 0 && "Cohort_fact" %in% names(data_model) && has_age_real && has_lngt_real) {

    data_model$Age_real  <- data_sp_full$Age
    data_model$Lngt_real <- data_sp_full$LngtClassGrouped

    cohorts_annees <- as.numeric(as.character(levels(data_model$Cohort_fact)))
    available_targets <- cohort_years_target[cohort_years_target %in% cohorts_annees]

    if (length(available_targets) < 3) {
      cat("  Attention : seules", length(available_targets), "des 3 cohortes cibles disponibles pour", sp, "\n")
    }

    if (length(available_targets) > 0) {

      cohort_lookup <- unique(data.frame(
        Cohort_fact = data_model$Cohort_fact,
        Cohort_num_sc = data_model$Cohort_num_sc
      ))

      get_cohort_sc <- function(year) {
        match_val <- as.character(cohort_lookup$Cohort_fact) == as.character(as.integer(year))
        result <- unique(cohort_lookup$Cohort_num_sc[match_val])
        if (length(result) == 0) return(NA)
        result[1]
      }

      age_lm  <- lm(Age_real ~ as.numeric(Age_sc), data = data_model)
      lngt_lm <- lm(Lngt_real ~ as.numeric(LngtClassGrouped_sc), data = data_model)

      sm_list <- lapply(available_targets, function(yr) {
        compute_surface_at_cohort(bam_ref, te_label, data_model,
                                   get_cohort_sc(yr), yr, age_lm, lngt_lm)
      })

      max_abs_common <- max(sapply(sm_list, function(sm) max(abs(sm$.estimate), na.rm = TRUE)))

      plots_te <- Map(function(sm, yr) {
        obs_points <- data_model[as.character(data_model$Cohort_fact) == as.character(as.integer(yr)), ]
        plot_surface_from_sm(sm, yr, max_abs_common, obs_points, pal_low, pal_mid, pal_high)
      }, sm_list, available_targets)

      p_te_combined <- wrap_plots(plots_te, ncol = length(plots_te)) +
        plot_layout(guides = "collect") &
        theme(legend.position = "right")

      ggsave(file.path(out_dir, "01_tenseur_cohortes.pdf"),
             plot = p_te_combined, width = 8 * length(plots_te), height = 8, units = "cm")
    }
  } else {
    cat("  Terme tenseur, Cohort_fact ou variables réelles absentes pour", sp, "- étape 1 ignorée.\n")
  }

  # ---- 2. Effet spatial ----
  spatial_label <- grep("Latitude,Longitude", term_labels, value = TRUE)
  if (length(spatial_label) > 0) {
    p_spatial <- plot_spatial_effect(bam_ref, spatial_label, data_model, sp, coast, pal_low, pal_mid, pal_high)
    if (!is.null(p_spatial)) {
      ggsave(file.path(out_dir, "02_spatial.pdf"), plot = p_spatial, width = 12, height = 10, units = "cm")
    }
  } else {
    cat("  Terme spatial absent pour", sp, "- étape 2 ignorée.\n")
  }

  # ---- 3. Effet saisonnier ----
  seasonal_label <- grep("julian_day", term_labels, value = TRUE)
  if (length(seasonal_label) > 0) {
    p_seasonal <- tryCatch(
      gratia::draw(bam_ref, select = seasonal_label) &
        theme_minimal(base_size = 9) &
        labs(title = paste("Effet saisonnier -", sp),
            x = "Jour julien", y = "Log-odds"),
      error = function(e) { cat("  Échec effet saisonnier :", conditionMessage(e), "\n"); NULL }
    )
    if (!is.null(p_seasonal)) {
      ggsave(file.path(out_dir, "03_saisonnier.pdf"), plot = p_seasonal, width = 10, height = 8, units = "cm")
    }
  } else {
    cat("  Terme saisonnier absent pour", sp, "- étape 3 ignorée.\n")
  }

  # ---- 4. Effet aléatoire de cohorte ----
  re_label <- grep("Cohort_fact", term_labels, value = TRUE)
  re_label <- re_label[!re_label %in% te_label]
  if (length(re_label) > 0) {
    p_re <- tryCatch(
      gratia::draw(bam_ref, select = re_label) &
        theme_minimal(base_size = 9) &
        labs(title = paste("QQ-plot -", sp),
            x = "Quantiles théoriques", y = "Quantiles empiriques"),
      error = function(e) { cat("  Échec effet aléatoire :", conditionMessage(e), "\n"); NULL }
    )
    if (!is.null(p_re)) {
      ggsave(file.path(out_dir, "04_re_cohort.pdf"), plot = p_re, width = 10, height = 8, units = "cm")
    }
  } else {
    cat("  Effet aléatoire de cohorte absent pour", sp, "- étape 4 ignorée.\n")
  }

  cat("  Done →", out_dir, "\n")
}

# =============================================================================
#  Traitement séparé : Pollachius virens
#  (pas de Cohort_fact ni de julian_day dans sa formule retenue, mais le
#  tenseur âge-longueur-cohorte existe toujours ; on reconstruit les
#  cohortes réelles à partir de data_expanded_1991_2023)
# =============================================================================

sp <- "Pollachius virens"
cat("\n==========", sp, "(traitement séparé) ==========\n")

bam_ref <- load_bam_ref(sp, base_dir)

if (!is.null(bam_ref)) {

  sp_slug <- gsub(" ", "_", sp)
  out_dir <- file.path(out_root, sp_slug)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  data_model  <- model.frame(bam_ref)
  term_labels <- gratia::smooths(bam_ref)

  data_sp_full <- data_expanded_1991_2023 %>%
    dplyr::filter(Species == sp, !is.na(Numeric_sex)) %>%
    droplevels()

  cat("  n data_sp_full :", nrow(data_sp_full), "| n data_model :", nrow(data_model), "\n")

  te_label <- grep("te\\(Age_sc,LngtClassGrouped_sc,Cohort_num_sc\\)", term_labels, value = TRUE)

  has_age_real  <- "Age" %in% names(data_sp_full) && nrow(data_sp_full) == nrow(data_model)
  has_lngt_real <- "LngtClassGrouped" %in% names(data_sp_full) && nrow(data_sp_full) == nrow(data_model)
  has_cohort_info <- all(c("Cohort_fact", "Cohort_num_sc") %in% names(data_sp_full))

  if (length(te_label) > 0 && has_age_real && has_lngt_real && has_cohort_info) {

    # Reconstruction de l'information de cohorte depuis data_sp_full,
    # puisque Cohort_fact n'est pas dans model.frame(bam_ref) pour cette espèce
    data_model$Age_real   <- data_sp_full$Age
    data_model$Lngt_real  <- data_sp_full$LngtClassGrouped
    data_model$Cohort_fact_ext   <- droplevels(data_sp_full$Cohort_fact)
    data_model$Cohort_num_sc_ext <- data_sp_full$Cohort_num_sc

    cohorts_annees <- as.numeric(as.character(levels(data_model$Cohort_fact_ext)))
    available_targets <- cohort_years_target[cohort_years_target %in% cohorts_annees]

    if (length(available_targets) < 3) {
      cat("  Attention : seules", length(available_targets), "des 3 cohortes cibles disponibles pour", sp, "\n")
    }

    if (length(available_targets) > 0) {

      cohort_lookup <- unique(data.frame(
        Cohort_fact = data_model$Cohort_fact_ext,
        Cohort_num_sc = data_model$Cohort_num_sc_ext
      ))

      get_cohort_sc <- function(year) {
        match_val <- as.character(cohort_lookup$Cohort_fact) == as.character(as.integer(year))
        result <- unique(cohort_lookup$Cohort_num_sc[match_val])
        if (length(result) == 0) return(NA)
        result[1]
      }

      age_lm  <- lm(Age_real ~ as.numeric(Age_sc), data = data_model)
      lngt_lm <- lm(Lngt_real ~ as.numeric(LngtClassGrouped_sc), data = data_model)

      # Version adaptée : pas de Cohort_fact dans newdata (absent du modèle)
      compute_surface_pollachius <- function(model, cohort_value_sc, year_label) {
        lat_ref <- if ("Latitude" %in% names(data_model)) median(data_model$Latitude, na.rm = TRUE) else NA
        lon_ref <- if ("Longitude" %in% names(data_model)) median(data_model$Longitude, na.rm = TRUE) else NA

        newdata <- expand.grid(
          Age_sc = seq(min(data_model$Age_sc), max(data_model$Age_sc), length.out = 60),
          LngtClassGrouped_sc = seq(min(data_model$LngtClassGrouped_sc), max(data_model$LngtClassGrouped_sc), length.out = 60)
        )
        newdata$Cohort_num_sc <- cohort_value_sc
        if ("Latitude" %in% names(data_model))  newdata$Latitude  <- lat_ref
        if ("Longitude" %in% names(data_model)) newdata$Longitude <- lon_ref

        sm <- smooth_estimates(model, smooth = te_label, data = newdata)

        coefs_age  <- coef(age_lm)
        coefs_lngt <- coef(lngt_lm)
        sm$Age_real  <- coefs_age[1]  + coefs_age[2]  * as.numeric(sm$Age_sc)
        sm$Lngt_real <- coefs_lngt[1] + coefs_lngt[2] * as.numeric(sm$LngtClassGrouped_sc)
        sm$year_label <- year_label
        sm
      }

      sm_list <- lapply(available_targets, function(yr) {
        compute_surface_pollachius(bam_ref, get_cohort_sc(yr), yr)
      })

      max_abs_common <- max(sapply(sm_list, function(sm) max(abs(sm$.estimate), na.rm = TRUE)))

      plots_te <- Map(function(sm, yr) {
        obs_points <- data_model[as.character(data_model$Cohort_fact_ext) == as.character(as.integer(yr)), ]
        plot_surface_from_sm(sm, yr, max_abs_common, obs_points, pal_low, pal_mid, pal_high)
      }, sm_list, available_targets)

      p_te_combined <- wrap_plots(plots_te, ncol = length(plots_te)) +
        plot_layout(guides = "collect") &
        theme(legend.position = "right")

      ggsave(file.path(out_dir, "01_tenseur_cohortes.pdf"),
             plot = p_te_combined, width = 8 * length(plots_te), height = 8, units = "cm")
    }
  } else {
    cat("  Conditions non remplies pour tracer le tenseur de", sp, "- étape 1 ignorée.\n")
  }

  # ---- Effet spatial (présent pour Pollachius) ----
  spatial_label <- grep("Latitude,Longitude", term_labels, value = TRUE)
  if (length(spatial_label) > 0) {
    p_spatial <- plot_spatial_effect(bam_ref, spatial_label, data_model, sp, coast, pal_low, pal_mid, pal_high)
    if (!is.null(p_spatial)) {
      ggsave(file.path(out_dir, "02_spatial.pdf"), plot = p_spatial, width = 12, height = 10, units = "cm")
    }
  } else {
    cat("  Terme spatial absent pour", sp, "- étape 2 ignorée.\n")
  }

  cat("  Pas de terme saisonnier ni d'effet aléatoire de cohorte dans le modèle final de", sp, "- étapes 3 et 4 non applicables.\n")
  cat("  Done →", out_dir, "\n")

} else {
  cat("  Aucun modèle trouvé pour", sp, "- traitement séparé ignoré.\n")
}

# # =============================================================================
# #  Bloc supplémentaire : effets aléatoires de cohorte en année réelle,
# #  pour toutes les espèces qui disposent de ce terme
# # =============================================================================

# cat("\n\n========== Effet marginal moyen + effets aléatoires de cohorte (année réelle) ==========\n")

# # ---- Calcul de l'effet marginal moyen (AME) du tenseur, par cohorte ----
# compute_ame_by_cohort <- function(bam_ref, te_label, data_model, cohorts_sc) {
  
#   lat_ref  <- if ("Latitude" %in% names(data_model)) median(data_model$Latitude, na.rm = TRUE) else NA
#   lon_ref  <- if ("Longitude" %in% names(data_model)) median(data_model$Longitude, na.rm = TRUE) else NA
#   jday_ref <- if ("julian_day" %in% names(data_model)) median(data_model$julian_day, na.rm = TRUE) else NA
#   cohort_fact_ref <- if ("Cohort_fact" %in% names(data_model)) levels(data_model$Cohort_fact)[1] else NA
  
#   ame_by_cohort <- sapply(cohorts_sc, function(c_val) {
    
#     # Grille : toutes les combinaisons âge-longueur réellement observées
#     newdata <- data.frame(
#       Age_sc = data_model$Age_sc,
#       LngtClassGrouped_sc = data_model$LngtClassGrouped_sc
#     )
#     newdata$Cohort_num_sc <- c_val
#     if (!is.na(lat_ref))  newdata$Latitude  <- lat_ref
#     if (!is.na(lon_ref))  newdata$Longitude <- lon_ref
#     if (!is.na(jday_ref)) newdata$julian_day <- jday_ref
#     if (!is.na(cohort_fact_ref)) {
#       newdata$Cohort_fact <- factor(cohort_fact_ref, levels = levels(data_model$Cohort_fact))
#     }
    
#     sm <- smooth_estimates(bam_ref, smooth = te_label, data = newdata)
#     mean(sm$.estimate, na.rm = TRUE)
#   })
  
#   ame_by_cohort
# }

# # ---- Tracé combiné : AME (ligne bleue) + effets aléatoires réalisés (diamants noirs) ----
# plot_marginal_and_random_effect <- function(bam_ref, re_label, te_label, data_model, sp,
#                                              cohorts_years_all, cohorts_sc_all) {
  
#   # ---- Effet marginal moyen ----
#   ame_vals <- compute_ame_by_cohort(bam_ref, te_label, data_model, cohorts_sc_all)
#   ame_df <- data.frame(year = cohorts_years_all, ame = ame_vals)
#   ame_df <- ame_df[order(ame_df$year), ]
  
#   # ---- Effets aléatoires réalisés, centrés sur l'AME de leur cohorte ----
#   sm_re <- NULL
#   if (length(re_label) > 0) {
#     sm_re <- tryCatch(
#       smooth_estimates(bam_ref, smooth = re_label),
#       error = function(e) { cat("  Échec extraction effet aléatoire :", conditionMessage(e), "\n"); NULL }
#     )
#     if (!is.null(sm_re) && "Cohort_fact" %in% names(sm_re)) {
#       sm_re$year <- as.numeric(as.character(sm_re$Cohort_fact))
#       sm_re <- sm_re[order(sm_re$year), ]
      
#       # Correspondance AME <-> année pour centrer chaque point
#       sm_re$ame_ref <- ame_df$ame[match(sm_re$year, ame_df$year)]
#       sm_re$estimate_centered <- sm_re$.estimate + sm_re$ame_ref
      
#     } else {
#       sm_re <- NULL
#     }
#   }
  
#   p <- ggplot() +
#     geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
#     geom_line(data = ame_df, aes(x = year, y = ame), colour = "#2166AC", linewidth = 0.7)
  
#   if (!is.null(sm_re)) {
#     has_se <- ".se" %in% names(sm_re)
#     if (has_se) {
#       p <- p +
#         geom_errorbar(data = sm_re,
#                       aes(x = year, ymin = estimate_centered - 1.96 * .se, ymax = estimate_centered + 1.96 * .se),
#                       width = 0.3, colour = "grey25", alpha = 0.6)
#     }
#     p <- p +
#       geom_point(data = sm_re, aes(x = year, y = estimate_centered),
#                  shape = 18, colour = "black", size = 2.5)
#   }
  
#   p <- p +
#     labs(title = paste("Effet marginal moyen et effets aléatoires réalisés -", sp),
#          x = "Cohorte", y = "Log-odds") +
#     theme_minimal(base_size = 9)
  
#   p
# }

# species_all_re <- basename(list.dirs(base_dir, recursive = FALSE))

# for (sp in species_all_re) {

#   cat("\n----------", sp, "----------\n")

#   bam_ref <- load_bam_ref(sp, base_dir)
#   if (is.null(bam_ref)) { cat("  Aucun modèle trouvé - ignoré.\n"); next }

#   sp_slug <- gsub(" ", "_", sp)
#   out_dir <- file.path(out_root, sp_slug)
#   dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

#   data_model  <- model.frame(bam_ref)
#   term_labels <- gratia::smooths(bam_ref)
#   te_label <- grep("te\\(Age_sc,LngtClassGrouped_sc,Cohort_num_sc\\)", term_labels, value = TRUE)

#   if (length(te_label) == 0) {
#     cat("  Terme tenseur absent pour", sp, "- ignoré.\n")
#     next
#   }

#   re_label <- grep("Cohort_fact", term_labels, value = TRUE)
#   re_label <- re_label[!re_label %in% te_label]

#   # ---- Récupération de toutes les cohortes disponibles (réelles + centrées-réduites) ----
#   if ("Cohort_fact" %in% names(data_model)) {
#     cohort_lookup <- unique(data.frame(
#       Cohort_fact = data_model$Cohort_fact,
#       Cohort_num_sc = data_model$Cohort_num_sc
#     ))
#     cohorts_years_all <- as.numeric(as.character(cohort_lookup$Cohort_fact))
#     cohorts_sc_all    <- cohort_lookup$Cohort_num_sc
#   } else {
#     # Cas Pollachius virens : reconstruire depuis data_expanded_1991_2023
#     data_sp_full <- data_expanded_1991_2023 %>%
#       dplyr::filter(Species == sp, !is.na(Numeric_sex)) %>%
#       droplevels()
#     if (!all(c("Cohort_fact", "Cohort_num_sc") %in% names(data_sp_full))) {
#       cat("  Impossible de reconstruire les cohortes pour", sp, "- ignoré.\n")
#       next
#     }
#     cohort_lookup <- unique(data.frame(
#       Cohort_fact = data_sp_full$Cohort_fact,
#       Cohort_num_sc = data_sp_full$Cohort_num_sc
#     ))
#     cohorts_years_all <- as.numeric(as.character(cohort_lookup$Cohort_fact))
#     cohorts_sc_all    <- cohort_lookup$Cohort_num_sc
#   }

#   ord <- order(cohorts_years_all)
#   cohorts_years_all <- cohorts_years_all[ord]
#   cohorts_sc_all    <- cohorts_sc_all[ord]

#   p_combined <- plot_marginal_and_random_effect(
#     bam_ref, re_label, te_label, data_model, sp,
#     cohorts_years_all, cohorts_sc_all
#   )

#   ggsave(file.path(out_dir, "05_marginal_et_re_cohort.pdf"), plot = p_combined, width = 14, height = 8, units = "cm")
#   cat("  Effet marginal moyen + effets aléatoires tracés avec succès.\n")
# }

# cat("\n========== Fin du bloc effet marginal / effets aléatoires ==========\n")

# =============================================================================
#  Graphe A : effet cohorte du tenseur (AME) + ruban de confiance
# =============================================================================

compute_ame_with_ci <- function(bam_ref, te_label, data_model, cohorts_sc,
                                 cols_te_in_vcov = NULL) {
  
  lat_ref  <- if ("Latitude" %in% names(data_model)) median(data_model$Latitude, na.rm = TRUE) else NA
  lon_ref  <- if ("Longitude" %in% names(data_model)) median(data_model$Longitude, na.rm = TRUE) else NA
  jday_ref <- if ("julian_day" %in% names(data_model)) median(data_model$julian_day, na.rm = TRUE) else NA
  cohort_fact_ref <- if ("Cohort_fact" %in% names(data_model)) levels(data_model$Cohort_fact)[1] else NA
  
  Vb <- vcov(bam_ref)
  
  res <- lapply(cohorts_sc, function(c_val) {
    newdata <- data.frame(
      Age_sc = data_model$Age_sc,
      LngtClassGrouped_sc = data_model$LngtClassGrouped_sc
    )
    newdata$Cohort_num_sc <- c_val
    if (!is.na(lat_ref))  newdata$Latitude  <- lat_ref
    if (!is.na(lon_ref))  newdata$Longitude <- lon_ref
    if (!is.na(jday_ref)) newdata$julian_day <- jday_ref
    if (!is.na(cohort_fact_ref)) {
      newdata$Cohort_fact <- factor(cohort_fact_ref, levels = levels(data_model$Cohort_fact))
    }
    
    Xp <- predict(bam_ref, newdata = newdata, type = "lpmatrix")
    Xbar <- colMeans(Xp)  # moyenne sur toutes les observations -> AME
    
    ame <- as.numeric(Xbar %*% coef(bam_ref))
    se  <- sqrt(as.numeric(t(Xbar) %*% Vb %*% Xbar))
    
    c(ame = ame, se = se)
  })
  
  do.call(rbind, res)
}

plot_tenseur_cohort_AME_ribbon <- function(bam_ref, te_label, data_model, sp,
                                            cohorts_years_all, cohorts_sc_all,
                                            couleur = "#2166AC") {
  
  res <- compute_ame_with_ci(bam_ref, te_label, data_model, cohorts_sc_all)
  ame_vals <- res[, "ame"]
  se_vals  <- res[, "se"]
  
  ame_centre <- ame_vals - mean(ame_vals)
  
  df <- data.frame(year = cohorts_years_all, ame = ame_centre, se = se_vals)
  df <- df[order(df$year), ]
  
  ggplot(df, aes(x = year, y = ame)) +
    geom_ribbon(aes(ymin = ame - 1.96 * se, ymax = ame + 1.96 * se),
                fill = couleur, alpha = 0.2) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
    geom_line(colour = couleur, linewidth = 0.8) +
    labs(title = paste("Effet cohorte du tenseur -", sp),
         x = "Cohorte", y = "Log-odds") +
    theme_minimal(base_size = 9)
}

# =============================================================================
#  Graphe B : effets aléatoires réalisés, centrés, IC par cohorte,
#             sigma_cohort en sous-titre
# =============================================================================

plot_re_realized_with_CI <- function(bam_ref, re_label, sp) {
  
  sm_re <- tryCatch(
    smooth_estimates(bam_ref, smooth = re_label),
    error = function(e) { cat("  Échec extraction effet aléatoire :", conditionMessage(e), "\n"); NULL }
  )
  
  if (is.null(sm_re) || !"Cohort_fact" %in% names(sm_re)) return(NULL)
  
  sm_re$year <- as.numeric(as.character(sm_re$Cohort_fact))
  sm_re <- sm_re[order(sm_re$year), ]
  
  has_se <- ".se" %in% names(sm_re)
  
  # ---- sigma_cohort : ecart-type de la loi normale des effets aleatoires ----
  vc <- gam.vcomp(bam_ref, rescale = FALSE)
  # le nom exact de la ligne depend de mgcv, on cherche celle qui contient "Cohort_fact"
  sigma_row <- grep("Cohort_fact", rownames(vc), value = TRUE)
  sigma_cohort <- if (length(sigma_row) > 0) vc[sigma_row[1], "std.dev"] else NA
  
  p <- ggplot(sm_re, aes(x = year, y = .estimate))
  
  if (has_se) {
    p <- p + geom_errorbar(aes(ymin = .estimate - 1.96 * .se, ymax = .estimate + 1.96 * .se),
                            width = 0.3, colour = "grey30", alpha = 0.6)
  }
  
  p <- p +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
    geom_point(shape = 18, colour = "black", size = 2.5) +
    labs(title = paste("Effets aléatoires de cohorte réalisés -", sp),
         subtitle = if (!is.na(sigma_cohort)) bquote(sigma[cohort] == .(round(sigma_cohort, 4))) else NULL,
         x = "Cohorte", y = "Log-odds") +
    theme_minimal(base_size = 9)
  
  p
}

cat("\n\n========== Effet cohorte du tenseur (AME + IC) et effets aléatoires (IC + sigma) ==========\n")

species_all_re <- basename(list.dirs(base_dir, recursive = FALSE))

for (sp in species_all_re) {
  
  cat("\n----------", sp, "----------\n")
  
  bam_ref <- load_bam_ref(sp, base_dir)
  if (is.null(bam_ref)) { cat("  Aucun modèle trouvé - ignoré.\n"); next }
  
  sp_slug <- gsub(" ", "_", sp)
  out_dir <- file.path(out_root, sp_slug)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  data_model  <- model.frame(bam_ref)
  term_labels <- gratia::smooths(bam_ref)
  te_label <- grep("te\\(Age_sc,LngtClassGrouped_sc,Cohort_num_sc\\)", term_labels, value = TRUE)
  
  if (length(te_label) == 0) {
    cat("  Terme tenseur absent pour", sp, "- ignoré.\n")
    next
  }
  
  re_label <- grep("Cohort_fact", term_labels, value = TRUE)
  re_label <- re_label[!re_label %in% te_label]
  
  if ("Cohort_fact" %in% names(data_model)) {
    cohort_lookup <- unique(data.frame(Cohort_fact = data_model$Cohort_fact, Cohort_num_sc = data_model$Cohort_num_sc))
    cohorts_years_all <- as.numeric(as.character(cohort_lookup$Cohort_fact))
    cohorts_sc_all    <- cohort_lookup$Cohort_num_sc
  } else {
    data_sp_full <- data_expanded_1991_2023 %>%
      dplyr::filter(Species == sp, !is.na(Numeric_sex)) %>% droplevels()
    if (!all(c("Cohort_fact", "Cohort_num_sc") %in% names(data_sp_full))) {
      cat("  Impossible de reconstruire les cohortes pour", sp, "- ignoré.\n")
      next
    }
    cohort_lookup <- unique(data.frame(Cohort_fact = data_sp_full$Cohort_fact, Cohort_num_sc = data_sp_full$Cohort_num_sc))
    cohorts_years_all <- as.numeric(as.character(cohort_lookup$Cohort_fact))
    cohorts_sc_all    <- cohort_lookup$Cohort_num_sc
  }
  
  ord <- order(cohorts_years_all)
  cohorts_years_all <- cohorts_years_all[ord]
  cohorts_sc_all    <- cohorts_sc_all[ord]
  
  # ---- Graphe A ----
  p_tenseur_ame <- tryCatch(
    plot_tenseur_cohort_AME_ribbon(bam_ref, te_label, data_model, sp, cohorts_years_all, cohorts_sc_all),
    error = function(e) { cat("  Échec AME tenseur :", conditionMessage(e), "\n"); NULL }
  )
  if (!is.null(p_tenseur_ame)) {
    ggsave(file.path(out_dir, "05a_tenseur_cohort_AME.pdf"), plot = p_tenseur_ame, width = 14, height = 8, units = "cm")
  }
  
  # ---- Graphe B ----
  if (length(re_label) > 0) {
    p_re_ci <- tryCatch(
      plot_re_realized_with_CI(bam_ref, re_label, sp),
      error = function(e) { cat("  Échec RE + IC :", conditionMessage(e), "\n"); NULL }
    )
    if (!is.null(p_re_ci)) {
      ggsave(file.path(out_dir, "05b_re_cohort_CI.pdf"), plot = p_re_ci, width = 14, height = 8, units = "cm")
    }
  } else {
    cat("  Pas d'effet aléatoire pour", sp, "- graphe B ignoré.\n")
  }
  
  cat("  Done ->", out_dir, "\n")
}

cat("\n========== Fin du bloc ==========\n")