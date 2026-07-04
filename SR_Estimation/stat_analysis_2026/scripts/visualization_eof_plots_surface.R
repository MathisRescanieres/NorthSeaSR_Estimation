# ==============================================================================
# pipeline_eof_plots_surface.R
# ==============================================================================

library(dplyr)
library(ggplot2)
library(lubridate)
library(metR)
library(patchwork)
library(rnaturalearth)
library(sf)

DEPTH      <- 1
N_PC       <- 3
N_PC_MAX   <- 10
YEAR_START <- 1991
YEAR_END   <- 2023

DIR_OUT <- "../../rapport/rapport_miproj/figures/results/eof/surface"
dir.create(DIR_OUT, recursive = TRUE, showWarnings = FALSE)

MONTH_LABELS <- c("janvier","fevrier","mars","avril","mai","juin",
                   "juillet","aout","septembre","octobre","novembre","decembre")

MONTH_LABELS_FR <- c("Janvier","Fevrier","Mars","Avril","Mai","Juin",
                      "Juillet","Aout","Septembre","Octobre","Novembre","Decembre")

world_sf <- ne_countries(scale = "medium", returnclass = "sf")
XLIM <- c(-4.5, 13.5)
YLIM <- c(48.6, 62.5)

# ==============================================================================
# Boucle principale
# ==============================================================================

results <- vector("list", 12)

for (m in 1:12) {

  cat("-- Mois", m, MONTH_LABELS[m], "\n")

  df_m <- df %>% dplyr::filter(depth == DEPTH, month == m)

  tbar <- df_m %>%
    group_by(time, year) %>%
    summarise(temp_mean = mean(temp, na.rm = TRUE), .groups = "drop")

  mod  <- lm(temp_mean ~ year, data = tbar)
  tbar <- tbar %>% mutate(trend = predict(mod), resid = temp_mean - trend)

  df_resid <- df_m %>%
    left_join(tbar %>% select(time, trend), by = "time") %>%
    mutate(temp_resid = temp - trend) %>%
    select(lon, lat, time, year, temp_resid)

  eof_res <- tryCatch(
    metR::EOF(temp_resid ~ lon + lat | time, data = df_resid, n = 1:N_PC_MAX),
    error = function(e) { message("EOF echouee mois ", m); NULL }
  )
  if (is.null(eof_res)) next

  # Normalisation des signes
  ref_lon <- -2.625; ref_lat <- 47.375
  ref_pt  <- eof_res$left %>%
    distinct(lon, lat) %>%
    mutate(dist = abs(lon - ref_lon) + abs(lat - ref_lat)) %>%
    slice_min(dist, n = 1, with_ties = FALSE)

  for (pc_lev in unique(eof_res$left$PC)) {
    sgn <- eof_res$left %>%
      dplyr::filter(PC == pc_lev, lon == ref_pt$lon, lat == ref_pt$lat) %>%
      pull(temp_resid)
    if (length(sgn) == 0 || is.na(sgn)) next
    if (sgn < 0) {
      eof_res$left  <- eof_res$left  %>%
        mutate(temp_resid = ifelse(PC == pc_lev, -temp_resid, temp_resid))
      eof_res$right <- eof_res$right %>%
        mutate(temp_resid = ifelse(PC == pc_lev, -temp_resid, temp_resid))
    }
  }

  # sdev : PC comme entier, r2 comme numérique
  sdev_df <- eof_res$sdev %>%
    mutate(pc_int = seq_len(n()),
           group  = ifelse(pc_int <= N_PC, "Retenue", "Eliminee"))

  val_col <- setdiff(names(eof_res$left), c("lon", "lat", "PC"))

  # ── i) Detrending ───────────────────────────────────────────────────────────
  pd <- tbar %>% dplyr::filter(year >= YEAR_START, year <= YEAR_END)

  p_det <- ggplot(pd, aes(x = time)) +
    geom_line(aes(y = trend), colour = "red3", linewidth = 1.2) +
    geom_point(aes(y = temp_mean), colour = "black", size = 2.5) +
    geom_segment(aes(xend = time, y = trend, yend = temp_mean),
                 colour = "forestgreen", linewidth = 0.7,
                 arrow = arrow(length = unit(0.15, "cm"), type = "closed")) +
    labs(title = MONTH_LABELS_FR[m], x = NULL,
         y = expression(bar(T)[obs]~"(°C)")) +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
    theme_bw(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid.minor = element_blank())

  if (m == 1) ggsave(file.path(DIR_OUT, "i_detrending_janvier.pdf"),
                     p_det, width = 10, height = 5)

  results[[m]] <- list(
    tbar     = tbar,
    mod      = mod,
    loadings = eof_res$left  %>% rename(loading = all_of(val_col)),
    scores   = eof_res$right %>% rename(score   = all_of(val_col)),
    sdev     = sdev_df,
    p_det    = p_det
  )
}

# ==============================================================================
# Fonctions utilitaires
# pc est toujours un ENTIER (1, 2, 3)
# Le filtre utilise paste0("PC", pc) pour matcher le facteur de metR
# Le titre utilise directement pc pour afficher "PC1" sans doublon
# ==============================================================================

make_map <- function(loadings_df, sdev_df, pc, month_label = NULL) {
  pct <- round(sdev_df$r2[sdev_df$pc_int == pc] * 100, 1)
  ttl <- if (!is.null(month_label)) {
    paste0(month_label, " - PC", pc, " (", pct, "%)")
  } else {
    paste0("PC", pc, " (", pct, "%)")
  }
  loadings_df %>%
    dplyr::filter(PC == paste0("PC", pc)) %>%
    ggplot(aes(lon, lat, fill = loading)) +
    geom_raster() +
    geom_sf(data = world_sf, fill = "grey85", colour = "grey30",
            linewidth = 0.3, inherit.aes = FALSE) +
    scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#d73027",
                         name = "Loading") +
    coord_sf(xlim = XLIM, ylim = YLIM) +
    labs(title = ttl) +
    theme_minimal(base_size = 10) +
    theme(plot.title        = element_text(face = "bold", size = 9),
          axis.title        = element_blank(),
          axis.text         = element_text(size = 7),
          legend.key.height = unit(0.4, "cm"))
}

make_ts <- function(scores_df, sdev_df, pc,
                    year_start = YEAR_START, year_end = YEAR_END,
                    month_label = NULL) {
  pct <- round(sdev_df$r2[sdev_df$pc_int == pc] * 100, 1)
  ttl <- if (!is.null(month_label)) {
    paste0(month_label, " - Score PC", pc, " (", pct, "%)")
  } else {
    paste0("Score PC", pc, " (", pct, "%)")
  }
  scores_df %>%
    dplyr::filter(PC == paste0("PC", pc),
                  year(time) >= year_start,
                  year(time) <= year_end) %>%
    ggplot(aes(x = time, y = score)) +
    geom_line(colour = "#2166ac", linewidth = 0.8) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
    labs(title = ttl, x = NULL, y = "Score") +
    scale_x_date(date_breaks = "4 years", date_labels = "%Y") +
    theme_bw(base_size = 10) +
    theme(axis.text.x     = element_text(angle = 45, hjust = 1),
          plot.title      = element_text(face = "bold", size = 9),
          panel.grid.minor = element_blank())
}

make_var <- function(sdev_df, month_label = NULL) {
  ggplot(sdev_df, aes(x = pc_int, y = r2, fill = group)) +
    geom_col(width = 0.7) +
    scale_fill_manual(values = c("Retenue" = "#2166ac", "Eliminee" = "#d73027"),
                      name = NULL) +
    scale_x_continuous(breaks = 1:N_PC_MAX) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(title = month_label, x = "PC", y = "Variance expliquee") +
    theme_bw(base_size = 10) +
    theme(legend.position  = "none",
          plot.title       = element_text(face = "bold", size = 9),
          panel.grid.minor = element_blank())
}

# ==============================================================================
# i bis) Detrending — grille 4x3 tous les mois
# ==============================================================================

det_plots <- lapply(1:12, function(m) {
  if (is.null(results[[m]])) return(NULL)
  results[[m]]$p_det
})
det_plots <- Filter(Negate(is.null), det_plots)

p_det_all <- wrap_plots(det_plots, ncol = 3) +
  plot_annotation(title = "Detrending lineaire - surface (depth = 1 m)",
                  theme = theme(plot.title = element_text(face = "bold", size = 13)))

ggsave(file.path(DIR_OUT, "i_detrending_all_months.pdf"),
       p_det_all, width = 18, height = 20)

# ==============================================================================
# ii) Variance expliquee — grille 4x3
# ==============================================================================

var_plots <- lapply(1:12, function(m) {
  if (is.null(results[[m]])) return(NULL)
  make_var(results[[m]]$sdev, month_label = MONTH_LABELS_FR[m])
})
var_plots <- Filter(Negate(is.null), var_plots)

p_var_all <- wrap_plots(var_plots, ncol = 3) +
  plot_annotation(title = "Variance expliquee par PC - surface (depth = 1 m)",
                  theme = theme(plot.title = element_text(face = "bold", size = 13)))

ggsave(file.path(DIR_OUT, "ii_variance_all_months.pdf"),
       p_var_all, width = 12, height = 14)

# ==============================================================================
# iii) 12 cartes PC1
# ==============================================================================

maps_pc1 <- lapply(1:12, function(m) {
  if (is.null(results[[m]])) return(NULL)
  make_map(results[[m]]$loadings, results[[m]]$sdev,
           pc = 1, month_label = MONTH_LABELS_FR[m])
})
maps_pc1 <- Filter(Negate(is.null), maps_pc1)

p_maps_pc1 <- wrap_plots(maps_pc1, ncol = 3) +
  plot_annotation(title = "Loadings PC1 - surface (depth = 1 m)",
                  theme = theme(plot.title = element_text(face = "bold", size = 13)))

ggsave(file.path(DIR_OUT, "iii_maps_PC1_all_months.pdf"),
       p_maps_pc1, width = 14, height = 18)

# ==============================================================================
# iv) 12 series temporelles PC1
# ==============================================================================

ts_pc1 <- lapply(1:12, function(m) {
  if (is.null(results[[m]])) return(NULL)
  make_ts(results[[m]]$scores, results[[m]]$sdev,
          pc = 1, month_label = MONTH_LABELS_FR[m])
})
ts_pc1 <- Filter(Negate(is.null), ts_pc1)

p_ts_pc1 <- wrap_plots(ts_pc1, ncol = 3) +
  plot_annotation(title = "Scores PC1 - surface (depth = 1 m)",
                  theme = theme(plot.title = element_text(face = "bold", size = 13)))

ggsave(file.path(DIR_OUT, "iv_timeseries_PC1_all_months.pdf"),
       p_ts_pc1, width = 14, height = 18)

# ==============================================================================
# v) Figure corps de texte : PC1-3, cartes + TS, pour un mois donne
# ==============================================================================

plot_eof_month <- function(m, year_start = YEAR_START, year_end = YEAR_END) {
  res <- results[[m]]
  if (is.null(res)) stop("EOF non disponible pour le mois ", m)

  rows <- lapply(1:N_PC, function(pc) {
    p_map <- make_map(res$loadings, res$sdev, pc = pc)
    p_ts  <- make_ts(res$scores,   res$sdev, pc = pc,
                     year_start = year_start, year_end = year_end)
    p_map | p_ts
  })

  Reduce(`/`, rows) +
    plot_annotation(
      title = paste0("EOF surface - ", MONTH_LABELS_FR[m],
                     " (", year_start, "-", year_end, ")"),
      theme = theme(plot.title = element_text(face = "bold", size = 13))
    )
}

# ==============================================================================
# vi) Plots individuels pour un mois et une profondeur donnés
# ==============================================================================

plot_eof_single <- function(m, depth_val = 1,
                             year_start = YEAR_START, year_end = YEAR_END,
                             save = TRUE) {

  cat("-- Mois", m, MONTH_LABELS_FR[m], "| Profondeur", depth_val, "m\n")

  # Recalcul EOF pour la profondeur demandée
  df_m <- df %>% dplyr::filter(depth == depth_val, month == m)

  tbar <- df_m %>%
    group_by(time, year) %>%
    summarise(temp_mean = mean(temp, na.rm = TRUE), .groups = "drop")

  mod  <- lm(temp_mean ~ year, data = tbar)
  tbar <- tbar %>% mutate(trend = predict(mod), resid = temp_mean - trend)

  df_resid <- df_m %>%
    left_join(tbar %>% select(time, trend), by = "time") %>%
    mutate(temp_resid = temp - trend) %>%
    select(lon, lat, time, year, temp_resid)

  eof_res <- tryCatch(
    metR::EOF(temp_resid ~ lon + lat | time, data = df_resid, n = 1:N_PC_MAX),
    error = function(e) { message("EOF echouee mois ", m, " depth ", depth_val); NULL }
  )
  if (is.null(eof_res)) return(invisible(NULL))

  # Normalisation des signes
  ref_lon <- -2.625; ref_lat <- 47.375
  ref_pt  <- eof_res$left %>%
    distinct(lon, lat) %>%
    mutate(dist = abs(lon - ref_lon) + abs(lat - ref_lat)) %>%
    slice_min(dist, n = 1, with_ties = FALSE)

  for (pc_lev in unique(eof_res$left$PC)) {
    sgn <- eof_res$left %>%
      dplyr::filter(PC == pc_lev, lon == ref_pt$lon, lat == ref_pt$lat) %>%
      pull(temp_resid)
    if (length(sgn) == 0 || is.na(sgn)) next
    if (sgn < 0) {
      eof_res$left  <- eof_res$left  %>%
        mutate(temp_resid = ifelse(PC == pc_lev, -temp_resid, temp_resid))
      eof_res$right <- eof_res$right %>%
        mutate(temp_resid = ifelse(PC == pc_lev, -temp_resid, temp_resid))
    }
  }

  val_col <- setdiff(names(eof_res$left), c("lon", "lat", "PC"))
  loadings <- eof_res$left  %>% rename(loading = all_of(val_col))
  scores   <- eof_res$right %>% rename(score   = all_of(val_col))
  sdev_df  <- eof_res$sdev  %>%
    mutate(pc_int = seq_len(n()),
           group  = ifelse(pc_int <= N_PC, "Retenue", "Eliminee"))

  label <- paste0(MONTH_LABELS_FR[m], " - Profondeur de ", depth_val, " m")

  # Detrending
  pd <- tbar %>% dplyr::filter(year >= year_start, year <= year_end)
  p_det <- ggplot(pd, aes(x = time)) +
    geom_line(aes(y = trend), colour = "red3", linewidth = 1.2) +
    geom_point(aes(y = temp_mean), colour = "black", size = 2.5) +
    geom_segment(aes(xend = time, y = trend, yend = temp_mean),
                 colour = "forestgreen", linewidth = 0.7,
                 arrow = arrow(length = unit(0.15, "cm"), type = "closed")) +
    labs(title = paste0(label), x = NULL,
         y = expression(bar(T)[obs]~"(°C)")) +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
    theme_bw(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid.minor = element_blank())

  # Variance expliquée
  p_var <- make_var(sdev_df, month_label = paste0("Variance - ", label))

  # Cartes et TS par PC
  pc_plots <- lapply(1:N_PC, function(pc) {
    p_map <- make_map(loadings, sdev_df, pc = pc)
    p_ts  <- make_ts(scores, sdev_df, pc = pc,
                     year_start = year_start, year_end = year_end)
    list(map = p_map, ts = p_ts)
  })

  # Sorties individuelles
  out <- list(
    detrending = p_det,
    variance   = p_var,
    maps       = lapply(pc_plots, `[[`, "map"),
    timeseries = lapply(pc_plots, `[[`, "ts")
  )

  if (save) {
    slug <- paste0("single_m", m, "_d", depth_val)
    dir_single <- file.path(DIR_OUT, "single")
    dir.create(dir_single, recursive = TRUE, showWarnings = FALSE)

    ggsave(file.path(dir_single, paste0(slug, "_detrending.pdf")),
           p_det, width = 10, height = 5)
    ggsave(file.path(dir_single, paste0(slug, "_variance.pdf")),
           p_var, width = 6, height = 4)

    for (pc in 1:N_PC) {
      ggsave(file.path(dir_single, paste0(slug, "_PC", pc, "_map.pdf")),
             pc_plots[[pc]]$map, width = 7, height = 5)
      ggsave(file.path(dir_single, paste0(slug, "_PC", pc, "_ts.pdf")),
             pc_plots[[pc]]$ts,  width = 7, height = 4)
    }
    cat("Fichiers dans :", file.path(dir_single), "\n")
  }

  invisible(out)
}

# Exemple d'utilisation
# out <- plot_eof_single(m = 1, depth_val = 10)
# out$detrending
# out$maps[[1]]
# out$timeseries[[2]]

# p_jan <- plot_eof_month(m = 1)
# ggsave(file.path(DIR_OUT, "v_eof_janvier_surface.pdf"),
#        p_jan, width = 13, height = 11)

# cat("\n OK - Fichiers dans :", DIR_OUT, "\n")