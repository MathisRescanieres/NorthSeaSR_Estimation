# =============================================================================
#  Visualisation EOF — cohorte 2000, couche de surface (depth == 1)
#  Fenêtre périnatale : 1er octobre 1999 → 31 mars 2001
#  Base spatiale commune estimée sur la série complète détrendée.
# =============================================================================

library(rnaturalearth)
library(sf)
library(scales)

world_sf <- ne_countries(scale = "medium", returnclass = "sf")



world_sf <- ne_countries(scale = "medium", returnclass = "sf")

# ── Paramètres ────────────────────────────────────────────────────────────────
cohort           <- 2000
win_start        <- as.Date("1999-10-01")
win_end          <- as.Date("2001-03-31")
depth_val        <- 1
threshold        <- 0.15

out_dir <- "/home/mathis/NorthSeaSR_Estimation/SR_Estimation/rapport/rapport_miproj/figures/results/eof/cohort_2000"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ── Détrending sur la série complète ─────────────────────────────────────────
.detrend_linear_mean <- function(df) {
  temp_bar_df <- df %>%
    group_by(depth, month, year) %>%
    summarise(temp_bar = mean(temp, na.rm = TRUE), .groups = "drop")

  trend_params <- temp_bar_df %>%
    group_by(depth, month) %>%
    summarise(
      alpha = coef(lm(temp_bar ~ year))[["year"]],
      beta  = coef(lm(temp_bar ~ year))[["(Intercept)"]],
      .groups = "drop"
    )

  df_out <- df %>%
    left_join(trend_params, by = c("depth", "month")) %>%
    left_join(temp_bar_df,  by = c("depth", "month", "year")) %>%
    mutate(temp = temp - (beta + alpha * year)) %>%
    dplyr::select(lon, lat, depth, time, year, month, temp, temp_bar, alpha, beta)

  list(
    temp_bar_df  = temp_bar_df,
    trend_params = trend_params,
    df_out       = df_out
  )
}

detrend_res  <- .detrend_linear_mean(df)
temp_bar_df  <- detrend_res$temp_bar_df
trend_params <- detrend_res$trend_params
df_detrended <- detrend_res$df_out

# =============================================================================
#  EOF sur la fenêtre périnatale de la cohorte (pas sur la série complète)
# =============================================================================

df_cohort <- df_detrended %>%
  dplyr::filter(depth == depth_val,
                time >= win_start & time <= win_end)

eof_res <- metR::EOF(temp ~ lon + lat | time,
                     data = df_cohort, n = 1:18)

# Sélection des PC au seuil de 15 %
sdev_df <- eof_res$sdev %>%
  mutate(retained = r2 >= threshold)

n_retained   <- sum(sdev_df$retained)
retained_pcs <- sdev_df %>% dplyr::filter(retained) %>% pull(PC)

# Modes spatiaux
spatial_loadings <- eof_res$left %>%
  rename(loading = temp)

# ── Homogénéisation des signes ────────────────────────────────────────────────
# Le signe des vecteurs propres est arbitraire. On impose que le loading
# au point de référence (lon = -2.625, lat = 47.375 — embouchure de la Vilaine)
# soit positif pour chaque PC. Cette convention permet de comparer les EOFs
# de différentes cohortes entre elles.

ref_lon <- -2.625
ref_lat <- 47.375

sign_ref <- spatial_loadings %>%
  group_by(PC) %>%
  slice_min(abs(lon - ref_lon) + abs(lat - ref_lat), n = 1) %>%
  ungroup() %>%
  dplyr::select(PC, sign_ref = loading) %>%
  mutate(flip = ifelse(sign_ref < 0, -1L, 1L))

spatial_loadings <- spatial_loadings %>%
  left_join(sign_ref %>% dplyr::select(PC, flip), by = "PC") %>%
  mutate(loading = loading * flip) %>%
  dplyr::select(-flip)

scores_cohort <- eof_res$right %>%
  rename(score = temp) %>%
  left_join(sign_ref %>% dplyr::select(PC, flip), by = "PC") %>%
  mutate(score = score * flip) %>%
  dplyr::select(-flip) %>%
  dplyr::filter(PC %in% retained_pcs)

# =============================================================================
#  Plot i — Température brute de surface sur la fenêtre périnatale
# =============================================================================

df_surface_raw <- df %>%
  dplyr::filter(depth == depth_val,
                time >= win_start & time <= win_end) %>%
  mutate(time_lab = format(time, "%b %Y"))

time_levels <- df_surface_raw %>%
  distinct(time, time_lab) %>% arrange(time) %>% pull(time_lab)

df_surface_raw <- df_surface_raw %>%
  mutate(time_lab = factor(time_lab, levels = time_levels))

p_surface <- ggplot(df_surface_raw, aes(lon, lat, fill = temp)) +
  geom_raster() +
  facet_wrap(~ time_lab, nrow = 3) +
  scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#d73027",
                       name = "T (°C)") +
  coord_quickmap() +
  labs(title    = paste0("Température de surface (depth = ", depth_val, " m) — Cohorte ", cohort),
       subtitle = paste(win_start, "→", win_end),
       x = "Longitude", y = "Latitude") +
  theme_minimal(base_size = 11) +
  theme(strip.text = element_text(size = 8))

ggsave(file.path(out_dir, "01_surface_raw_window.pdf"),
       plot = p_surface, width = 16, height = 8, device = "pdf")

# =============================================================================
#  Plot ii — Détrending linéaire sur la fenêtre périnatale
# =============================================================================

trend_plot_data <- temp_bar_df %>%
  mutate(time = as.Date(paste(year, month, "15", sep = "-"))) %>%
  dplyr::filter(depth == depth_val,
                time >= win_start & time <= win_end) %>%
  left_join(trend_params %>% dplyr::filter(depth == depth_val),
            by = c("depth", "month")) %>%
  mutate(trend_val = beta + alpha * year)

p_detrend <- ggplot(trend_plot_data, aes(x = time)) +
  geom_segment(
    aes(xend = time, y = trend_val, yend = temp_bar),
    color = "forestgreen", alpha = 0.8,
    arrow = arrow(length = unit(0.15, "cm"), type = "closed")
  ) +
  geom_line(aes(y = trend_val), color = "red", linewidth = 0.9) +
  geom_point(aes(y = temp_bar), color = "black", size = 2) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +
  labs(x = NULL, y = "Température spatiale moyenne (°C)") +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggsave(file.path(out_dir, "02_detrending.pdf"),
       plot = p_detrend, width = 12, height = 5, device = "pdf")

# =============================================================================
#  Plot iii — Variance expliquée (base commune, série complète)
# =============================================================================

p_variance <- ggplot(sdev_df, aes(x = PC, y = r2, fill = retained)) +
  geom_col(width = 0.75) +
  geom_hline(yintercept = threshold, linetype = "dashed", color = "grey40") +
  scale_fill_manual(
    values = c("TRUE" = "#2166ac", "FALSE" = "#d73027"),
    labels = c("TRUE" = paste0("≥ ", threshold * 100, " % (retenu)"),
               "FALSE" = paste0("< ",  threshold * 100, " % (écarté)")),
    name = NULL
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(title    = "Variance expliquée par composante — Base commune (série complète)",
       subtitle = paste0(n_retained, " PC(s) retenue(s) au seuil de ", threshold * 100, " %"),
       x = NULL, y = "Variance expliquée") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

ggsave(file.path(out_dir, "03_variance_expliquee.pdf"),
       plot = p_variance, width = 10, height = 5, device = "pdf")

# =============================================================================
#  Plot iv — Figure combinée : cartes (gauche) + séries temporelles (droite)
# =============================================================================

library(patchwork)

# Limites communes aux deux cartes
load_lim <- spatial_loadings %>%
  dplyr::filter(PC %in% retained_pcs) %>%
  summarise(lo = min(loading, na.rm = TRUE),
            hi = max(loading, na.rm = TRUE))
load_range <- c(load_lim$lo, load_lim$hi)

# Fonction utilitaire : carte pour une PC
make_map <- function(pc) {
  pct <- round(sdev_df$r2[match(pc, sdev_df$PC)] * 100, 1)
  spatial_loadings %>%
    dplyr::filter(PC == pc) %>%
    ggplot(aes(lon, lat, fill = loading)) +
    geom_raster() +
    geom_sf(data = world_sf, fill = "grey85", colour = "grey30",
            linewidth = 0.3, inherit.aes = FALSE) +
    scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#d73027",
                         name = "Loading",
                         limits = load_range,
                         labels = scales::number_format(accuracy = 0.01)) +
    coord_sf(xlim = range(spatial_loadings$lon),
             ylim = range(spatial_loadings$lat)) +
    labs(title = paste0(pc, " (", pct, " %)"), x = NULL, y = NULL) +
    theme_minimal(base_size = 10) +
    theme(legend.position = "bottom",
          plot.title = element_text(face = "bold"))
}

# Fonction utilitaire : série temporelle pour une PC
make_ts <- function(pc) {
  pct <- round(sdev_df$r2[match(pc, sdev_df$PC)] * 100, 1)
  scores_cohort %>%
    dplyr::filter(PC == pc) %>%
    ggplot(aes(x = time, y = score)) +
    geom_line(color = "#2166ac", linewidth = 0.9) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +
    labs(title = paste0("Score ", pc),
         x = NULL, y = "Score") +
    theme_minimal(base_size = 10) +
    theme(axis.text.x = element_text(angle = 30, hjust = 1),
          plot.title = element_text(face = "bold"))
}

# Assemblage ligne par ligne : carte | série temporelle, empilées par PC
rows <- lapply(retained_pcs, function(pc) make_map(pc) | make_ts(pc))
p_combined <- Reduce(`/`, rows) +
  plot_annotation(
    title    = paste0("EOF — Cohorte ", cohort),
    theme    = theme(plot.title = element_text(face = "bold", size = 14))
  )

ggsave(file.path(out_dir, "04_eof_combined.pdf"),
       plot   = p_combined,
       width  = 14,
       height = 6 * length(retained_pcs),
       device = "pdf")

cat("Figures sauvées dans :", out_dir, "\n")