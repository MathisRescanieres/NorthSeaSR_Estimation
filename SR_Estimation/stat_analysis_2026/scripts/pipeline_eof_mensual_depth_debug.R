# ==============================================================================
# pipeline_eof_interannual_depth.R
# ==============================================================================

library(dplyr)
library(tidyr)
library(lubridate)
library(purrr)
library(metR)
library(ggplot2)

ensure_dir <- function(dir) {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
}

dir_eof <- "../results_eof_depth/EOF"
ensure_dir(dir_eof)

# ================================================================
# STRUCTURE SORTIE EOF (maps + timeseries par profondeur)
# ================================================================

dir_maps <- file.path(dir_eof, "maps")
dir_ts   <- file.path(dir_eof, "timeseries")

ensure_dir(dir_maps)
ensure_dir(dir_ts)

# --Récupération du label de profondeur
.get_depth_label <- function(d) {
  paste0("depth_", gsub("\\.", "_", d))
}

# -- Fenêtre de 18 mois --
.month_window <- tibble(
  offset      = c(-3, -2, -1, 0:11, 12, 13, 14),
  month_label = c(
    "oct_m1", "nov_m1", "dec_m1",
    "jan", "feb", "mar", "apr", "may", "jun",
    "jul", "aug", "sep", "oct", "nov", "dec",
    "jan_p1", "feb_p1", "mar_p1"
  )
) %>%
  mutate(
    month_num  = month(as.Date("2000-01-01") %m+% months(offset)),
    year_shift = case_when(
      grepl("_m1$", month_label) ~ -1L,
      grepl("_p1$", month_label) ~  1L,
      TRUE                       ~  0L
    )
  )

# ================================================================
# 1. EOF par mois ET par couche de profondeur
# ================================================================
.build_monthly_depth_eof_list <- function(df, n_pc_start, n_pc_end,
                                           out_dir = NULL) {
  if (!is.null(out_dir)) ensure_dir(out_dir)

  depth_levels <- sort(unique(df$depth))
  cat("    Couches détectées :", depth_levels, "\n")

  eof_list <- list()

  for (m in 1:12) {
    cat("\n  ── Mois", m, "──\n")
    df_m <- df %>% filter(month(time) == m)

    n_times_m <- length(unique(df_m$time))
    cat("    Pas de temps disponibles :", n_times_m, "\n")

    if (n_times_m < 2) {
      warning("Mois ", m, " : pas assez de pas de temps — ignoré.")
      next
    }

    for (d in depth_levels) {
      key <- sprintf("month_%02d_depth_%g", m, d)
      cat("  → Tentative :", key, "\n")

      df_md <- df_m %>% filter(depth == d)
      cat("    Lignes après filtre depth :", nrow(df_md), "\n")

      # Vérification années complètes
      n_grid <- df_md %>% filter(time == first(df_md$time)) %>% nrow()
      cat("    Points de grille (n_grid) :", n_grid, "\n")

      valid_times <- df_md %>%
        group_by(time) %>%
        summarise(n = n(), .groups = "drop") %>%
        filter(n == n_grid) %>%
        pull(time)

      cat("    Années complètes retenues :", length(valid_times), "\n")

      df_md <- df_md %>% filter(time %in% valid_times)

      if (length(unique(df_md$time)) < 2) {
        warning("Mois ", m, " depth ", d, " : pas assez d'années complètes — ignoré.")
        cat("    ✗ ECHEC (pas assez d'années complètes)\n")
        next
      }

      n_pc_safe <- min(n_pc_end, length(unique(df_md$time)), n_grid)
      cat("    n_pc demandé :", n_pc_start, "→", n_pc_end,
          "| n_pc_safe utilisé :", n_pc_safe, "\n")

      if (n_pc_safe < n_pc_start) {
        cat("    ✗ ECHEC (n_pc_safe < n_pc_start)\n")
        next
      }

      eof_obj <- tryCatch(
        metR::EOF(temp ~ lon + lat | time, data = df_md, n = n_pc_start:n_pc_safe),
        error = function(e) {
          cat("    [ERROR EOF] —", conditionMessage(e), "\n")
          NULL
        },
        warning = function(w) {
          cat("    [WARNING EOF] —", conditionMessage(w), "\n")
          # On continue malgré le warning
          metR::EOF(temp ~ lon + lat | time, data = df_md, n = n_pc_start:n_pc_safe)
        }
      )

      if (!is.null(eof_obj)) {
        # ====================================
        # Sauvegarde des cartes et timeseries
        # ====================================

        depth_label <- .get_depth_label(d)
        month_label_dir <- sprintf("month_%02d", m)

        dir_map_d <- file.path(dir_maps, month_label_dir, depth_label)
        dir_ts_d  <- file.path(dir_ts,  month_label_dir, depth_label)

        ensure_dir(dir_map_d)
        ensure_dir(dir_ts_d)

        val_col <- "temp"

        for (pc_name in unique(eof_obj$left$PC)) {

          # ===========
          # Carte EOF
          # ===========
          df_map <- eof_obj$left %>%
            filter(PC == pc_name)

          p_map <- ggplot(df_map, aes(x = lon, y = lat, fill = .data[[val_col]])) +
            geom_raster() +
            scale_fill_viridis_c(option = "plasma") +
            coord_fixed() +
            labs(
              title = paste0("EOF ", pc_name, " | ", key)
            ) +
            theme_minimal()

          file_map <- file.path(
            dir_map_d,
            paste0(key, "_", pc_name, ".pdf")
          )

          ggsave(file_map, p_map, device = "pdf", width = 7, height = 5)


          # ===========
          # Timeseries 
          # ===========
          df_ts <- eof_obj$right %>%
            filter(PC == pc_name)

          p_ts <- ggplot(df_ts, aes(x = time, y = .data[[val_col]])) +
            geom_line(color = "steelblue") +
            labs(
              title = paste0("EOF TS ", pc_name, " | ", key),
              x = "Time",
              y = "Amplitude"
            ) +
            theme_minimal()

          file_ts <- file.path(
            dir_ts_d,
            paste0(key, "_", pc_name, "_ts.pdf")
          )

          ggsave(file_ts, p_ts, device = "pdf", width = 7, height = 4)
        }

                eof_list[[key]] <- eof_obj
                cat("    ✓ Succès — PCs calculées :",
                    length(unique(eof_obj$right$PC)), "\n")
              } else {
                cat("    ✗ ECHEC (eof_obj est NULL)\n")
              }
            }
          }

  cat("\n    →", length(eof_list), "combinaisons (mois x profondeur) calculées\n")
  eof_list
}

# ================================================================
# 2. Extraction des scores
# ================================================================
.extract_scores <- function(eof_obj) {
  eof_obj$right %>%
    as_tibble() %>%
    mutate(year = year(time)) %>%
    select(-time) %>%
    pivot_wider(
      names_from   = PC,
      values_from  = temp,
      names_prefix = "T_"
    )
}

# ================================================================
# 3. Flatten : 1 ligne par cohorte
# ================================================================
.flatten_monthly_depth_scores <- function(eof_list, cohort_years) {

  depth_levels <- sort(unique(
    as.numeric(gsub(".*_depth_", "", names(eof_list)))
  ))

  map_dfr(cohort_years, function(coh) {

    row_vals <- list(year = coh)

    for (j in seq_len(nrow(.month_window))) {

      m_num     <- .month_window$month_num[j]
      lbl       <- .month_window$month_label[j]
      target_yr <- coh + .month_window$year_shift[j]

      for (d in depth_levels) {

        key      <- sprintf("month_%02d_depth_%g", m_num, d)
        d_suffix <- paste0("d", d)

        if (!key %in% names(eof_list)) next

        sc <- .extract_scores(eof_list[[key]]) %>%
          filter(year == target_yr) %>%
          select(-year)

        if (nrow(sc) == 0) {
          sc <- .extract_scores(eof_list[[key]]) %>%
            select(-year) %>%
            slice(0) %>%
            add_row()
        }

        renamed <- sc %>%
          rename_with(~ paste0(., "_", lbl, "_", d_suffix))

        row_vals <- c(row_vals, as.list(renamed[1, , drop = FALSE]))
      }
    }

    as_tibble(row_vals)
  })
}

# ================================================================
# FONCTION PRINCIPALE
# ================================================================
run_eof_pipeline <- function(df,
                              n_pc_start = 1,
                              n_pc_end   = 63) {

  stopifnot(
    n_pc_start >= 1,
    n_pc_start <= n_pc_end,
    all(c("lon", "lat", "depth", "time", "temp") %in% names(df))
  )

  cat(">>> [1/3] EOFs interannuelles (temp brute, par mois x profondeur)\n")
  eof_list <- .build_monthly_depth_eof_list(
    df, n_pc_start, n_pc_end, out_dir = dir_eof)

  cat(">>> [2/3] Extraction des années de cohorte\n")
  cohort_years <- sort(unique(unlist(
    lapply(eof_list, function(obj) year(obj$right$time))
  )))
  cat("    →", length(cohort_years), "années :", range(cohort_years), "\n")

  cat(">>> [3/3] Pivot final (1 ligne par cohorte)\n")
  data_eof_flatten <- .flatten_monthly_depth_scores(eof_list, cohort_years)

  cat("\n✔ Pipeline terminé\n")
  cat("  Dimensions data_eof_flatten :", nrow(data_eof_flatten),
      "x", ncol(data_eof_flatten), "\n")
  cat("  Colonnes EOF                :", ncol(data_eof_flatten) - 1, "\n")

  data_eof_flatten
}