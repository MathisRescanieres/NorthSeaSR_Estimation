# ==============================================================================
# pipeline_eof_interannual_depth.R
# ==============================================================================

library(dplyr)
library(tidyr)
library(lubridate)
library(purrr)
library(metR)
library(ggplot2)
library(conflicted)

ensure_dir <- function(dir) {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
}

dir_eof <- "../results_eof_depth/EOF_with_trend"
ensure_dir(dir_eof)

dir_table <- file.path(dir_eof, "table_explained_var")
ensure_dir(dir_table)

dir_maps <- file.path(dir_eof, "maps")
dir_ts   <- file.path(dir_eof, "timeseries")
dir_var  <- file.path(dir_eof, "cum_var")

ensure_dir(dir_maps)
ensure_dir(dir_ts)
ensure_dir(dir_var)

# ================================================================
# utilitaires
# ================================================================

.get_depth_label <- function(d) {
  paste0("depth_", gsub("\\.", "_", d))
}

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
      TRUE ~ 0L
    )
  )

# ================================================================
# VARIANCE CUMULÉE
# ================================================================

.plot_cum_var <- function(eof_obj, title, file_out, threshold_var_plot) {

  var_df <- eof_obj$sdev %>%
    as_tibble() %>%
    mutate(PC = as.integer(PC)) %>%
    arrange(PC) %>%
    mutate(cum_var = cumsum(r2))

  pc_thresh <- which(var_df$cum_var >= threshold_var_plot)[1]

  p <- ggplot(var_df, aes(x = PC, y = cum_var)) +
    geom_col(fill = "steelblue", alpha = 0.7) +
    geom_line(color = "black") +
    geom_point(color = "black") +
    geom_hline(yintercept = threshold_var_plot, linetype = "dashed", color = "red") +
    geom_vline(xintercept = pc_thresh, linetype = "dotted", color = "red") +
    scale_x_continuous(breaks = var_df$PC) +
    labs(title = title, x = "Principal Component",
         y = "Cumulative explained variance") +
    theme_minimal()

  ggsave(file_out, p, device = "pdf", width = 7, height = 4)
  var_df
}

# ================================================================
# NORMALISATION DES SIGNES
# Impose que le loading au point de référence (embouchure de la Vilaine)
# soit positif pour chaque PC de chaque EOF.
# La grille étant fixée par la bathymétrie, le point de référence
# est recalculé par EOF (il peut varier selon la profondeur).
# ================================================================

.normalize_signs <- function(eof_list,
                              ref_lon = -2.625,
                              ref_lat = 47.375) {

  for (k in names(eof_list)) {

    pcs <- unique(eof_list[[k]]$left$PC)

    ref_point <- eof_list[[k]]$left %>%
      distinct(lon, lat) %>%
      mutate(dist = abs(lon - ref_lon) + abs(lat - ref_lat)) %>%
      slice_min(dist, n = 1, with_ties = FALSE)

    for (pc in pcs) {
      loading_ref <- eof_list[[k]]$left %>%
        dplyr::filter(PC == pc,
                      lon == ref_point$lon,
                      lat == ref_point$lat) %>%
        pull(temp)

      if (length(loading_ref) == 0 || is.na(loading_ref)) next

      if (loading_ref < 0) {
        eof_list[[k]]$left <- eof_list[[k]]$left %>%
          mutate(temp = ifelse(PC == pc, -temp, temp))
        eof_list[[k]]$right <- eof_list[[k]]$right %>%
          mutate(temp = ifelse(PC == pc, -temp, temp))
      }
    }
  }

  cat("  Signes normalisés pour", length(eof_list), "EOFs\n")
  eof_list
}

# ================================================================
# 1. EOF par mois x profondeur
# ================================================================

.build_monthly_depth_eof_list <- function(df, n_pc_start, n_pc_end,
                                          threshold_var_plot = 0.99,
                                          rotate_fct = NULL,
                                          out_dir = NULL) {

  if (!is.null(out_dir)) ensure_dir(out_dir)

  depth_levels <- sort(unique(df$depth))
  cat("    Couches détectées :", depth_levels, "\n")

  eof_list <- list()

  for (m in 1:12) {

    cat("\n  ── Mois", m, "──\n")
    df_m      <- df %>% dplyr::filter(month(time) == m)
    n_times_m <- length(unique(df_m$time))
    cat("    Pas de temps disponibles :", n_times_m, "\n")
    if (n_times_m < 2) next

    r2_month <- list()

    for (d in depth_levels) {

      key   <- sprintf("month_%02d_depth_%g", m, d)
      df_md <- df_m %>% dplyr::filter(depth == d)

      n_grid <- df_md %>% dplyr::filter(time == first(df_md$time)) %>% nrow()

      valid_times <- df_md %>%
        group_by(time) %>%
        summarise(n = n(), .groups = "drop") %>%
        dplyr::filter(n == n_grid) %>%
        pull(time)

      df_md <- df_md %>% dplyr::filter(time %in% valid_times)
      if (length(unique(df_md$time)) < 2) next

      n_pc_safe <- min(n_pc_end, length(unique(df_md$time)), n_grid)
      if (n_pc_safe < n_pc_start) next

      eof_obj <- tryCatch(
        if (!is.null(rotate_fct)) {
          metR::EOF(temp ~ lon + lat | time, data = df_md,
                    n = n_pc_start:n_pc_safe, rotate = rotate_fct)
        } else {
          metR::EOF(temp ~ lon + lat | time, data = df_md,
                    n = n_pc_start:n_pc_safe)
        },
        error   = function(e) NULL,
        warning = function(w) tryCatch(
          if (!is.null(rotate_fct)) {
            metR::EOF(temp ~ lon + lat | time, data = df_md,
                      n = n_pc_start:n_pc_safe, rotate = rotate_fct)
          } else {
            metR::EOF(temp ~ lon + lat | time, data = df_md,
                      n = n_pc_start:n_pc_safe)
          },
          error = function(e) NULL
        )
      )

      if (is.null(eof_obj)) next

      eof_list[[key]] <- eof_obj
      cat("    ✓ OK\n")
    }

    if (length(r2_month) > 0) {
      csv_file <- file.path(dir_table, sprintf("r2_month_%02d.csv", m))
      write.csv(bind_rows(r2_month), csv_file, row.names = FALSE)
      cat("    → CSV :", csv_file, "\n")
    }
  }

  cat("\n→", length(eof_list), "EOF calculés\n")
  eof_list
}

# ================================================================
# 2. extraction scores
# ================================================================

.extract_scores <- function(eof_obj, threshold_r2 = 0.0) {
  retained_pcs <- eof_obj$sdev %>%
    dplyr::filter(r2 >= threshold_r2) %>%
    pull(PC)

  eof_obj$right %>%
    as_tibble() %>%
    dplyr::filter(PC %in% retained_pcs) %>%
    mutate(year = year(time)) %>%
    select(-time) %>%
    pivot_wider(names_from = PC, values_from = temp, names_prefix = "T_")
}

# ================================================================
# 3. flatten cohorte
# ================================================================

.flatten_monthly_depth_scores <- function(eof_list, cohort_years,
                                          threshold_r2 = 0.0) {

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

        sc <- .extract_scores(eof_list[[key]], threshold_r2 = threshold_r2) %>%
          dplyr::filter(year == target_yr) %>%
          select(-year)

        if (nrow(sc) == 0) {
          sc <- .extract_scores(eof_list[[key]], threshold_r2 = threshold_r2) %>%
            slice(0) %>%
            select(-year) %>%
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
# PIPELINE PRINCIPAL
# ================================================================

run_eof_pipeline <- function(df,
                              n_pc_start         = 1,
                              n_pc_end           = 10,
                              threshold_r2       = NULL,
                              threshold_var_plot = 0.99,
                              rotate_fct         = NULL) {

  stopifnot(all(c("lon", "lat", "depth", "time", "temp") %in% names(df)))

  cat(">>> [1/4] EOF par mois x profondeur\n")
  eof_list <- .build_monthly_depth_eof_list(
    df, n_pc_start, n_pc_end,
    out_dir            = dir_eof,
    threshold_var_plot = threshold_var_plot,
    rotate_fct         = rotate_fct
  )

  cat(">>> [2/4] Normalisation des signes\n")
  eof_list <- .normalize_signs(eof_list)

  cat(">>> [3/4] Extraction des cohortes\n")
  cohort_years <- sort(unique(unlist(
    lapply(eof_list, function(x) year(x$right$time))
  )))

  cat(">>> [4/4] Flatten (threshold_r2 =", threshold_r2, ")\n")
  out <- .flatten_monthly_depth_scores(eof_list, cohort_years,
                                       threshold_r2 = threshold_r2)

  cat("\n✔ DONE\n")
  cat("  Dimensions :", nrow(out), "x", ncol(out), "\n")
  list(
    flatten  = out,
    eof_list = eof_list
  )
}
