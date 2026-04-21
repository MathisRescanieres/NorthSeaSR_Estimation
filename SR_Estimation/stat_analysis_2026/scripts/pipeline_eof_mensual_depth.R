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
# STRUCTURE SORTIE EOF
# ================================================================

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

.plot_cum_var <- function(eof_obj, title, file_out) {

  var_df <- eof_obj$sdev %>%
    as_tibble() %>%
    mutate(PC = as.integer(PC)) %>%
    arrange(PC) %>%
    mutate(cum_var = cumsum(r2))

  pc_99 <- which(var_df$cum_var >= 0.99)[1]

  p <- ggplot(var_df, aes(x = PC, y = cum_var)) +
    geom_col(fill = "steelblue", alpha = 0.7) +
    geom_line(color = "black") +
    geom_point(color = "black") +
    geom_hline(yintercept = 0.99, linetype = "dashed", color = "red") +
    geom_vline(xintercept = pc_99, linetype = "dotted", color = "red") +
    scale_x_continuous(breaks = var_df$PC) +
    labs(
      title = title,
      x = "Principal Component",
      y = "Cumulative explained variance"
    ) +
    theme_minimal()

  ggsave(file_out, p, device = "pdf", width = 7, height = 4)

  var_df
}

# ================================================================
# 1. EOF par mois x profondeur
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

    if (n_times_m < 2) next

    for (d in depth_levels) {

      key <- sprintf("month_%02d_depth_%g", m, d)
      cat("  →", key, "\n")

      df_md <- df_m %>% filter(depth == d)

      n_grid <- df_md %>% filter(time == first(df_md$time)) %>% nrow()

      valid_times <- df_md %>%
        group_by(time) %>%
        summarise(n = n(), .groups = "drop") %>%
        filter(n == n_grid) %>%
        pull(time)

      df_md <- df_md %>% filter(time %in% valid_times)

      if (length(unique(df_md$time)) < 2) next

      n_pc_safe <- min(n_pc_end, length(unique(df_md$time)), n_grid)
      if (n_pc_safe < n_pc_start) next

      eof_obj <- tryCatch(
        metR::EOF(temp ~ lon + lat | time,
                  data = df_md,
                  n = n_pc_start:n_pc_safe),
        error = function(e) NULL,
        warning = function(w) {
          metR::EOF(temp ~ lon + lat | time,
                    data = df_md,
                    n = n_pc_start:n_pc_safe)
        }
      )

      if (is.null(eof_obj)) next

      depth_label <- .get_depth_label(d)
      month_label_dir <- sprintf("month_%02d", m)

      dir_map_d <- file.path(dir_maps, month_label_dir, depth_label)
      dir_ts_d  <- file.path(dir_ts, month_label_dir, depth_label)
      dir_var_d <- file.path(dir_var, month_label_dir, depth_label)

      ensure_dir(dir_map_d)
      ensure_dir(dir_ts_d)
      ensure_dir(dir_var_d)

      val_col <- "temp"

      # # ======================================================
      # # PLOTS PAR PC
      # # ======================================================

      # for (pc_name in unique(eof_obj$left$PC)) {

      #   df_map <- eof_obj$left %>% filter(PC == pc_name)

      #   p_map <- ggplot(df_map, aes(lon, lat, fill = .data[[val_col]])) +
      #     geom_tile() +
      #     scale_fill_viridis_c(option = "plasma") +
      #     coord_fixed() +
      #     labs(title = paste0("EOF ", pc_name, " | ", key)) +
      #     theme_minimal()

      #   ggsave(file.path(dir_map_d, paste0(key, "_", pc_name, ".pdf")),
      #          p_map, width = 7, height = 5)

      #   df_ts <- eof_obj$right %>% filter(PC == pc_name)

      #   p_ts <- ggplot(df_ts, aes(time, .data[[val_col]])) +
      #     geom_line(color = "steelblue") +
      #     labs(title = paste0("EOF TS ", pc_name, " | ", key)) +
      #     theme_minimal()

      #   ggsave(file.path(dir_ts_d, paste0(key, "_", pc_name, "_ts.pdf")),
      #          p_ts, width = 7, height = 4)
      # }

      # # =================
      # # VARIANCE CUMULÉE
      # # =================

      # var_file <- file.path(dir_var_d, paste0(key, "_cumvar.pdf"))

      # .plot_cum_var(
      #   eof_obj,
      #   title = paste0("Cumulative variance | ", key),
      #   file_out = var_file
      # )

      eof_list[[key]] <- eof_obj

      cat("    ✓ OK\n")
    }
  }

  cat("\n→", length(eof_list), "EOF calculés\n")
  eof_list
}

# ================================================================
# 2. extraction scores
# ================================================================

.extract_scores <- function(eof_obj) {
  eof_obj$right %>%
    as_tibble() %>%
    mutate(year = year(time)) %>%
    select(-time) %>%
    pivot_wider(
      names_from = PC,
      values_from = temp,
      names_prefix = "T_"
    )
}

# ================================================================
# 3. flatten cohorte
# ================================================================

.flatten_monthly_depth_scores <- function(eof_list, cohort_years) {

  depth_levels <- sort(unique(
    as.numeric(gsub(".*_depth_", "", names(eof_list)))
  ))

  map_dfr(cohort_years, function(coh) {

    row_vals <- list(year = coh)

    for (j in seq_len(nrow(.month_window))) {

      m_num <- .month_window$month_num[j]
      lbl <- .month_window$month_label[j]
      target_yr <- coh + .month_window$year_shift[j]

      for (d in depth_levels) {

        key <- sprintf("month_%02d_depth_%g", m_num, d)
        d_suffix <- paste0("d", d)

        if (!key %in% names(eof_list)) next

        sc <- .extract_scores(eof_list[[key]]) %>%
          filter(year == target_yr) %>%
          select(-year)

        if (nrow(sc) == 0) {
          sc <- .extract_scores(eof_list[[key]]) %>%
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
                              n_pc_start = 1,
                              n_pc_end = 63) {

  stopifnot(all(c("lon","lat","depth","time","temp") %in% names(df)))

  cat(">>> EOF computation\n")

  eof_list <- .build_monthly_depth_eof_list(
    df, n_pc_start, n_pc_end, out_dir = dir_eof
  )

  cat(">>> cohort extraction\n")

  cohort_years <- sort(unique(unlist(
    lapply(eof_list, function(x) year(x$right$time))
  )))

  cat(">>> flatten\n")

  out <- .flatten_monthly_depth_scores(eof_list, cohort_years)

  cat("\n✔ DONE\n")

  out
}