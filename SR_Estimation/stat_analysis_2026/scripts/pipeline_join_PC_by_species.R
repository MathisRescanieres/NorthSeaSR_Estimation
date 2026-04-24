# ==============================================================================
# pipeline_join_PC_by_species.R
# ==============================================================================
run_species_prejoin <- function(sp,
                                depths,
                                r2_thresh,
                                data_expanded,
                                data_eof_flatten,
                                dir_r2,
                                dir_out,
                                detrending,        # obligatoire : "none", "global_mean", "linear_mean"
                                trend_params = NULL,
                                temp_bar = NULL) {

  # Warnings 
  stopifnot(detrending %in% c("none", "global_mean", "linear_mean"))
  if (detrending != "none" && is.null(trend_params))
    stop("trend_params est requis quand detrending != 'none'")
  
  if (detrending == "linear_mean" && is.null(temp_bar))
    stop("temp_bar est requis quand detrending == 'linear_mean'")
  
  # Lecture si chemin fourni
  if (is.character(trend_params)) trend_params <- read.csv(trend_params)
  
  if (is.character(temp_bar))     temp_bar     <- read.csv(temp_bar)

  cat("Processing:", sp, "\n")

  # Variance table
  r2_tables <- map(
    setNames(1:12, sprintf("month_%02d", 1:12)),
    ~ read.csv(file.path(dir_r2, sprintf("r2_month_%02d.csv", .x))))

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

  # Fonction : colonnes EOF à garder pour une espèce
  .get_cols_to_keep <- function(r2_tables, depths, r2_thresh) {
    cols_keep <- c()
    for (m in 1:12) {
      month_key <- sprintf("month_%02d", m)
      r2_tab    <- r2_tables[[month_key]] %>% filter(depth %in% depths)
      for (i in seq_len(nrow(r2_tab))) {
        d        <- r2_tab$depth[i]
        r2_row   <- r2_tab[i, grepl("^PC_", names(r2_tab)), drop = FALSE]
        r2_vec   <- unlist(r2_row)
        r2_vec   <- r2_vec[!is.na(r2_vec)]
        above    <- which(r2_vec >= r2_thresh)
        if (length(above) == 0) next
        k        <- max(above)
        pc_nums  <- gsub("PC_", "", names(r2_vec)[1:k])
        d_suffix <- paste0("d", d)
        month_lbl <- .month_window$month_label[.month_window$month_num == m]
        new_cols <- outer(
          paste0("T_PC", pc_nums),
          paste0("_", month_lbl, "_", d_suffix),
          paste0
        ) %>% as.vector()
        cols_keep <- union(cols_keep, new_cols)
      }
    }
    cols_keep
  }

  # Colonnes EOF pertinentes
  cols_keep <- .get_cols_to_keep(r2_tables, depths, r2_thresh)
  cat("  Colonnes EOF retenues :", length(cols_keep), "\n")

  if (length(cols_keep) == 0) {
    warning("Aucune colonne retenue pour ", sp, " — vérifier depths et r2_thresh")
    return(invisible(NULL))
  }

  # Sous-ensemble EOF
  eof_sp <- data_eof_flatten %>%
    select(year, any_of(cols_keep))

  # Données espèce
  data_sp <- data_expanded %>%
    filter(Species == sp) %>%
    mutate(Age_x_Lngt_sc = as.numeric(scale(Age_sc * LngtClassGrouped_sc)))

  if (nrow(data_sp) == 0) {
    warning("Aucun individu trouvé pour ", sp)
    return(invisible(NULL))
  }
  cat("  Individus :", nrow(data_sp), "\n")

  # Jointure EOF
  data_sp_joined <- data_sp %>%
    left_join(eof_sp, by = c("Cohorte_num" = "year"))

  if (detrending == "global_mean") {

    # 12 mois x nb_couches colonnes : clim_jan_d1, clim_jan_d3, ...
    clim_wide <- trend_params %>%
      filter(depth %in% depths) %>%
      mutate(month_label = c("jan","feb","mar","apr","may","jun",
                             "jul","aug","sep","oct","nov","dec")[month],
             col_name = paste0("clim_", month_label, "_d", depth)) %>%
      select(col_name, temp_clim) %>%
      pivot_wider(names_from = col_name, values_from = temp_clim)

    # Même valeur pour tous les individus
    data_sp_joined <- data_sp_joined %>%
      bind_cols(clim_wide[rep(1, nrow(data_sp_joined)), , drop = FALSE])

  } else if (detrending == "linear_mean") {

    tp <- trend_params  # copie locale

    cohortes <- unique(data_sp_joined$Cohorte_num)

    trend_cov <- map_dfr(cohortes, function(y) {

      row <- list(Cohorte_num = y)

      for (j in seq_len(nrow(.month_window))) {
        lbl       <- .month_window$month_label[j]
        m_num     <- .month_window$month_num[j]
        yr_target <- y + .month_window$year_shift[j]

        for (d in depths) {
          params <- tp %>%
            filter(month == m_num, depth == d)
          if (nrow(params) == 0) next
          row[[paste0("trend_", lbl, "_d", d)]] <-
            params$beta + params$alpha * yr_target
        }
      }

      as_tibble(row)
    })

    data_sp_joined <- data_sp_joined %>%
      left_join(trend_cov, by = "Cohorte_num")

    tb <- temp_bar  # copie locale

    obs_cov <- map_dfr(cohortes, function(y) {
      row <- list(Cohorte_num = y)
      for (j in seq_len(nrow(.month_window))) {
        lbl       <- .month_window$month_label[j]
        m_num     <- .month_window$month_num[j]
        yr_target <- y + .month_window$year_shift[j]
        for (d in depths) {
          val <- tb %>%
            filter(month == m_num, depth == d, year == yr_target) %>%
            pull(temp_bar)
          row[[paste0("obs_", lbl, "_d", d)]] <- if (length(val) == 1) val else NA_real_
        }
      }
      as_tibble(row)
    })

    data_sp_joined <- data_sp_joined %>%
      left_join(obs_cov, by = "Cohorte_num")
  }

  # ============================================================
  # SELECTION FINALE DES COLONNES
  # ============================================================

  cols_final <- c("Numeric_sex", "Age_sc", "LngtClassGrouped_sc", "Age_x_Lngt_sc",
                  "Cohorte_num_sc", "Cohorte_fact", "Area")

  eof_cols <- grep("^T_PC", names(data_sp_joined), value = TRUE)
  det_cols <- switch(detrending,
    "none"        = character(0),
    "global_mean" = grep("^clim_",  names(data_sp_joined), value = TRUE),
    "linear_mean" = c(grep("^trend_", names(data_sp_joined), value = TRUE),
                      grep("^obs_",   names(data_sp_joined), value = TRUE))
  )

  data_sp_joined <- data_sp_joined %>%
    select(all_of(c(cols_final, eof_cols, det_cols)))

  # Sauvegarde
  dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)
  out_path <- file.path(dir_out, paste0(gsub(" ", "_", sp), ".rds"))
  saveRDS(data_sp_joined, file = out_path)
  cat("  Sauvegardé :", out_path, "\n")

  rm(data_sp, data_sp_joined, eof_sp)
  gc()
  invisible(out_path)
}