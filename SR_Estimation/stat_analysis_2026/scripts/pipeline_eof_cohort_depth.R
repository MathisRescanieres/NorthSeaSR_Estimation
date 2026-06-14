# ==============================================================================
# pipeline_eof_cohort_depth.R
# Pour chaque cohorte c et chaque profondeur d, calcule une EOF sur le champ
# spatial (lon x lat) à travers les 18 mois de la fenêtre périnatale.
# Seules les PC dont r2 >= threshold_r2 sont conservées dans la sortie.
# ==============================================================================

library(dplyr)
library(tidyr)
library(lubridate)
library(purrr)
library(metR)
library(conflicted)

ensure_dir <- function(dir) {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
}

dir_eof   <- "../results_eof_cohort_depth/REOF_with_trend"
dir_table <- file.path(dir_eof, "table_explained_var")
ensure_dir(dir_eof)
ensure_dir(dir_table)

# ── Fenêtre périnatale de 18 mois ─────────────────────────────────────────────
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

# ── 1b. Normalisation des signes par cohorte ─────────────────────────────────
# Pour chaque cohorte, on cherche le point de référence (ref_lon, ref_lat)
# dans l'intersection des grilles lon/lat disponibles à TOUTES les profondeurs.
# Si ce point existe dans l'intersection, il est utilisé pour toutes les couches.
# Si l'intersection est vide, on prend le point commun le plus proche.
# Le signe de chaque vecteur propre (loading + score) est retourné si le
# loading au point de référence est négatif.

.normalize_signs_by_cohort <- function(eof_list,
                                        ref_lon = -2.625,
                                        ref_lat = 47.375) {
  parsed <- tibble(
    key   = names(eof_list),
    coh   = as.integer(gsub("coh_(\\d+)_depth_.*", "\\1", key)),
    depth = as.numeric(gsub("coh_\\d+_depth_", "", key))
  )

  # ── Point de référence commun — calculé une seule fois ───────────────────
  # La grille lon/lat disponible à chaque profondeur est fixée par la
  # bathymétrie et ne varie pas entre cohortes. On prend une cohorte
  # quelconque pour extraire les grilles par profondeur.

  ref_coh   <- parsed$coh[1]
  keys_ref  <- parsed %>% dplyr::filter(coh == ref_coh)

  grids <- lapply(keys_ref$key, function(k)
    eof_list[[k]]$left %>% distinct(lon, lat)
  )

  common_grid <- grids[[1]]
  for (g in grids[-1])
    common_grid <- dplyr::inner_join(common_grid, g, by = c("lon", "lat"))

  if (nrow(common_grid) == 0)
    stop("Aucun point commun entre les couches de profondeur. ",
         "Impossible de normaliser les signes.")

  ref_point <- common_grid %>%
    mutate(dist = abs(lon - ref_lon) + abs(lat - ref_lat)) %>%
    slice_min(dist, n = 1, with_ties = FALSE)

  cat("  Point de référence commun (bathymétrie) :",
      round(ref_point$lon, 3), "/", round(ref_point$lat, 3), "\n")

  # ── Normalisation pour chaque (cohorte, profondeur, PC) ──────────────────
  for (k in names(eof_list)) {
    pcs <- unique(eof_list[[k]]$left$PC)

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

  eof_list
}


.build_cohort_depth_eof_list <- function(df, n_pc_start, n_pc_end,
                                         rotate_fct = NULL) {
  depth_levels <- sort(unique(df$depth))
  all_years    <- sort(unique(year(df$time)))
  cohort_years <- all_years[all_years >= min(all_years) + 1 &
                            all_years <= max(all_years) - 1]

  cat("    Couches détectées :", depth_levels, "\n")
  cat("    Cohortes possibles :", length(cohort_years), "\n")

  eof_list <- list()

  for (coh in cohort_years) {
    cat("\n  ── Cohorte", coh, "──\n")
    window_dates <- as.Date(paste0(coh, "-01-01")) %m+% months(.month_window$offset)
    df_win <- df %>% dplyr::filter(time %in% window_dates)

    if (length(unique(df_win$time)) < 2) {
      warning("Cohorte ", coh, " : pas assez de pas de temps.")
      next
    }

    for (d in depth_levels) {
      key    <- sprintf("coh_%d_depth_%g", coh, d)
      df_wd  <- df_win %>% dplyr::filter(depth == d)
      n_grid  <- df_wd %>% dplyr::filter(time == first(df_wd$time)) %>% nrow()
      n_times <- length(unique(df_wd$time))

      if (n_times < 2 || n_grid < 2) next
      n_pc_safe <- min(n_pc_end, n_times, n_grid)
      if (n_pc_safe < n_pc_start) next

      eof_obj <- tryCatch(
        if (!is.null(rotate_fct)) {
          metR::EOF(temp ~ lon + lat | time, data = df_wd,
                    n = n_pc_start:n_pc_safe, rotate = rotate_fct)
        } else {
          metR::EOF(temp ~ lon + lat | time, data = df_wd,
                    n = n_pc_start:n_pc_safe)
        },
        error   = function(e) NULL,
        warning = function(w) tryCatch(
          if (!is.null(rotate_fct)) {
            metR::EOF(temp ~ lon + lat | time, data = df_wd,
                      n = n_pc_start:n_pc_safe, rotate = rotate_fct)
          } else {
            metR::EOF(temp ~ lon + lat | time, data = df_wd,
                      n = n_pc_start:n_pc_safe)
          },
          error = function(e) NULL
        )
      )

      if (is.null(eof_obj)) next
      eof_list[[key]] <- eof_obj
      cat("    ✓", key, "\n")
    }
  }

  cat("\n→", length(eof_list), "EOF calculés\n")
  eof_list
}

# ── 2. Flatten : 1 ligne par cohorte ─────────────────────────────────────────
# threshold_r2 : seules les PC avec r2 individuel >= threshold_r2 sont gardées.

.flatten_cohort_depth_scores <- function(eof_list, threshold_r2 = 0.0) {

  parsed <- tibble(
    key   = names(eof_list),
    coh   = as.integer(gsub("coh_(\\d+)_depth_.*", "\\1", key)),
    depth = as.numeric(gsub("coh_\\d+_depth_", "", key))
  )

  cohort_years <- sort(unique(parsed$coh))

  map_dfr(cohort_years, function(coh) {

    row_vals <- list(year = coh)
    keys_coh <- parsed %>% dplyr::filter(coh == !!coh)
    window_dates <- as.Date(paste0(coh, "-01-01")) %m+% months(.month_window$offset)
    date_labels  <- tibble(time = window_dates,
                           month_label = .month_window$month_label)

    for (i in seq_len(nrow(keys_coh))) {
      k        <- keys_coh$key[i]
      d        <- keys_coh$depth[i]
      d_suffix <- paste0("d", d)

      # Sélection des PC retenues
      retained_pcs <- eof_list[[k]]$sdev %>%
        dplyr::filter(r2 >= threshold_r2) %>%
        pull(PC)

      if (length(retained_pcs) == 0) next

      sc <- eof_list[[k]]$right %>%
        as_tibble() %>%
        dplyr::filter(PC %in% retained_pcs) %>%
        left_join(date_labels, by = "time")

      if (nrow(sc) == 0) next

      sc_wide <- sc %>%
        dplyr::select(-time) %>%
        pivot_wider(names_from = PC, values_from = temp,
                    names_prefix = "T_") %>%
        pivot_longer(cols = -month_label, names_to = "pc_col",
                     values_to = "value") %>%
        mutate(col_name = paste0(pc_col, "_", month_label, "_", d_suffix)) %>%
        dplyr::select(col_name, value)

      for (r in seq_len(nrow(sc_wide)))
        row_vals[[sc_wide$col_name[r]]] <- sc_wide$value[r]
    }

    as_tibble(row_vals)
  })
}

# ── PIPELINE PRINCIPAL ────────────────────────────────────────────────────────

run_eof_pipeline <- function(df,
                                          n_pc_start   = 1,
                                          n_pc_end     = 18,
                                          threshold_r2 = 0.15,
                                          rotate_fct   = NULL) {

  stopifnot(all(c("lon", "lat", "depth", "time", "temp") %in% names(df)))

  cat(">>> [1/3] EOF par cohorte x profondeur\n")
  eof_list <- .build_cohort_depth_eof_list(df, n_pc_start, n_pc_end,
                                           rotate_fct = rotate_fct)

  cat(">>> [2/3] Normalisation des signes par cohorte\n")
  eof_list <- .normalize_signs_by_cohort(eof_list)

  cat(">>> [3/3] Flatten (threshold_r2 =", threshold_r2, ")\n")
  out <- .flatten_cohort_depth_scores(eof_list, threshold_r2 = threshold_r2)

  cat("\n✔ Pipeline terminé\n")
  cat("  Dimensions :", nrow(out), "x", ncol(out), "\n")

  list(
    flatten  = out,       # tableau XGBoost
    eof_list = eof_list   # tous les objets EOF par cohorte x profondeur
  )
}