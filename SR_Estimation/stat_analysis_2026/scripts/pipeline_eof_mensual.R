# ==============================================================================
# pipeline_eof_interannual.R
# ------------------------------------------------------------------------------
# Construit data_eof_flatten : un tableau 1 ligne par cohorte contenant les
# scores des EOFs interannuelles (1 EOF par mois sur toute la série) pour
# T_mean, T_var, T_grad
#
# Usage :
#   source("pipeline_eof_interannual.R")
#   data_eof_flatten <- run_eof_pipeline(n_pc_start = 1, n_pc_end = 65)
# ==============================================================================

library(dplyr)
library(tidyr)
library(lubridate)
library(purrr)

# -- Création d'un dossier de sauvegarde ---
ensure_dir <- function(dir) {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
}

# EOF
dir_eof <- "../results_mensual_PC1_to_PC5/EOF"
ensure_dir(dir_eof)

# -- Fenêtre de 18 mois --
.month_window <- tibble(
  offset      = c(-3, -2, -1, 0:11, 12, 13, 14),
  month_label = c(
    "oct_m1", "nov_m1", "dec_m1",
    "jan", "feb", "mar", "apr", "may", "jun",
    "jul", "aug", "sep", "oct", "nov", "dec",
    "jan_p1", "feb_p1", "mar_p1"
  )
)

# Correspondance label --> (numéro de mois, décalage année)
.month_window <- .month_window %>%
  mutate(
    month_num  = month(as.Date(paste0("2000-01-01")) %m+% months(offset)),
    year_shift = case_when(
      grepl("_m1$", month_label) ~ -1L,
      grepl("_p1$", month_label) ~  1L,
      TRUE                       ~  0L
    )
  )

# -- 1. EOFs interannuelles : 1 EOF par mois sur toute la série --
#
# Pour chaque mois m :
#   - on garde uniquement les années où le mois m est présent dans df_T
#   - on calcule l'EOF sur ces pas de temps
#   - on retourne une liste nommée par numéro de mois (1..12)
#
.build_monthly_eof_list <- function(df_T, val_col, prefix, n_pc_start, n_pc_end, out_dir = NULL) {

  ensure_dir <- function(dir) if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  if (!is.null(out_dir)) ensure_dir(out_dir)

  eof_list <- vector("list", 12L)
  names(eof_list) <- sprintf("month_%02d", 1:12)

  for (m in 1:12) {

    # Filtrer les pas de temps du mois m
    df_m <- df_T %>% filter(month(time) == m)

    if (length(unique(df_m$time)) < 2) {
      warning("Mois ", m, " (", prefix, ") : pas assez de pas de temps — ignoré.")
      next
    }

    # Vérifier la cohérence lon/lat : on retire les années incomplètes
    n_grid  <- df_m %>% filter(time == first(df_m$time)) %>% nrow()
    valid_times <- df_m %>%
      group_by(time) %>%
      summarise(n = n(), .groups = "drop") %>%
      filter(n == n_grid) %>%
      pull(time)

    df_m <- df_m %>% filter(time %in% valid_times)

    if (length(unique(df_m$time)) < 2) {
      warning("Mois ", m, " (", prefix, ") : pas assez d'années complètes — ignoré.")
      next
    }

    eof_formula <- as.formula(paste0(val_col, " ~ lon + lat | time"))

    eof_obj <- tryCatch(
      metR::EOF(eof_formula, data = df_m, n = n_pc_start:n_pc_end),
      error = function(e) {
        warning("Mois ", m, " (", prefix, ") : erreur EOF — ", conditionMessage(e))
        NULL
      }
    )

    if (!is.null(eof_obj)) {
      eof_list[[sprintf("month_%02d", m)]] <- eof_obj

      # # ---- Sauvegarde des cartes EOF ----
      # if (!is.null(out_dir)) {
      #     # Dossier pour cartes
      #     dir_map <- file.path(out_dir, "maps")
      #     ensure_dir(dir_map)
          
      #     # Dossier pour séries temporelles
      #     dir_ts  <- file.path(out_dir, "timeseries")
      #     ensure_dir(dir_ts)

      #     for (pc_name in unique(eof_obj$left$PC)) {
      #         # --- Carte EOF ---
      #         df_map <- eof_obj$left %>% filter(PC == pc_name)
      #         file_name_map <- file.path(dir_map, paste0(prefix, "_month_", sprintf("%02d", m), "_", pc_name, ".png"))
              
      #         p_map <- ggplot(df_map, aes(x = lon, y = lat, fill = .data[[val_col]])) +
      #             geom_raster() +
      #             scale_fill_viridis_c(option = "plasma") +
      #             coord_fixed() +
      #             labs(title = paste0(prefix, " month_", sprintf("%02d", m), " ", pc_name)) +
      #             theme_minimal()
              
      #         ggsave(file_name_map, p_map, width = 6, height = 4)

      #         # --- Série temporelle du PC ---
      #         df_ts <- eof_obj$right %>% select(time, PC, all_of(val_col)) %>%
      #                 filter(PC == pc_name)
              
      #         file_name_ts <- file.path(dir_ts, paste0(prefix, "_month_", sprintf("%02d", m), "_", pc_name, "_ts.png"))
              
      #         p_ts <- ggplot(df_ts, aes(x = time, y = .data[[val_col]], color = PC)) +
      #             geom_line() +
      #             labs(title = paste0(prefix, " month_", sprintf("%02d", m), " ", pc_name, " time series")) +
      #             theme_minimal()
              
      #         ggsave(file_name_ts, p_ts, width = 6, height = 4)
      #     }
      # }
    }
  }

  Filter(Negate(is.null), eof_list)
}

# -- 2. Extraction des scores ($right) des objets EOF --
# Retourne un tibble : year | PC1 | PC2 | ... (le "time" de metR::EOF est la date du pas de temps mensuel)

.extract_scores <- function(eof_obj, prefix) {
  val_col <- attr(eof_obj, "value.var")
  eof_obj$right %>%
    as_tibble() %>%
    mutate(year = year(time)) %>%
    select(-time) %>%
    pivot_wider(
      names_from  = PC,
      values_from = all_of(val_col),
      names_prefix = paste0(prefix, "_")
    )
}

# -- 3. Fusion des 3 signaux --> scores mensuels par variable --
# Retourne une liste nommée month_01..month_12, chaque élément étant un tibble
# year | T_mean_PC1 | ... | T_var_PC1 | ... | T_grad_PC1 | ...

.build_monthly_scores <- function(eof_list_T_mean,
                                  eof_list_T_var,
                                  eof_list_T_grad) {

  # Mois présents dans les 3 signaux
  months_all <- Reduce(intersect, list(
    names(eof_list_T_mean),
    names(eof_list_T_var),
    names(eof_list_T_grad)
  ))
  cat("  Mois communs aux 3 signaux :", length(months_all), "\n")

  monthly_scores        <- vector("list", length(months_all))
  names(monthly_scores) <- months_all

  for (m_key in months_all) {
    sc_mean <- .extract_scores(eof_list_T_mean[[m_key]], "T_mean")
    sc_var  <- .extract_scores(eof_list_T_var [[m_key]], "T_var")
    sc_grad <- .extract_scores(eof_list_T_grad[[m_key]], "T_grad")

    monthly_scores[[m_key]] <-
      sc_mean %>%
      left_join(sc_var,  by = "year") %>%
      left_join(sc_grad, by = "year")
  }

  monthly_scores
}

# -- 4. Pivot final : scores mensuels --> 1 ligne par cohorte --
#
# Pour chaque cohorte (year) et chaque label de la fenêtre 18 mois :
#   - on identifie le mois m et le décalage d'année
#   - on va chercher dans monthly_scores[[month_m]] la ligne year + shift
#   - on nomme les colonnes <var>_<PC>_<month_label>

.flatten_monthly_scores <- function(monthly_scores, cohort_years) {

  map_dfr(cohort_years, function(coh) {

    row_vals <- list(year = coh)

    for (j in seq_len(nrow(.month_window))) {

      m_key      <- sprintf("month_%02d", .month_window$month_num[j])
      lbl        <- .month_window$month_label[j]
      target_yr  <- coh + .month_window$year_shift[j]

      if (!m_key %in% names(monthly_scores)) next

      sc <- monthly_scores[[m_key]] %>%
        filter(year == target_yr) %>%
        select(-year)

      if (nrow(sc) == 0) {
        # Année manquante pour ce mois --> NA pour toutes les PC
        sc <- monthly_scores[[m_key]] %>%
          select(-year) %>%
          slice(0) %>%
          add_row()
      }

      # Renommer les colonnes : T_mean_PC1 --> T_mean_PC1_jan
      renamed <- sc %>%
        rename_with(~ paste0(., "_", lbl))

      row_vals <- c(row_vals, as.list(renamed[1, , drop = FALSE]))
    }

    as_tibble(row_vals)
  })
}

# ═══════════════════
# FONCTION PRINCIPALE
# ═══════════════════
#
# Paramètres :
#   n_pc_start     : première composante retenue (défaut : 1)
#   n_pc_end       : dernière composante retenue (défaut : 65)
#   df_T_mean      : data.frame lon / lat / time / T_mean
#   df_T_var       : data.frame lon / lat / time / T_var
#   df_T_grad      : data.frame lon / lat / time / T_grad
#
# Retourne :
#   data_eof_flatten : tibble (n_cohortes × (1 + n_PC × 3 × 18))
#                      colonnes : year | <signal>_PC<n>_<mois>

run_eof_pipeline <- function(n_pc_start   = 1,
                             n_pc_end     = 65,
                             df_T_mean    = df_T_mean,
                             df_T_var     = df_T_var,
                             df_T_grad    = df_T_grad) {

  stopifnot(n_pc_start >= 1, n_pc_start <= n_pc_end)

  cat(">>> [1/4] EOFs interannuelles T_mean\n")
  eof_list_T_mean <- .build_monthly_eof_list(df_T_mean, "T_mean", "T_mean", n_pc_start, n_pc_end, out_dir = dir_eof)
  cat("    →", length(eof_list_T_mean), "mois calculés\n")

  cat(">>> [2/4] EOFs interannuelles T_var\n")
  eof_list_T_var <- .build_monthly_eof_list(df_T_var, "T_var", "T_var", n_pc_start, n_pc_end, out_dir = dir_eof)
  cat("    →", length(eof_list_T_var), "mois calculés\n")

  cat(">>> [3/4] EOFs interannuelles T_grad\n")
  eof_list_T_grad <- .build_monthly_eof_list(df_T_grad, "T_grad", "T_grad", n_pc_start, n_pc_end, out_dir = dir_eof)
  cat("    →", length(eof_list_T_grad), "mois calculés\n")

  cat(">>> [4/4] Fusion & pivot\n")
  monthly_scores   <- .build_monthly_scores(eof_list_T_mean, eof_list_T_var, eof_list_T_grad)
  cohort_years <- sort(unique(unlist(lapply(monthly_scores, function(sc) sc$year))))
  data_eof_flatten <- .flatten_monthly_scores(monthly_scores, cohort_years)

  cat("\n✔ Pipeline terminé\n")
  cat("  Dimensions data_eof_flatten :", nrow(data_eof_flatten), "x", ncol(data_eof_flatten), "\n")
  cat("  Colonnes température        :", ncol(data_eof_flatten) - 1, "\n")

  data_eof_flatten
}
