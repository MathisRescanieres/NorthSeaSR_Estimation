# ==============================================================================
# pipeline_eof_cohort.R
# ------------------------------------------------------------------------------
# Construit data_eof_flatten : un tableau 1 ligne par cohorte contenant les
# scores des EOFs cohort-spécifiques (fenêtre 18 mois) pour T_mean, T_var, T_grad
#
# Usage :
#   source("pipeline_eof_cohort.R")
#   data_eof_flatten <- run_eof_pipeline(n_pc_start = 1, n_pc_end = 17)
# ==============================================================================

library(dplyr)
library(tidyr)
library(lubridate)
library(purrr)

# ── Fenêtre de 18 mois (interne) ──────────────────────────────────────────────
.month_window <- tibble(
  offset      = c(-3, -2, -1, 0:11, 12, 13, 14),
  month_label = c(
    "oct_m1", "nov_m1", "dec_m1",
    "jan", "feb", "mar", "apr", "may", "jun",
    "jul", "aug", "sep", "oct", "nov", "dec",
    "jan_p1", "feb_p1", "mar_p1"
  )
)

# ── 1. Construction des EOFs par cohorte pour un signal ───────────────────────
.build_eof_list <- function(df_T, val_col, prefix, n_pc_start, n_pc_end) {

  all_years    <- sort(unique(year(df_T$time)))
  cohort_years <- all_years[
    all_years >= min(all_years) + 1 &
    all_years <= max(all_years) - 1
  ]

  eof_list        <- vector("list", length(cohort_years))
  names(eof_list) <- paste0("eof_", prefix, "_", cohort_years)

  for (i in seq_along(cohort_years)) {
    coh          <- cohort_years[i]
    window_dates <- as.Date(paste0(coh, "-01-01")) %m+% months(.month_window$offset)
    df_win       <- df_T %>% filter(time %in% window_dates)

    if (length(unique(df_win$time)) < 2) {
      warning("Cohorte ", coh, " (", prefix, ") : pas assez de pas de temps — ignorée.")
      next
    }

    eof_formula  <- as.formula(paste0(val_col, " ~ lon + lat | time"))
    eof_list[[i]] <- tryCatch(
      metR::EOF(eof_formula, data = df_win, n = n_pc_start:n_pc_end),
      error = function(e) {
        warning("Cohorte ", coh, " (", prefix, ") : erreur EOF — ", conditionMessage(e))
        NULL
      }
    )
  }

  Filter(Negate(is.null), eof_list)
}

# ── 2. Pivot wide des scores d'un objet EOF ───────────────────────────────────
.extract_wide <- function(eof_obj, prefix) {
  val_col <- attr(eof_obj, "value.var")
  eof_obj$right %>%
    as_tibble() %>%
    pivot_wider(
      names_from   = PC,
      values_from  = all_of(val_col),
      names_prefix = paste0(prefix, "_")
    )
}

# ── 3. Fusion des 3 signaux par cohorte → liste eof_all_sig ──────────────────
.build_eof_all_sig <- function(eof_list_T_mean, eof_list_T_var, eof_list_T_grad) {

  cohorts_all <- Reduce(intersect, list(
    gsub("eof_T_mean_", "", names(eof_list_T_mean)),
    gsub("eof_T_var_",  "", names(eof_list_T_var)),
    gsub("eof_T_grad_", "", names(eof_list_T_grad))
  ))
  cat("  Cohortes communes aux 3 signaux :", length(cohorts_all), "\n")

  eof_all_sig        <- vector("list", length(cohorts_all))
  names(eof_all_sig) <- paste0("eof_all_sig_", cohorts_all)

  for (coh in cohorts_all) {
    df_mean <- .extract_wide(eof_list_T_mean[[paste0("eof_T_mean_", coh)]], "T_mean")
    df_var  <- .extract_wide(eof_list_T_var [[paste0("eof_T_var_",  coh)]], "T_var")
    df_grad <- .extract_wide(eof_list_T_grad[[paste0("eof_T_grad_", coh)]], "T_grad")

    eof_all_sig[[paste0("eof_all_sig_", coh)]] <-
      df_mean %>%
      left_join(df_var,  by = "time") %>%
      left_join(df_grad, by = "time") %>%
      arrange(time)
  }

  eof_all_sig
}

# ── 4. Pivot final : eof_all_sig → 1 ligne par cohorte ───────────────────────
.flatten_eof_all_sig <- function(eof_all_sig) {

  cohorts   <- names(eof_all_sig)
  coh_years <- as.integer(gsub("eof_all_sig_", "", cohorts))

  map_dfr(seq_along(cohorts), function(i) {
    coh          <- coh_years[i]
    df           <- eof_all_sig[[cohorts[i]]]
    window_dates <- as.Date(paste0(coh, "-01-01")) %m+% months(.month_window$offset)
    date_labels  <- tibble(time = window_dates, month_label = .month_window$month_label)

    df %>%
      left_join(date_labels, by = "time") %>%
      select(-time) %>%
      pivot_longer(cols = -month_label, names_to = "pc_col", values_to = "value") %>%
      mutate(col_name = paste0(pc_col, "_", month_label)) %>%
      select(col_name, value) %>%
      pivot_wider(names_from = col_name, values_from = value) %>%
      mutate(year = coh, .before = 1)
  })
}

# ══════════════════════════════════════════════════════════════════════════════
# FONCTION PRINCIPALE
# ══════════════════════════════════════════════════════════════════════════════
#
# Paramètres :
#   n_pc_start  : première composante retenue (défaut : 1)
#   n_pc_end    : dernière composante retenue — max 17 pour une fenêtre 18 mois
#                 (défaut : 17)
#   df_T_mean   : data.frame lon / lat / time / T_mean
#   df_T_var    : data.frame lon / lat / time / T_var
#   df_T_grad   : data.frame lon / lat / time / T_grad
#
# Retourne :
#   data_eof_flatten : tibble (n_cohortes × (1 + n_PC × 3 × 18))
#                      colonnes : year | <signal>_<PC>_<mois>

run_eof_pipeline <- function(n_pc_start = 1,
                             n_pc_end   = 17,
                             df_T_mean  = df_T_mean,
                             df_T_var   = df_T_var,
                             df_T_grad  = df_T_grad) {

  stopifnot(n_pc_end <= 17, n_pc_start >= 1, n_pc_start <= n_pc_end)

  cat(">>> [1/4] EOFs T_mean\n")
  eof_list_T_mean <- .build_eof_list(df_T_mean, "T_mean", "T_mean", n_pc_start, n_pc_end)
  cat("    →", length(eof_list_T_mean), "EOFs calculées\n")

  cat(">>> [2/4] EOFs T_var\n")
  eof_list_T_var  <- .build_eof_list(df_T_var,  "T_var",  "T_var",  n_pc_start, n_pc_end)
  cat("    →", length(eof_list_T_var), "EOFs calculées\n")

  cat(">>> [3/4] EOFs T_grad\n")
  eof_list_T_grad <- .build_eof_list(df_T_grad, "T_grad", "T_grad", n_pc_start, n_pc_end)
  cat("    →", length(eof_list_T_grad), "EOFs calculées\n")

  cat(">>> [4/4] Fusion & pivot\n")
  eof_all_sig      <- .build_eof_all_sig(eof_list_T_mean, eof_list_T_var, eof_list_T_grad)
  data_eof_flatten <- .flatten_eof_all_sig(eof_all_sig)

  cat("\n✔ Pipeline terminé\n")
  cat("  Dimensions data_eof_flatten :", nrow(data_eof_flatten), "x", ncol(data_eof_flatten), "\n")
  cat("  Colonnes température        :", ncol(data_eof_flatten) - 1, "\n")

  data_eof_flatten
}
