# ================================================================
# GAM complet a penalisation libre + selection backward par p-value
# Wald sur les covariables thermiques, une seule espece, process isole
# Sauvegarde incrementale a chaque etape (reprise possible en cas
# d'interruption). Couvre les 8 especes standard (hors Pollachius
# virens, traitee separement du fait de sa formule reduite).
# Conserve aussi le modele complet initial (4 covariables, avant
# toute selection) pour comparer l'EDF du tenseur avec/sans thermique.
# Usage : Rscript run_one_gam_species.R <espece>
# ================================================================

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1) stop("Usage: Rscript run_one_gam_species.R <espece>")
sp <- args[1]

if (sp == "Pollachius virens") {
  stop("Pollachius virens doit etre traitee avec un script dedie (formule reduite, sans f4/bc).")
}

library(dplyr)
library(mgcv)
library(lubridate)
library(rprojroot)

proj_root <- find_root(has_file(".git") | is_rstudio_project)
setwd(file.path(proj_root, "SR_Estimation", "stat_analysis_2026", "data"))

load("temp_v4.RData")
source("../scripts/test_temp_v4.R")

MONTH_OFFSETS <- 0:11
method <- "detrend"
seuil_p <- 0.05

out_dir <- "results_gam_validation"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
out_file <- file.path(out_dir, paste0("gam_selection_", sp, ".rds"))
progress_file <- file.path(out_dir, paste0("gam_selection_", sp, "_progress.rds"))

# ---- Si l'espece a deja tourne (resultat final present), on saute ----
if (file.exists(out_file)) {
  cat("Deja en cache pour", sp, ", rien a faire.\n")
  quit(save = "no", status = 0)
}

# ---- Hyperparametres, identiques a test_temp_v4.R, pour les 8 especes ----
K_TENSOR_BY_SPECIES <- list(
  "Trisopterus esmarkii"      = c(7, 8, 8),
  "Pleuronectes platessa"     = c(6, 6, 8),
  "Clupea harengus"           = c(6, 6, 8),
  "Merlangius merlangus"      = c(6, 6, 8),
  "Melanogrammus aeglefinus"  = c(6, 6, 8)
)
k_tensor_default <- c(8, 8, 8)   # Gadus morhua, Sprattus sprattus, Scomber scombrus

K_SPACE_BY_SPECIES <- list(
  "Pleuronectes platessa" = 240,
  "Trisopterus esmarkii"  = 240,
  "Sprattus sprattus"     = 240
)
k_space_default <- 120

k_time <- 12

k_tensor <- if (!is.null(K_TENSOR_BY_SPECIES[[sp]])) K_TENSOR_BY_SPECIES[[sp]] else k_tensor_default
k_age <- k_tensor[1]; k_lngt <- k_tensor[2]; k_fs <- k_tensor[3]
k_space <- if (!is.null(K_SPACE_BY_SPECIES[[sp]])) K_SPACE_BY_SPECIES[[sp]] else k_space_default

cat("Espece :", sp, "| k_age =", k_age, "| k_lngt =", k_lngt, "| k_fs =", k_fs,
    "| k_space =", k_space, "\n")

# ---- Donnees individuelles de l'espece ----
data_sp <- data_expanded_1991_2023 %>%
  dplyr::filter(Species == sp, !is.na(Numeric_sex)) %>%
  droplevels()

cat("N =", nrow(data_sp), "\n")

# ---- Serie thermique et covariables agregees par cohorte (poids uniforme) ----
temp_surface_mensuelle <- prepare_temp_series(df_long)

cohort_levels <- levels(data_sp$Cohort_fact)
cohort_years  <- as.numeric(as.character(cohort_levels))

build_temp_extended <- function(monthly_series, cohort_years, month_offsets) {
  n_cohort <- length(cohort_years)
  temp_extended <- matrix(NA_real_, nrow = n_cohort, ncol = length(month_offsets))
  for (i in seq_along(cohort_years)) {
    yr <- cohort_years[i]
    target_dates <- as.Date(paste0(yr, "-01-01")) %m+% months(month_offsets)
    idx_match <- match(target_dates, monthly_series$date)
    temp_extended[i, ] <- monthly_series$temp_moy_bassin[idx_match]
  }
  temp_extended
}

temp_extended_raw <- build_temp_extended(
  temp_surface_mensuelle %>% dplyr::select(date, temp_moy_bassin),
  cohort_years, MONTH_OFFSETS
)
temp_extended_anomaly <- build_temp_extended(
  temp_surface_mensuelle %>% dplyr::select(date, temp_anomaly_harm) %>%
    dplyr::rename(temp_moy_bassin = temp_anomaly_harm),
  cohort_years, MONTH_OFFSETS
)
temp_extended_trend <- build_temp_extended(
  temp_surface_mensuelle %>% dplyr::select(date, trend_component) %>%
    dplyr::rename(temp_moy_bassin = trend_component),
  cohort_years, MONTH_OFFSETS
)

stopifnot(!anyNA(temp_extended_raw), !anyNA(temp_extended_anomaly), !anyNA(temp_extended_trend))

w_month <- rep(1 / length(MONTH_OFFSETS), length(MONTH_OFFSETS))
wmean <- function(mat) as.vector(mat %*% w_month)
wsd   <- function(mat, m) sqrt(as.vector(((mat - m)^2) %*% w_month))

Trend_mean_w   <- wmean(temp_extended_trend)
T_sd_abs_w     <- wsd(temp_extended_raw, wmean(temp_extended_raw))
Anomaly_mean_w <- wmean(temp_extended_anomaly)
Anomaly_sd_w   <- wsd(temp_extended_anomaly, Anomaly_mean_w)

cohort_covariates <- data.frame(
  Cohort_fact    = cohort_levels,
  Trend_mean_w   = Trend_mean_w,
  T_sd_abs_w     = T_sd_abs_w,
  Anomaly_mean_w = Anomaly_mean_w,
  Anomaly_sd_w   = Anomaly_sd_w
)

data_sp_thermal <- data_sp %>%
  dplyr::left_join(cohort_covariates, by = "Cohort_fact") %>%
  dplyr::mutate(
    Cohort_fact = factor(Cohort_fact, levels = levels(data_sp$Cohort_fact)),
    Age_sc = as.numeric(Age_sc),
    LngtClassGrouped_sc = as.numeric(LngtClassGrouped_sc),
    Cohort_num_sc = as.numeric(Cohort_num_sc)
  )

stopifnot(!anyNA(data_sp_thermal$Trend_mean_w))

termes_thermiques <- c("Trend_mean_w", "T_sd_abs_w", "Anomaly_mean_w", "Anomaly_sd_w")

# ---- Construction de la formule (formule complete standard) ----
build_formula_sp <- function(k_age, k_lngt, k_space, k_time, k_cohort) {
  as.formula(bquote(
    Numeric_sex ~ te(Age_sc, LngtClassGrouped_sc, Cohort_num_sc,
                      bs = c("cr", "cr", "cs"),
                      k = c(.(k_age), .(k_lngt), .(k_cohort))) +
      s(Latitude, Longitude, k = .(k_space), bs = "sos") +
      s(julian_day, bs = "cc", k = .(k_time)) +
      s(Cohort_fact, bs = "re")
  ))
}

formula_base <- build_formula_sp(k_age, k_lngt, k_space, k_time, k_fs)

build_formula_gam <- function(termes_actifs) {
  if (length(termes_actifs) == 0) return(formula_base)
  ajout <- as.formula(paste(". ~ . +", paste(termes_actifs, collapse = " + ")))
  update(formula_base, ajout)
}

fit_gam <- function(termes_actifs) {
  formule <- build_formula_gam(termes_actifs)
  bam(formule, family = binomial(link = "logit"), data = data_sp_thermal,
      method = "ML", discrete = TRUE, keepData = TRUE)
}

sauver_progression <- function(termes_actifs, modele_courant, historique, etape,
                                modele_complet_initial) {
  saveRDS(list(
    termes_actifs = termes_actifs, modele_courant = modele_courant,
    historique = historique, etape = etape,
    modele_complet_initial = modele_complet_initial
  ), progress_file)
}

# ================================================================
# Selection backward par p-value Wald, avec reprise sur progression
# ================================================================

if (file.exists(progress_file)) {

  cat("Reprise depuis la progression sauvegardee...\n")
  prog <- readRDS(progress_file)
  termes_actifs          <- prog$termes_actifs
  modele_courant         <- prog$modele_courant
  historique             <- prog$historique
  etape                  <- prog$etape
  modele_complet_initial <- prog$modele_complet_initial
  cat("Reprise a l'etape", etape, ", termes actifs :", paste(termes_actifs, collapse = ", "), "\n")

} else {

  cat("\n=== Fit du modele GAM complet (4 covariables thermiques) ===\n")
  termes_actifs <- termes_thermiques
  modele_courant <- fit_gam(termes_actifs)
  modele_complet_initial <- modele_courant
  cat("AIC modele complet :", round(AIC(modele_courant), 2), "\n")
  cat("EDF tenseur (modele complet, 4 cov. thermiques) :",
      round(summary(modele_courant)$edf[1], 3), "\n")

  historique <- list()
  etape <- 1
  sauver_progression(termes_actifs, modele_courant, historique, etape, modele_complet_initial)
}

repeat {

  if (length(termes_actifs) == 0) {
    cat("\nPlus aucun terme actif, arret.\n")
    break
  }

  cat("\n--- Etape", etape, ": termes actifs =", paste(termes_actifs, collapse = ", "), "---\n")

  summ <- summary(modele_courant)
  p_table <- summ$p.table

  p_values <- setNames(numeric(length(termes_actifs)), termes_actifs)
  for (terme in termes_actifs) {
    p_values[terme] <- p_table[terme, "Pr(>|z|)"]
  }

  cat("  p-values :\n")
  print(round(p_values, 4))

  terme_a_retirer <- names(which.max(p_values))
  p_max <- max(p_values)

  historique[[etape]] <- data.frame(
    espece = sp, etape = etape, terme_teste = terme_a_retirer, p_wald = p_max
  )

  if (p_max > seuil_p) {
    cat(">>> Retrait de", terme_a_retirer, "(p =", round(p_max, 4), ">", seuil_p, ")\n")
    termes_actifs  <- setdiff(termes_actifs, terme_a_retirer)
    modele_courant <- fit_gam(termes_actifs)
    etape <- etape + 1
    sauver_progression(termes_actifs, modele_courant, historique, etape, modele_complet_initial)
  } else {
    cat(">>> Tous les termes restants sont significatifs (p min =", round(p_max, 4), "), arret.\n")
    break
  }
}

cat("\n=== RESULTAT FINAL", sp, "===\n")
cat("Termes retenus :", paste(termes_actifs, collapse = ", "), "\n")
cat("Termes retires  :", paste(setdiff(termes_thermiques, termes_actifs), collapse = ", "), "\n")

cat("\n=== Fit du modele de base (sans covariable thermique) ===\n")
modele_base <- fit_gam(character(0))
cat("AIC modele de base :", round(AIC(modele_base), 2), "\n")
cat("AIC modele final avec thermique :", round(AIC(modele_courant), 2), "\n")
cat("Delta AIC (base - final) :", round(AIC(modele_base) - AIC(modele_courant), 2), "\n")

cat("\n========== COMPARAISON EDF DU TENSEUR ==========\n")
edf_base    <- summary(modele_base)$edf[1]
edf_initial <- summary(modele_complet_initial)$edf[1]
cat("EDF tenseur, modele sans thermique       :", round(edf_base, 3), "\n")
cat("EDF tenseur, modele complet (4 cov.)     :", round(edf_initial, 3), "\n")
cat("Delta EDF (avec - sans)                  :", round(edf_initial - edf_base, 3), "\n")

cat("\n========== SUMMARY MODELE FINAL ==========\n")
print(summary(modele_courant))

cat("\n========== CONCURVITE (modele final) ==========\n")
conc <- tryCatch(concurvity(modele_courant, full = TRUE), error = function(e) NULL)
print(conc)

resultat <- list(
  species = sp,
  k_age = k_age, k_lngt = k_lngt, k_fs = k_fs, k_space = k_space,
  termes_retenus = termes_actifs,
  termes_retires = setdiff(termes_thermiques, termes_actifs),
  modele_initial = modele_complet_initial,
  modele_final = modele_courant,
  modele_base = modele_base,
  AIC_final = AIC(modele_courant),
  AIC_base = AIC(modele_base),
  edf_tenseur_base = edf_base,
  edf_tenseur_initial = edf_initial,
  delta_edf_tenseur = edf_initial - edf_base,
  summary_final = summary(modele_courant),
  historique = if (length(historique) > 0) do.call(rbind, historique) else NULL,
  concurvity = conc,
  cohort_covariates = cohort_covariates
)

saveRDS(resultat, out_file)
cat("\nSauvegarde :", out_file, "\n")

if (file.exists(progress_file)) file.remove(progress_file)

quit(save = "no", status = 0)
