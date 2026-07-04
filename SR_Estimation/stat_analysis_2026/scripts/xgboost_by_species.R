# =============================================================================
#  XGBoost par espèce — baseline vs EOF
#  Sortie : AUC par fold + modèles sauvegardés + importance des variables
#
#  NOTE : validation par folds aléatoires stratifiés (createFolds).
#  Les covariables trend_* et T_PC* sont constantes intra-cohorte : ce schéma
#  laisse donc passer la fuite cohorte (choix assumé ici).
# =============================================================================

library(xgboost)
library(Matrix)
library(pROC)
library(caret)
library(dplyr)
library(tidyr)   # replace_na

# ── Profondeurs par espèce ────────────────────────────────────────────────────
depth_ranges <- list(
  "Clupea harengus"          = c(1, 10, 20, 30, 40, 50, 60),
  "Gadus morhua"             = c(1, 10, 20, 30, 40),
  "Melanogrammus aeglefinus" = c(1, 10, 20, 30, 40),
  "Merlangius merlangus"     = c(1, 10, 20, 30, 40),
  "Pleuronectes platessa"    = c(1, 10, 20, 30, 40, 50, 60, 70, 80),
  "Pollachius virens"        = c(1, 10, 20, 30, 40, 50, 60, 70),
  "Scomber scombrus"         = c(1, 10, 20, 30, 40, 50, 60, 70),
  "Sprattus sprattus"        = c(1, 10, 20, 30),
  "Trisopterus esmarkii"     = c(40, 50, 60, 70, 80, 90, 100)
)

# ── Paramètres globaux ────────────────────────────────────────────────────────
pc_pattern    <- "^T_PC(1|2|3)_"   # PC1 à PC3 retenues
k             <- 10
n_rounds      <- 5000
early_stop    <- 30
n_rounds_full <- 500                   # rounds fixes pour le modèle final

dir_eof_data <- "../data/model_data/EOF_miprojet"
dir_models   <- "../data/model_data/xgb_models"
dir_results  <- "../data/model_data/xgb_results"
dir.create(dir_models,  recursive = TRUE, showWarnings = FALSE)
dir.create(dir_results, recursive = TRUE, showWarnings = FALSE)

# ── Paramètres XGBoost communs (scale_pos_weight ajouté par fold) ─────────────
xgb_base_params <- list(
  objective        = "binary:logistic",
  eval_metric      = "auc",
  eta              = 0.05,
  max_depth        = 6,
  subsample        = 0.8,
  colsample_bytree = 0.8
)

# ── Colonnes biologiques communes aux deux modèles ────────────────────────────
bio_cols <- c("Numeric_sex", "Age_sc", "LngtClassGrouped_sc", "Age_x_Lngt_sc",
              "Cohort_num_sc", "Latitude", "Longitude", "Latitude_x_Longitude",
              "Depth", "julian_day", "Area_fact")

# =============================================================================
#  Fonction : validation croisée + modèle final pour un jeu de features donné
# =============================================================================
run_xgb_cv <- function(df_model, label = "model") {

  X <- model.matrix(~ . - Numeric_sex, data = df_model)
  y <- df_model$Numeric_sex

  set.seed(42)
  folds   <- createFolds(factor(y), k = k, list = TRUE, returnTrain = FALSE)
  auc_vec <- numeric(k)

  for (i in seq_len(k)) {
    cat("    ── Fold", i, "/", k, "\n")
    idx_test  <- folds[[i]]
    idx_train <- setdiff(seq_len(nrow(X)), idx_test)

    params_i <- c(xgb_base_params,
                  list(scale_pos_weight =
                         sum(y[idx_train] == 0) / sum(y[idx_train] == 1)))

    dtrain <- xgb.DMatrix(X[idx_train, ], label = y[idx_train])
    dtest  <- xgb.DMatrix(X[idx_test, ],  label = y[idx_test])

    fit_i <- xgb.train(
      params                = params_i,
      data                  = dtrain,
      nrounds               = n_rounds,
      evals                 = list(test = dtest),
      early_stopping_rounds = early_stop,
      verbose               = 0
    )

    preds      <- predict(fit_i, dtest)
    auc_vec[i] <- as.numeric(auc(roc(y[idx_test], preds, quiet = TRUE)))
  }

  cat("    ", label, "AUC :", round(mean(auc_vec), 4),
      "± sd :", round(sd(auc_vec), 4), "\n")

  # ── Modèle final sur l'ensemble des données ────────────────────────────────
  params_full <- c(xgb_base_params,
                   list(scale_pos_weight = sum(y == 0) / sum(y == 1)))
  fit_full <- xgb.train(
    params  = params_full,
    data    = xgb.DMatrix(X, label = y),
    nrounds = n_rounds_full,
    verbose = 0
  )

  list(
    auc_folds  = auc_vec,
    auc_mean   = mean(auc_vec),
    auc_sd     = sd(auc_vec),
    importance = xgb.importance(model = fit_full),
    model      = fit_full
  )
}

# =============================================================================
#  Boucle principale par espèce
# =============================================================================
results_all <- list()

for (sp in names(depth_ranges)) {

  cat("\n", strrep("=", 60), "\n", sep = "")
  cat("Espèce :", sp, "\n")
  cat(strrep("=", 60), "\n", sep = "")

  sp_slug       <- gsub(" ", "_", sp)
  depths_keep   <- depth_ranges[[sp]]
  depth_pattern <- paste0("_d", depths_keep, "$", collapse = "|")

  rds_path <- file.path(dir_eof_data, paste0(sp_slug, ".rds"))
  if (!file.exists(rds_path)) {
    warning("Fichier introuvable : ", rds_path, " — espèce ignorée.")
    next
  }

  ML_data_mono <- readRDS(rds_path)

  # ── Sélection des colonnes EOF et trend filtrées par profondeur ────────────
  cols_T_filt <- grep("^T_PC", names(ML_data_mono), value = TRUE)
  cols_T_filt <- cols_T_filt[grepl(pc_pattern, cols_T_filt) &
                               grepl(depth_pattern, cols_T_filt)]

  cols_trend_filt <- grep("^trend_", names(ML_data_mono), value = TRUE)
  cols_trend_filt <- cols_trend_filt[grepl(depth_pattern, cols_trend_filt)]

  ML_data_mono <- ML_data_mono %>%
    mutate(across(all_of(cols_T_filt),     ~ replace_na(., 0))) %>%
    mutate(across(all_of(cols_trend_filt), ~ replace_na(., 0)))

  cat("  EOF cols :", length(cols_T_filt),
      "| trend cols :", length(cols_trend_filt),
      "| individus :", nrow(ML_data_mono), "\n")

  if (length(cols_T_filt) == 0)
    warning("Aucune colonne EOF pour ", sp,
            " (profondeurs disponibles incompatibles avec sa range).")

  # ==========================================================================
  #  1. BASELINE — variables biologiques seules
  # ==========================================================================
  cat("  → Baseline\n")
  df_base <- ML_data_mono %>%
    dplyr::select(all_of(bio_cols)) %>%
    mutate(Area_fact = factor(Area_fact)) %>%
    na.omit()
  res_base <- run_xgb_cv(df_base, label = "Baseline")

  # ==========================================================================
  #  2. EOF — biologiques + trend_* + T_PC*
  # ==========================================================================
  cat("  → EOF\n")
  df_eof <- ML_data_mono %>%
    dplyr::select(all_of(bio_cols), all_of(cols_trend_filt), all_of(cols_T_filt)) %>%
    mutate(Area_fact = factor(Area_fact)) %>%
    na.omit()
  res_eof <- run_xgb_cv(df_eof, label = "EOF")

  # ==========================================================================
  #  SAUVEGARDE
  # ==========================================================================
  saveRDS(res_base$model, file.path(dir_models, paste0(sp_slug, "_baseline.rds")))
  saveRDS(res_eof$model,  file.path(dir_models, paste0(sp_slug, "_eof.rds")))

  results_all[[sp]] <- list(
    baseline = res_base[c("auc_folds", "auc_mean", "auc_sd", "importance")],
    eof      = res_eof[c("auc_folds", "auc_mean", "auc_sd", "importance")]
  )

  saveRDS(results_all[[sp]],
          file.path(dir_results, paste0(sp_slug, "_results.rds")))

  rm(ML_data_mono, df_base, df_eof, res_base, res_eof)
  gc()

  cat("  ✔ Sauvegardé.\n")
}

# =============================================================================
#  Tableau récapitulatif AUC
# =============================================================================
auc_table <- bind_rows(lapply(names(results_all), function(sp) {
  tibble(
    Species      = sp,
    AUC_baseline = round(results_all[[sp]]$baseline$auc_mean, 4),
    SD_baseline  = round(results_all[[sp]]$baseline$auc_sd,   4),
    AUC_eof      = round(results_all[[sp]]$eof$auc_mean,      4),
    SD_eof       = round(results_all[[sp]]$eof$auc_sd,        4),
    Delta_AUC    = round(results_all[[sp]]$eof$auc_mean -
                           results_all[[sp]]$baseline$auc_mean, 4)
  )
}))

print(auc_table)
saveRDS(auc_table, file.path(dir_results, "auc_table_all_species.rds"))
write.csv(auc_table, file.path(dir_results, "auc_table_all_species.csv"),
          row.names = FALSE)
cat("\n✔ Tableau AUC sauvegardé.\n")