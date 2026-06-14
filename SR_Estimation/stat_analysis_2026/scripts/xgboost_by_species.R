# =============================================================================
#  XGBoost par espèce — baseline vs EOF
#  Sortie : AUC par fold + modèles sauvegardés + importance des variables
# =============================================================================

library(xgboost)
library(Matrix)
library(pROC)
library(caret)
library(dplyr)

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

pc_pattern <- "^T_PC(1|2|3)_"
k          <- 10
n_rounds   <- 5000
early_stop <- 30

dir_models  <- "model_data/xgb_models"
dir_results <- "model_data/xgb_results"
dir.create(dir_models,  recursive = TRUE, showWarnings = FALSE)
dir.create(dir_results, recursive = TRUE, showWarnings = FALSE)

results_all <- list()

for (sp in names(depth_ranges)) {

  cat("\n", strrep("=", 60), "\n")
  cat("Espèce :", sp, "\n")
  cat(strrep("=", 60), "\n")

  sp_slug       <- gsub(" ", "_", sp)
  depths_keep   <- depth_ranges[[sp]]
  depth_pattern <- paste0("_d", depths_keep, "$", collapse = "|")

  rds_path <- file.path(
    "model_data/EOF_species_joined_data_without_linear_trend_5_prct",
    paste0(sp_slug, ".rds")
  )
  if (!file.exists(rds_path)) {
    warning("Fichier introuvable : ", rds_path)
    next
  }

  ML_data_mono <- readRDS(rds_path)

  cols_T_filt <- grep("^T_PC", names(ML_data_mono), value = TRUE)
  cols_T_filt <- cols_T_filt[grepl(pc_pattern, cols_T_filt) &
                               grepl(depth_pattern, cols_T_filt)]

  cols_trend_filt <- grep("^trend_", names(ML_data_mono), value = TRUE)
  cols_trend_filt <- cols_trend_filt[grepl(depth_pattern, cols_trend_filt)]

  cols_bio <- names(ML_data_mono)[!grepl("^T_PC|^trend_|^obs_", names(ML_data_mono))]

  ML_data_mono <- ML_data_mono %>%
    select(all_of(c(cols_bio, cols_T_filt, cols_trend_filt))) %>%
    mutate(across(starts_with("T_PC"),   ~ replace_na(., 0))) %>%
    mutate(across(starts_with("trend_"), ~ replace_na(., 0)))

  cat("  EOF cols :", length(cols_T_filt),
      "| trend cols :", length(cols_trend_filt),
      "| individus :", nrow(ML_data_mono), "\n")

  # ── params XGBoost communs (scale_pos_weight ajusté par fold) ──────────────
  xgb_base_params <- list(
    objective        = "binary:logistic",
    eval_metric      = "auc",
    eta              = 0.05,
    max_depth        = 6,
    subsample        = 0.8,
    colsample_bytree = 0.8
  )

  # ==========================================================================
  #  BASELINE
  # ==========================================================================
  cat("  → Baseline\n")

  df_indiv <- ML_data_mono %>%
    dplyr::select(Numeric_sex, Age_sc, LngtClassGrouped_sc, Age_x_Lngt_sc,
                  Cohort_num_sc, Latitude, Longitude, Latitude_x_Longitude,
                  Depth, julian_day, Area_fact) %>%
    mutate(Area_fact = factor(Area_fact)) %>%
    na.omit()

  X_base <- model.matrix(~ . - Numeric_sex, data = df_indiv)
  y_base <- df_indiv$Numeric_sex

  set.seed(42)
  folds_base   <- createFolds(y_base, k = k, list = TRUE, returnTrain = FALSE)
  auc_baseline <- numeric(k)

  for (i in seq_len(k)) {
    cat("── Fold", i, "/", k, "\n")
    idx_test  <- folds_base[[i]]
    idx_train <- setdiff(seq_len(nrow(X_base)), idx_test)

    params_i <- c(xgb_base_params,
                  list(scale_pos_weight =
                         sum(y_base[idx_train] == 0) / sum(y_base[idx_train] == 1)))

    fit_i <- xgb.train(
      params                = params_i,
      data                  = xgb.DMatrix(X_base[idx_train, ], label = y_base[idx_train]),
      nrounds               = n_rounds,
      evals                 = list(test = xgb.DMatrix(X_base[idx_test, ], label = y_base[idx_test])),
      early_stopping_rounds = early_stop,
      verbose               = 0
    )

    preds            <- predict(fit_i, xgb.DMatrix(X_base[idx_test, ]))
    auc_baseline[i]  <- as.numeric(auc(roc(y_base[idx_test], preds, quiet = TRUE)))
  }

  cat("── Baseline AUC :", round(mean(auc_baseline), 4),
      "± sd :", round(sd(auc_baseline), 4), "\n")

  # Modèle final baseline — 500 rounds fixes
  params_full <- c(xgb_base_params,
                   list(scale_pos_weight = sum(y_base == 0) / sum(y_base == 1)))
  fit_baseline_full <- xgb.train(
    params  = params_full,
    data    = xgb.DMatrix(X_base, label = y_base),
    nrounds = 500,
    verbose = 0
  )
  imp_baseline <- xgb.importance(model = fit_baseline_full)

  # ==========================================================================
  #  EOF
  # ==========================================================================
  cat("  → EOF\n")

  df_trend <- ML_data_mono %>%
    dplyr::select(Numeric_sex, Age_sc, LngtClassGrouped_sc, Age_x_Lngt_sc,
                  Cohort_num_sc, Latitude, Longitude, Latitude_x_Longitude,
                  Depth, julian_day, Area_fact,
                  starts_with("trend_"), starts_with("T_PC")) %>%
    mutate(Area_fact = factor(Area_fact)) %>%
    na.omit()

  X_eof <- model.matrix(~ . - Numeric_sex, data = df_trend)
  y_eof <- df_trend$Numeric_sex

  set.seed(42)
  folds_eof <- createFolds(factor(y_eof), k = k, list = TRUE, returnTrain = FALSE)
  auc_eof   <- numeric(k)

  for (i in seq_len(k)) {
    cat("── Fold", i, "/", k, "\n")
    idx_test  <- folds_eof[[i]]
    idx_train <- setdiff(seq_len(nrow(X_eof)), idx_test)

    params_i <- c(xgb_base_params,
                  list(scale_pos_weight =
                         sum(y_eof[idx_train] == 0) / sum(y_eof[idx_train] == 1)))

    fit_i <- xgb.train(
      params                = params_i,
      data                  = xgb.DMatrix(X_eof[idx_train, ], label = y_eof[idx_train]),
      nrounds               = n_rounds,
      evals                 = list(test = xgb.DMatrix(X_eof[idx_test, ], label = y_eof[idx_test])),
      early_stopping_rounds = early_stop,
      verbose               = 0
    )

    preds        <- predict(fit_i, xgb.DMatrix(X_eof[idx_test, ]))
    auc_eof[i]   <- as.numeric(auc(roc(y_eof[idx_test], preds, quiet = TRUE)))
  }

  cat("── EOF AUC :", round(mean(auc_eof), 4),
      "± sd :", round(sd(auc_eof), 4), "\n")

  # Modèle final EOF — 500 rounds fixes
  params_full <- c(xgb_base_params,
                   list(scale_pos_weight = sum(y_eof == 0) / sum(y_eof == 1)))
  fit_eof_full <- xgb.train(
    params  = params_full,
    data    = xgb.DMatrix(X_eof, label = y_eof),
    nrounds = 500,
    verbose = 0
  )
  imp_eof <- xgb.importance(model = fit_eof_full)

  # ==========================================================================
  #  SAUVEGARDE
  # ==========================================================================
  saveRDS(fit_baseline_full, file.path(dir_models, paste0(sp_slug, "_baseline.rds")))
  saveRDS(fit_eof_full,      file.path(dir_models, paste0(sp_slug, "_eof.rds")))

  results_all[[sp]] <- list(
    baseline = list(auc_folds  = auc_baseline,
                    auc_mean   = mean(auc_baseline),
                    auc_sd     = sd(auc_baseline),
                    importance = imp_baseline),
    eof      = list(auc_folds  = auc_eof,
                    auc_mean   = mean(auc_eof),
                    auc_sd     = sd(auc_eof),
                    importance = imp_eof)
  )

  saveRDS(results_all[[sp]],
          file.path(dir_results, paste0(sp_slug, "_results.rds")))

  # Libération mémoire
  rm(ML_data_mono, df_indiv, df_trend,
     X_base, y_base, X_eof, y_eof,
     fit_baseline_full, fit_eof_full,
     imp_baseline, imp_eof,
     folds_base, folds_eof,
     auc_baseline, auc_eof)
  gc()

  cat("  ✔ Sauvegardé.\n")
}

# ── Tableau AUC ───────────────────────────────────────────────────────────────
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
pc_pattern <- "^T_PC(1|2|3)_"
k          <- 10
n_rounds   <- 5000
early_stop <- 30

dir_models  <- "model_data/xgb_models"
dir_results <- "model_data/xgb_results"
dir.create(dir_models,  recursive = TRUE, showWarnings = FALSE)
dir.create(dir_results, recursive = TRUE, showWarnings = FALSE)

results_all <- list()

for (sp in names(depth_ranges)) {

  cat("\n", strrep("=", 60), "\n")
  cat("Espèce :", sp, "\n")
  cat(strrep("=", 60), "\n")

  sp_slug       <- gsub(" ", "_", sp)
  depths_keep   <- depth_ranges[[sp]]
  depth_pattern <- paste0("_d", depths_keep, "$", collapse = "|")

  rds_path <- file.path(
    "model_data/EOF_species_joined_data_without_linear_trend_5_prct",
    paste0(sp_slug, ".rds")
  )
  if (!file.exists(rds_path)) {
    warning("Fichier introuvable : ", rds_path)
    next
  }

  ML_data_mono <- readRDS(rds_path)

  cols_T_filt <- grep("^T_PC", names(ML_data_mono), value = TRUE)
  cols_T_filt <- cols_T_filt[grepl(pc_pattern, cols_T_filt) &
                               grepl(depth_pattern, cols_T_filt)]

  cols_trend_filt <- grep("^trend_", names(ML_data_mono), value = TRUE)
  cols_trend_filt <- cols_trend_filt[grepl(depth_pattern, cols_trend_filt)]

  cols_bio <- names(ML_data_mono)[!grepl("^T_PC|^trend_|^obs_", names(ML_data_mono))]

  ML_data_mono <- ML_data_mono %>%
    select(all_of(c(cols_bio, cols_T_filt, cols_trend_filt))) %>%
    mutate(across(starts_with("T_PC"),   ~ replace_na(., 0))) %>%
    mutate(across(starts_with("trend_"), ~ replace_na(., 0)))

  cat("  EOF cols :", length(cols_T_filt),
      "| trend cols :", length(cols_trend_filt),
      "| individus :", nrow(ML_data_mono), "\n")

  # ── params XGBoost communs (scale_pos_weight ajusté par fold) ──────────────
  xgb_base_params <- list(
    objective        = "binary:logistic",
    eval_metric      = "auc",
    eta              = 0.05,
    max_depth        = 6,
    subsample        = 0.8,
    colsample_bytree = 0.8
  )

  # ==========================================================================
  #  BASELINE
  # ==========================================================================
  cat("  → Baseline\n")

  df_indiv <- ML_data_mono %>%
    dplyr::select(Numeric_sex, Age_sc, LngtClassGrouped_sc, Age_x_Lngt_sc,
                  Cohort_num_sc, Latitude, Longitude, Latitude_x_Longitude,
                  Depth, julian_day, Area_fact) %>%
    mutate(Area_fact = factor(Area_fact)) %>%
    na.omit()

  X_base <- model.matrix(~ . - Numeric_sex, data = df_indiv)
  y_base <- df_indiv$Numeric_sex

  set.seed(42)
  folds_base   <- createFolds(y_base, k = k, list = TRUE, returnTrain = FALSE)
  auc_baseline <- numeric(k)

  for (i in seq_len(k)) {
    cat("── Fold", i, "/", k, "\n")
    idx_test  <- folds_base[[i]]
    idx_train <- setdiff(seq_len(nrow(X_base)), idx_test)

    params_i <- c(xgb_base_params,
                  list(scale_pos_weight =
                         sum(y_base[idx_train] == 0) / sum(y_base[idx_train] == 1)))

    fit_i <- xgb.train(
      params                = params_i,
      data                  = xgb.DMatrix(X_base[idx_train, ], label = y_base[idx_train]),
      nrounds               = n_rounds,
      evals                 = list(test = xgb.DMatrix(X_base[idx_test, ], label = y_base[idx_test])),
      early_stopping_rounds = early_stop,
      verbose               = 0
    )

    preds            <- predict(fit_i, xgb.DMatrix(X_base[idx_test, ]))
    auc_baseline[i]  <- as.numeric(auc(roc(y_base[idx_test], preds, quiet = TRUE)))
  }

  cat("── Baseline AUC :", round(mean(auc_baseline), 4),
      "± sd :", round(sd(auc_baseline), 4), "\n")

  # Modèle final baseline — 500 rounds fixes
  params_full <- c(xgb_base_params,
                   list(scale_pos_weight = sum(y_base == 0) / sum(y_base == 1)))
  fit_baseline_full <- xgb.train(
    params  = params_full,
    data    = xgb.DMatrix(X_base, label = y_base),
    nrounds = 500,
    verbose = 0
  )
  imp_baseline <- xgb.importance(model = fit_baseline_full)

  # ==========================================================================
  #  EOF
  # ==========================================================================
  cat("  → EOF\n")

  df_trend <- ML_data_mono %>%
    dplyr::select(Numeric_sex, Age_sc, LngtClassGrouped_sc, Age_x_Lngt_sc,
                  Cohort_num_sc, Latitude, Longitude, Latitude_x_Longitude,
                  Depth, julian_day, Area_fact,
                  starts_with("trend_"), starts_with("T_PC")) %>%
    mutate(Area_fact = factor(Area_fact)) %>%
    na.omit()

  X_eof <- model.matrix(~ . - Numeric_sex, data = df_trend)
  y_eof <- df_trend$Numeric_sex

  set.seed(42)
  folds_eof <- createFolds(factor(y_eof), k = k, list = TRUE, returnTrain = FALSE)
  auc_eof   <- numeric(k)

  for (i in seq_len(k)) {
    cat("── Fold", i, "/", k, "\n")
    idx_test  <- folds_eof[[i]]
    idx_train <- setdiff(seq_len(nrow(X_eof)), idx_test)

    params_i <- c(xgb_base_params,
                  list(scale_pos_weight =
                         sum(y_eof[idx_train] == 0) / sum(y_eof[idx_train] == 1)))

    fit_i <- xgb.train(
      params                = params_i,
      data                  = xgb.DMatrix(X_eof[idx_train, ], label = y_eof[idx_train]),
      nrounds               = n_rounds,
      evals                 = list(test = xgb.DMatrix(X_eof[idx_test, ], label = y_eof[idx_test])),
      early_stopping_rounds = early_stop,
      verbose               = 0
    )

    preds        <- predict(fit_i, xgb.DMatrix(X_eof[idx_test, ]))
    auc_eof[i]   <- as.numeric(auc(roc(y_eof[idx_test], preds, quiet = TRUE)))
  }

  cat("── EOF AUC :", round(mean(auc_eof), 4),
      "± sd :", round(sd(auc_eof), 4), "\n")

  # Modèle final EOF — 500 rounds fixes
  params_full <- c(xgb_base_params,
                   list(scale_pos_weight = sum(y_eof == 0) / sum(y_eof == 1)))
  fit_eof_full <- xgb.train(
    params  = params_full,
    data    = xgb.DMatrix(X_eof, label = y_eof),
    nrounds = 500,
    verbose = 0
  )
  imp_eof <- xgb.importance(model = fit_eof_full)

  # ==========================================================================
  #  SAUVEGARDE
  # ==========================================================================
  saveRDS(fit_baseline_full, file.path(dir_models, paste0(sp_slug, "_baseline.rds")))
  saveRDS(fit_eof_full,      file.path(dir_models, paste0(sp_slug, "_eof.rds")))

  results_all[[sp]] <- list(
    baseline = list(auc_folds  = auc_baseline,
                    auc_mean   = mean(auc_baseline),
                    auc_sd     = sd(auc_baseline),
                    importance = imp_baseline),
    eof      = list(auc_folds  = auc_eof,
                    auc_mean   = mean(auc_eof),
                    auc_sd     = sd(auc_eof),
                    importance = imp_eof)
  )

  saveRDS(results_all[[sp]],
          file.path(dir_results, paste0(sp_slug, "_results.rds")))
  cat("  ✔ Sauvegardé.\n")
}

# ── Tableau AUC ───────────────────────────────────────────────────────────────
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
pc_pattern <- "^T_PC(1|2|3)_"
k          <- 10
n_rounds   <- 5000
early_stop <- 30

dir_models  <- "model_data/xgb_models"
dir_results <- "model_data/xgb_results"
dir.create(dir_models,  recursive = TRUE, showWarnings = FALSE)
dir.create(dir_results, recursive = TRUE, showWarnings = FALSE)

results_all <- list()

# =============================================================================
#  Boucle principale
# =============================================================================

for (sp in names(depth_ranges)) {

  cat("\n", strrep("=", 60), "\n")
  cat("Espèce :", sp, "\n")
  cat(strrep("=", 60), "\n")

  sp_slug       <- gsub(" ", "_", sp)
  depths_keep   <- depth_ranges[[sp]]
  depth_pattern <- paste0("_d", depths_keep, "$", collapse = "|")

  rds_path <- file.path(
    "model_data/EOF_species_joined_data_without_linear_trend_5_prct",
    paste0(sp_slug, ".rds")
  )
  if (!file.exists(rds_path)) {
    warning("Fichier introuvable : ", rds_path, " — espèce ignorée.")
    next
  }

  ML_data_mono <- readRDS(rds_path)

  # ── Sélection des colonnes ─────────────────────────────────────────────────
  cols_bio <- names(ML_data_mono)[!grepl("^T_PC|^trend_|^obs_", names(ML_data_mono))]

  cols_T_filt <- grep("^T_PC", names(ML_data_mono), value = TRUE)
  cols_T_filt <- cols_T_filt[grepl(pc_pattern, cols_T_filt) &
                               grepl(depth_pattern, cols_T_filt)]

  cols_trend_filt <- grep("^trend_", names(ML_data_mono), value = TRUE)
  cols_trend_filt <- cols_trend_filt[grepl(depth_pattern, cols_trend_filt)]

  ML_data_mono <- ML_data_mono %>%
    select(all_of(c(cols_bio, cols_T_filt, cols_trend_filt))) %>%
    mutate(across(starts_with("T_PC"),   ~ replace_na(., 0))) %>%
    mutate(across(starts_with("trend_"), ~ replace_na(., 0)))

  cat("  EOF cols :", length(cols_T_filt),
      "| trend cols :", length(cols_trend_filt),
      "| individus :", nrow(ML_data_mono), "\n")

  # ==========================================================================
  #  1. BASELINE
  # ==========================================================================
  cat("  → Baseline\n")

  df_indiv <- ML_data_mono %>%
    dplyr::select(Numeric_sex, Age_sc, LngtClassGrouped_sc, Age_x_Lngt_sc,
                  Cohort_num_sc, Latitude, Longitude, Latitude_x_Longitude,
                  Depth, julian_day, Area_fact) %>%
    mutate(Area_fact = factor(Area_fact)) %>%
    na.omit()

  X <- model.matrix(~ . - Numeric_sex, data = df_indiv)
  y <- df_indiv$Numeric_sex

  set.seed(42)
  folds     <- createFolds(y, k = k, list = TRUE, returnTrain = FALSE)
  n_folds   <- length(folds)
  auc_baseline <- numeric(n_folds)

  for (i in seq_len(n_folds)) {
    cat("── Fold", i, "/", n_folds, "\n")
    idx_test  <- folds[[i]]
    idx_train <- setdiff(seq_len(nrow(X)), idx_test)

    params <- list(
      objective        = "binary:logistic",
      eval_metric      = "auc",
      eta              = 0.05,
      max_depth        = 6,
      subsample        = 0.8,
      colsample_bytree = 0.8,
      scale_pos_weight = sum(y[idx_train] == 0) / sum(y[idx_train] == 1)
    )

    dtrain <- xgb.DMatrix(data = X[idx_train, ], label = y[idx_train])
    dtest  <- xgb.DMatrix(data = X[idx_test, ],  label = y[idx_test])

    fit <- xgb.train(
      params                = params,
      data                  = dtrain,
      nrounds               = n_rounds,
      evals                 = list(train = dtrain, test = dtest),
      early_stopping_rounds = early_stop,
      verbose               = 0
    )

    preds            <- predict(fit, dtest)
    auc_baseline[i]  <- as.numeric(auc(roc(y[idx_test], preds, quiet = TRUE)))
  }

  cat("\n── Baseline AUC :", round(mean(auc_baseline), 4),
      "± sd :", round(sd(auc_baseline), 4), "\n")

  # Modèle final baseline
  fit_baseline_full <- xgb.train(
    params  = c(params[names(params) != "scale_pos_weight"],
                list(scale_pos_weight = sum(y == 0) / sum(y == 1))),
    data    = xgb.DMatrix(data = X, label = y),
    nrounds = round(mean(auc_baseline * 0 + fit$best_iteration)),
    verbose = 0
  )
  imp_baseline <- xgb.importance(model = fit_baseline_full)

  # ==========================================================================
  #  2. MODELE EOF
  # ==========================================================================
  cat("  → EOF\n")

  df_trend <- ML_data_mono %>%
    dplyr::select(Numeric_sex, Age_sc, LngtClassGrouped_sc, Age_x_Lngt_sc,
                  Cohort_num_sc, Latitude, Longitude, Latitude_x_Longitude,
                  Depth, julian_day, Area_fact,
                  starts_with("trend_"), starts_with("T_PC")) %>%
    mutate(Area_fact = factor(Area_fact)) %>%
    na.omit()

  X <- model.matrix(~ . - Numeric_sex, data = df_trend)
  y <- df_trend$Numeric_sex

  set.seed(42)
  folds   <- createFolds(factor(y), k = k, list = TRUE, returnTrain = FALSE)
  auc_eof <- numeric(n_folds)

  for (i in seq_len(n_folds)) {
    cat("── Fold", i, "/", n_folds, "\n")
    idx_test  <- folds[[i]]
    idx_train <- setdiff(seq_len(nrow(X)), idx_test)

    params <- list(
      objective        = "binary:logistic",
      eval_metric      = "auc",
      eta              = 0.05,
      max_depth        = 6,
      subsample        = 0.8,
      colsample_bytree = 0.8,
      scale_pos_weight = sum(y[idx_train] == 0) / sum(y[idx_train] == 1)
    )

    dtrain <- xgb.DMatrix(data = X[idx_train, ], label = y[idx_train])
    dtest  <- xgb.DMatrix(data = X[idx_test, ],  label = y[idx_test])

    fit <- xgb.train(
      params                = params,
      data                  = dtrain,
      nrounds               = n_rounds,
      evals                 = list(train = dtrain, test = dtest),
      early_stopping_rounds = early_stop,
      verbose               = 0
    )

    preds       <- predict(fit, dtest)
    auc_eof[i]  <- as.numeric(auc(roc(y[idx_test], preds, quiet = TRUE)))
  }

  cat("\n── EOF AUC :", round(mean(auc_eof), 4),
      "± sd :", round(sd(auc_eof), 4), "\n")

  # Modèle final EOF
  fit_eof_full <- xgb.train(
    params  = c(params[names(params) != "scale_pos_weight"],
                list(scale_pos_weight = sum(y == 0) / sum(y == 1))),
    data    = xgb.DMatrix(data = X, label = y),
    nrounds = fit$best_iteration,
    verbose = 0
  )
  imp_eof <- xgb.importance(model = fit_eof_full)

  # ==========================================================================
  #  SAUVEGARDE
  # ==========================================================================
  saveRDS(fit_baseline_full, file.path(dir_models, paste0(sp_slug, "_baseline.rds")))
  saveRDS(fit_eof_full,      file.path(dir_models, paste0(sp_slug, "_eof.rds")))

  results_all[[sp]] <- list(
    baseline = list(auc_folds  = auc_baseline,
                    auc_mean   = mean(auc_baseline),
                    auc_sd     = sd(auc_baseline),
                    importance = imp_baseline),
    eof      = list(auc_folds  = auc_eof,
                    auc_mean   = mean(auc_eof),
                    auc_sd     = sd(auc_eof),
                    importance = imp_eof)
  )

  saveRDS(results_all[[sp]],
          file.path(dir_results, paste0(sp_slug, "_results.rds")))

  cat("  ✔ Sauvegardé.\n")
}

# ── Tableau récapitulatif AUC ─────────────────────────────────────────────────
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