# ================================================================
# PIPELINE : XGBOOST — FICHIERS PRE-JOINTURES PAR ESPECE
# ================================================================

library(xgboost)
library(pROC)
library(dplyr)
library(Matrix)

# ================================================================
# PARAMETRES
# ================================================================
SEED          <- 42
TEST_RATIO    <- 0.2
NFOLDS        <- 1
TARGET        <- "Numeric_sex"
OUT_DIR       <- "../results_mensual_PC1_to_PC65_xgb_low_memory"

XGB_PARAMS <- list(
  objective        = "binary:logistic",
  eval_metric      = "auc",
  eta              = 0.05,
  max_depth        = 4,
  subsample        = 0.8,
  colsample_bytree = 0.8,
  min_child_weight = 5,
  nthread          = 1
)

XGB_NROUNDS_MAX <- 5000
XGB_EARLY_STOP  <- 30

FORMULA_BASE <- ~ Age_sc + LngtClassGrouped_sc + Age_x_Lngt_sc +
  Cohorte_num_sc + Area + Cohorte_fact - 1

# ================================================================
# FILTRE PC
# ================================================================
pc_keep  <- 1:10
var_keep <- c("T")

# ================================================================
# HELPERS
# ================================================================
.stratified_split <- function(data_sp, target, test_ratio, seed) {
  set.seed(seed)
  idx_male   <- which(data_sp[[target]] == 1)
  idx_female <- which(data_sp[[target]] == 0)
  idx_test <- c(
    sample(idx_male,   floor(length(idx_male)   * test_ratio)),
    sample(idx_female, floor(length(idx_female) * test_ratio))
  )
  list(train = data_sp[-idx_test, ], test = data_sp[idx_test, ])
}

.fit_auc_xgb <- function(X_train, y_train, X_test, y_test,
                         params, nrounds_max, early_stop, nfolds, seed) {
  y_train <- as.numeric(y_train)
  y_test  <- as.numeric(y_test)

  dtrain <- xgb.DMatrix(X_train, label = y_train)
  dtest  <- xgb.DMatrix(X_test,  label = y_test)

  if (nfolds > 1) {
    set.seed(seed)
    cv_fit <- xgb.cv(
      params = params, data = dtrain,
      nrounds = nrounds_max, nfold = nfolds,
      early_stopping_rounds = early_stop,
      verbose = 0, stratified = TRUE
    )
    best_round <- cv_fit$best_iteration
    if (is.null(best_round) || is.na(best_round) || best_round == 0)
      best_round <- which.max(cv_fit$evaluation_log$test_auc_mean)

  } else {
    model <- xgb.train(
      params = params, data = dtrain,
      nrounds = nrounds_max,
      evals = list(val = dtest),
      early_stopping_rounds = early_stop,
      verbose = 0
    )
    best_round <- model$best_iteration
    if (is.null(best_round) || is.na(best_round) || best_round == 0)
      best_round <- nrounds_max
  }

  final_model <- xgb.train(
    params = params, data = dtrain,
    nrounds = best_round, verbose = 0
  )

  preds   <- predict(final_model, dtest)
  auc_val <- as.numeric(auc(roc(y_test, preds, quiet = TRUE)))

  rm(dtrain, dtest, final_model); gc()
  list(auc = auc_val, best_nround = best_round)
}

# ================================================================
# PIPELINE
# ================================================================
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

files       <- list.files("../data/species_prejoined", full.names = TRUE)
results_all <- data.frame()

cat("Nb fichiers détectés :", length(files), "\n")

for (f in files) {

  sp <- tools::file_path_sans_ext(basename(f))

  cat("\n═══════════════════════════════\n")
  cat("ESPECE :", sp, "\n")

  # ============================================================
  # LECTURE ET SEPARATION BASE / TEMPERATURE
  # ============================================================
  data_raw <- readRDS(f)

  if (nrow(data_raw) == 0) {
    cat("  ⚠️ SKIP fichier vide\n"); next
  }

  # Identifier les colonnes température
  pc_pattern  <- paste0("_PC", pc_keep, "_", collapse = "|")
  var_pattern <- paste0("^(", paste(var_keep, collapse = "|"), ")")

  temp_cols <- names(data_raw)[
    grepl(pc_pattern,  names(data_raw)) &
    grepl(var_pattern, names(data_raw))
  ]

  # Colonnes de base = tout sauf les colonnes température
  base_cols <- setdiff(names(data_raw), names(data_raw)[grepl("^T", names(data_raw))])

  cat("  Nb colonnes base        :", length(base_cols), "\n")
  cat("  Nb colonnes température :", length(temp_cols), "\n")
  cat("  Dimensions raw          :", dim(data_raw),     "\n")

  # Séparer proprement
  data_base <- data_raw[, base_cols]
  data_temp <- data_raw[, temp_cols, drop = FALSE]

  rm(data_raw); gc()

  # ============================================================
  # PREPARATION BASE
  # ============================================================
  if (!"Age_x_Lngt_sc" %in% names(data_base)) {
    data_base <- data_base %>%
      mutate(Age_x_Lngt_sc = as.numeric(scale(Age_sc * LngtClassGrouped_sc)))
  }

  # Split sur les indices (partagés entre base et temp)
  set.seed(SEED)
  idx_male   <- which(data_base[[TARGET]] == 1)
  idx_female <- which(data_base[[TARGET]] == 0)
  idx_test   <- c(
    sample(idx_male,   floor(length(idx_male)   * TEST_RATIO)),
    sample(idx_female, floor(length(idx_female) * TEST_RATIO))
  )
  idx_train <- setdiff(seq_len(nrow(data_base)), idx_test)

  # Données de base splitées
  base_train <- data_base[idx_train, ]
  base_test  <- data_base[idx_test,  ]

  y_train <- base_train[[TARGET]]
  y_test  <- base_test[[TARGET]]

  rm(data_base); gc()

  # Données température splitées
  temp_train <- data_temp[idx_train, , drop = FALSE]
  temp_test  <- data_temp[idx_test,  , drop = FALSE]

  rm(data_temp); gc()

  # ============================================================
  # BASE MODEL
  # ============================================================
  cat("  BASE...\n")

  X_base_train <- sparse.model.matrix(FORMULA_BASE, data = base_train)
  X_base_test  <- sparse.model.matrix(FORMULA_BASE, data = base_test)

  res_base <- .fit_auc_xgb(
    X_base_train, y_train,
    X_base_test,  y_test,
    XGB_PARAMS, XGB_NROUNDS_MAX, XGB_EARLY_STOP, NFOLDS, SEED
  )
  auc_base <- res_base$auc
  cat("  AUC baseline =", auc_base, "\n")

  rm(X_base_train, X_base_test); gc()

  # ============================================================
  # FULL MODEL (base + température)
  # ============================================================
  cat("  FULL TEMP...\n")

  X_full_train <- sparse.model.matrix(FORMULA_BASE, data = base_train)
  X_full_test  <- sparse.model.matrix(FORMULA_BASE, data = base_test)

  rm(base_train, base_test); gc()

  X_temp_train <- Matrix(data.matrix(temp_train), sparse = TRUE)
  X_temp_test  <- Matrix(data.matrix(temp_test),  sparse = TRUE)

  rm(temp_train, temp_test); gc()

  X_train_full_temp <- cbind2(X_full_train, X_temp_train)
  X_test_full_temp  <- cbind2(X_full_test,  X_temp_test)

  rm(X_full_train, X_full_test, X_temp_train, X_temp_test); gc()

  cat("  RAM train matrix:", format(object.size(X_train_full_temp), "GB"), "\n")

  res_full <- .fit_auc_xgb(
    X_train_full_temp, y_train,
    X_test_full_temp,  y_test,
    XGB_PARAMS, XGB_NROUNDS_MAX, XGB_EARLY_STOP, NFOLDS, SEED
  )
  auc_full <- res_full$auc
  delta    <- auc_full - auc_base

  cat("  AUC temp =", auc_full, "| Δ =", delta, "\n")

  rm(X_train_full_temp, X_test_full_temp); gc()

  # ============================================================
  # STOCKAGE
  # ============================================================
  results_all <- rbind(results_all, data.frame(
    Species     = sp,
    n_temp_col  = length(temp_cols),
    auc_base    = auc_base,
    auc_temp    = auc_full,
    delta_auc   = delta,
    nround_base = res_base$best_nround,
    nround_full = res_full$best_nround
  ))
}

# ================================================================
# OUTPUT
# ================================================================
print(results_all)

write.csv(results_all,
          file.path(OUT_DIR, "results_all_species_xgb.csv"),
          row.names = FALSE)

cat("\n✓ Pipeline terminé\n")