load("../scripts/env_ML_models.RData")

# ================================================================
# PIPELINE : XGBoost LOW MEMORY (JOIN PAR ESPECE)
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
# PREPARATION COLONNES TEMP (SANS JOIN GLOBAL)
# ================================================================
pc_keep     <- 1:63
var_keep    <- c("T")

pc_pattern  <- paste0("_PC", pc_keep, "_", collapse = "|")
var_pattern <- paste0("^(", paste(var_keep, collapse = "|"), ")")

cols_keep <- c("year", grep(pc_pattern, names(data_eof_flatten), value = TRUE))
cols_keep <- cols_keep[grepl(var_pattern, cols_keep) | cols_keep == "year"]

# noms des colonnes température uniquement
all_temp_cols <- cols_keep[cols_keep != "year"]

cat("Total colonnes température :", length(all_temp_cols), "\n")

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

  list(
    train = data_sp[-idx_test, ],
    test  = data_sp[idx_test, ]
  )
}

.fit_auc_xgb <- function(X_train, y_train, X_test, y_test,
                         params, nrounds_max, early_stop, nfolds, seed) {

  y_train <- as.numeric(y_train)
  y_test  <- as.numeric(y_test)

  dtrain <- xgb.DMatrix(data = X_train, label = y_train)
  dtest  <- xgb.DMatrix(data = X_test,  label = y_test)

  if (nfolds > 1) {

    set.seed(seed)
    cv_fit <- xgb.cv(
      params = params,
      data = dtrain,
      nrounds = nrounds_max,
      nfold = nfolds,
      early_stopping_rounds = early_stop,
      verbose = 0,
      stratified = TRUE
    )

    best_round <- cv_fit$best_iteration

    if (is.null(best_round) || is.na(best_round) || best_round == 0) {
      best_round <- which.max(cv_fit$evaluation_log$test_auc_mean)
    }

  } else {

    model <- xgb.train(
      params = params,
      data = dtrain,
      nrounds = nrounds_max,
      evals = list(val = dtest),
      early_stopping_rounds = early_stop,
      verbose = 0
    )

    best_round <- model$best_iteration

    if (is.null(best_round) || is.na(best_round) || best_round == 0) {
      best_round <- nrounds_max
    }
  }

  final_model <- xgb.train(
    params = params,
    data = dtrain,
    nrounds = best_round,
    verbose = 0
  )

  preds <- predict(final_model, dtest)
  auc_val <- as.numeric(auc(roc(y_test, preds, quiet = TRUE)))

  rm(dtrain, dtest, final_model)
  gc()

  list(auc = auc_val, best_nround = best_round)
}

# ================================================================
# BOUCLE PRINCIPALE
# ================================================================
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

species_list <- "Limanda limanda"
results_all  <- data.frame()

for (sp in species_list) {

  cat("\n═══════════════════════════════\n")
  cat("ESPECE :", sp, "\n")

  sp_safe <- gsub(" ", "_", sp)
  sp_dir  <- file.path(OUT_DIR, sp_safe)
  dir.create(sp_dir, recursive = TRUE, showWarnings = FALSE)

  # ================= BASE DATA =================
  data_sp_base <- data_expanded %>%
    filter(Species == sp) %>%
    mutate(Age_x_Lngt_sc = as.numeric(scale(Age_sc * LngtClassGrouped_sc)))

  spl_base <- .stratified_split(data_sp_base, TARGET, TEST_RATIO, SEED)

  # ================= BASE MODEL =================
  cat("  BASE...\n")

  X_base_train <- sparse.model.matrix(FORMULA_BASE, data = spl_base$train)
  X_base_test  <- sparse.model.matrix(FORMULA_BASE, data = spl_base$test)

  y_base_train <- spl_base$train[[TARGET]]
  y_base_test  <- spl_base$test[[TARGET]]

  res_base <- .fit_auc_xgb(
    X_base_train, y_base_train,
    X_base_test,  y_base_test,
    XGB_PARAMS, XGB_NROUNDS_MAX, XGB_EARLY_STOP,
    NFOLDS, SEED
  )

  auc_base <- res_base$auc
  cat("  AUC baseline =", auc_base, "\n")

  rm(X_base_train, X_base_test)
  gc()

  # ================= FULL TEMP (JOIN ICI) =================
  cat("  FULL TEMP (join par espèce)...\n")

  data_sp_full <- data_sp_base %>%
    left_join(
      data_eof_flatten %>% select(all_of(cols_keep)),
      by = c("Cohorte_num" = "year")
    )

  spl_full <- .stratified_split(data_sp_full, TARGET, TEST_RATIO, SEED)

  # matrices temp
  temp_train <- sparse.model.matrix(~ . - 1, data = spl_full$train[, all_temp_cols])
  temp_test  <- sparse.model.matrix(~ . - 1, data = spl_full$test[, all_temp_cols])

  X_full_train <- sparse.model.matrix(FORMULA_BASE, data = spl_full$train)
  X_full_test  <- sparse.model.matrix(FORMULA_BASE, data = spl_full$test)

  X_train_full_temp <- cbind2(X_full_train, temp_train)
  X_test_full_temp  <- cbind2(X_full_test,  temp_test)

  cat("  Taille X_train_full_temp :",
      format(object.size(X_train_full_temp), units = "GB"), "\n")

  y_full_train <- spl_full$train[[TARGET]]
  y_full_test  <- spl_full$test[[TARGET]]

  res_full <- .fit_auc_xgb(
    X_train_full_temp, y_full_train,
    X_test_full_temp,  y_full_test,
    XGB_PARAMS, XGB_NROUNDS_MAX, XGB_EARLY_STOP,
    NFOLDS, SEED
  )

  auc_full <- res_full$auc
  delta    <- auc_full - auc_base

  cat("  AUC temp =", auc_full, "| Δ =", delta, "\n")

  # nettoyage massif
  rm(
    data_sp_full, spl_full,
    X_train_full_temp, X_test_full_temp,
    temp_train, temp_test,
    X_full_train, X_full_test
  )
  gc()

  results_all <- rbind(results_all, data.frame(
    Species = sp,
    auc_base = auc_base,
    auc_temp = auc_full,
    delta_auc = delta
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