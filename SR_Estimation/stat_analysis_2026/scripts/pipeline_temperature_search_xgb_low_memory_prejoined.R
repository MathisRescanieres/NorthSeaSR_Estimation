library(xgboost)
library(pROC)
library(dplyr)
library(Matrix)

SEED <- 42
TEST_RATIO <- 0.2
TARGET <- "Numeric_sex"

FORMULA_BASE <- ~ Age_sc + LngtClassGrouped_sc + Age_x_Lngt_sc +
  Cohorte_num_sc + Area + Cohorte_fact - 1

cat("Nb fichiers détectés :", length(list.files("../data/species_prejoined")), "\n")

files <- list.files("../data/species_prejoined", full.names = TRUE)

cat("Liste fichiers :\n")
print(files)

results <- data.frame()

# ================================================================
# SPLIT
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

# ================================================================
# AUC FUNCTION
# ================================================================
.fit_auc_xgb <- function(X_train, y_train, X_test, y_test,
                         params, nrounds_max, early_stop, nfolds, seed) {

  y_train <- as.numeric(y_train)
  y_test  <- as.numeric(y_test)

  dtrain <- xgb.DMatrix(X_train, label = y_train)
  dtest  <- xgb.DMatrix(X_test,  label = y_test)

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
# LOOP
# ================================================================
for (i in seq_along(files)) {

  f <- files[i]

  cat("\n====================================\n")
  cat("FILE", i, ":", f, "\n")

  # ---- READ ----
  data_sp <- readRDS(f)
  
  temp_cols <- names(data_sp)[grepl("^T", names(data_sp))]

  cat("Dimensions data_sp :", dim(data_sp), "\n")

  if (nrow(data_sp) == 0) {
    cat("⚠️ SKIP fichier vide\n")
    next
  }

  # ---- SPLIT ----
  spl <- .stratified_split(data_sp, TARGET, TEST_RATIO, SEED)

  cat("Train dim :", dim(spl$train), "\n")
  cat("Test dim  :", dim(spl$test), "\n")

  if (nrow(spl$train) == 0 || nrow(spl$test) == 0) {
    cat("⚠️ SKIP split vide\n")
    next
  }

  # ---- BASE MODEL ----
  cat("→ BASE MODEL\n")

  X_train <- sparse.model.matrix(FORMULA_BASE, spl$train)
  X_test  <- sparse.model.matrix(FORMULA_BASE, spl$test)

  cat("X_train :", dim(X_train), "\n")
  cat("X_test  :", dim(X_test), "\n")

  auc_base <- .fit_auc_xgb(
    X_train, spl$train[[TARGET]],
    X_test,  spl$test[[TARGET]],
    list(objective="binary:logistic", eval_metric="auc")
  )

  cat("AUC base =", auc_base, "\n")

  # ---- TEMP MATRIX ----
  cat("→ TEMP MATRIX\n")

  temp_train <- Matrix(
    as.matrix(spl$train[, temp_cols, drop = FALSE]),
    sparse = TRUE
  )

  temp_test <- Matrix(
    as.matrix(spl$test[, temp_cols, drop = FALSE]),
    sparse = TRUE
  )

  cat("temp_train :", dim(temp_train), "\n")
  cat("temp_test  :", dim(temp_test), "\n")

  # ---- FULL MODEL ----
  cat("→ FULL MODEL\n")

  X_train_full <- cbind2(X_train, temp_train)
  X_test_full  <- cbind2(X_test, temp_test)

  cat("X_train_full :", dim(X_train_full), "\n")
  cat("X_test_full  :", dim(X_test_full), "\n")

  auc_full <- .fit_auc_xgb(
    X_train_full, spl$train[[TARGET]],
    X_test_full,  spl$test[[TARGET]],
    list(objective="binary:logistic", eval_metric="auc")
  )

  cat("AUC full =", auc_full, "\n")
  cat("DELTA =", auc_full - auc_base, "\n")

  # ---- STORE ----
  results <- rbind(results, data.frame(
    file = basename(f),
    auc_base = auc_base,
    auc_full = auc_full,
    delta = auc_full - auc_base
  ))

  cat("Results rows so far:", nrow(results), "\n")

  rm(data_sp, spl, X_train, X_test, X_train_full, X_test_full)
  gc()
}

# ================================================================
# SAVE
# ================================================================
cat("\nFINAL results dim:", dim(results), "\n")
print(results)

write.csv(results, "../results_prejoined_xgb.csv", row.names = FALSE)

cat("DONE\n")