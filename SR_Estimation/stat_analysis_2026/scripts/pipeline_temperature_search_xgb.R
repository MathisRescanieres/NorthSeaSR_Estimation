# ================================================================
# PIPELINE : Modèle température complet — XGBoost
# ================================================================

library(xgboost)
library(pROC)
library(ggplot2)
library(dplyr)

# ================================================================
# PARAMETRES
# ================================================================
SEED          <- 42
TEST_RATIO    <- 0.2
NFOLDS        <- 10
TARGET        <- "Numeric_sex"
OUT_DIR       <- "../results_mensual_PC1_to_PC65_xgb_Tmean"

XGB_PARAMS <- list(
  objective        = "binary:logistic",
  eval_metric      = "auc",
  eta              = 0.05,
  max_depth        = 4,
  subsample        = 0.8,
  colsample_bytree = 0.8,
  min_child_weight = 5,
  nthread          = max(1, parallel::detectCores() - 5)
)
XGB_NROUNDS_MAX <- 5000
XGB_EARLY_STOP  <- 30

FORMULA_BASE <- ~ Age_sc + LngtClassGrouped_sc + Age_x_Lngt_sc +
                  Cohorte_num_sc + Area + Cohorte_fact - 1

# ================================================================
# PREPARATION DES DONNEES
# ================================================================
pc_keep     <- 1:63
var_keep    <- c("T")
pc_pattern  <- paste0("_PC", pc_keep, "_", collapse = "|")
var_pattern <- paste0("^(", paste(var_keep, collapse = "|"), ")")
cols_keep   <- c("year", grep(pc_pattern, names(data_eof_flatten), value = TRUE))
cols_keep   <- cols_keep[grepl(var_pattern, cols_keep) | cols_keep == "year"]

data_model_temp <- data_expanded %>%
  left_join(
    data_eof_flatten %>% select(all_of(cols_keep)),
    by = c("Cohorte_num" = "year")
  )

data_full <- data_model_temp %>%
  mutate(Age_x_Lngt_sc = as.numeric(scale(Age_sc * LngtClassGrouped_sc)))

data_base <- data_expanded %>%
  mutate(Age_x_Lngt_sc = as.numeric(scale(Age_sc * LngtClassGrouped_sc)))

# -- Groupes PC et colonnes température --
PC_GROUPS <- lapply(pc_keep, function(k) {
  cols <- grep(paste0("_PC", k, "_"), colnames(data_model_temp), value = TRUE)
  cols[grepl(var_pattern, cols)]
})
names(PC_GROUPS) <- paste0("PC", pc_keep)

all_temp_cols <- unlist(PC_GROUPS, use.names = FALSE)

cat("Groupes PC :\n")
for (nm in names(PC_GROUPS))
  cat(" ", nm, "→", length(PC_GROUPS[[nm]]), "colonnes\n")
cat("Total colonnes température :", length(all_temp_cols), "\n")

# ================================================================
# HELPERS
# ================================================================

.stratified_split <- function(data_sp, target, test_ratio, seed) {
  set.seed(seed)
  idx_male   <- which(data_sp[[target]] == 1)
  idx_female <- which(data_sp[[target]] == 0)
  idx_test   <- c(
    sample(idx_male,   size = floor(length(idx_male)   * test_ratio)),
    sample(idx_female, size = floor(length(idx_female) * test_ratio))
  )
  list(
    train = data_sp[setdiff(seq_len(nrow(data_sp)), idx_test), ],
    test  = data_sp[idx_test, ]
  )
}

.fit_auc_xgb <- function(X_train, y_train, X_test, y_test,
                          params, nrounds_max, early_stop, nfolds, seed) {

  y_train <- as.numeric(y_train)
  y_test  <- as.numeric(y_test)

  # Sécurité : forcer matrix dense
  X_train <- as.matrix(X_train)
  X_test  <- as.matrix(X_test)

  dtrain <- xgb.DMatrix(data = X_train, label = y_train)
  dtest  <- xgb.DMatrix(data = X_test,  label = y_test)

  # CV pour trouver le meilleur nombre de rounds
  set.seed(seed)
  cv_fit <- tryCatch(
    xgb.cv(
      params                = params,
      data                  = dtrain,
      nrounds               = nrounds_max,
      nfold                 = nfolds,
      early_stopping_rounds = early_stop,
      verbose               = 0,
      stratified            = TRUE   # CV stratifiée sur la cible binaire
    ),
    error = function(e) {
      cat("    [ERROR xgb.cv]", conditionMessage(e), "\n")
      NULL
    }
  )
  if (is.null(cv_fit)) return(list(auc = NA_real_, best_nround = NA_integer_))

  best_round <- cv_fit$best_iteration

  # Sécurité : best_iteration peut être NULL/NA/0 si early stopping n'a pas déclenché
  if (is.null(best_round) || is.na(best_round) || best_round == 0) {
    # Fallback : prendre le round avec le meilleur AUC moyen en CV
    best_round <- which.max(cv_fit$evaluation_log$test_auc_mean)
    cat("(best_round fallback =", best_round, ") ")
  } else {
    cat("(best_round =", best_round, ") ")
  }

  # Dernière sécurité
  if (is.null(best_round) || length(best_round) == 0 || best_round == 0) {
    best_round <- nrounds_max
    cat("(best_round forcé à nrounds_max =", best_round, ") ")
  }

  # Entraînement final sur tout le train
  final_model <- xgb.train(
    params  = params,
    data    = dtrain,
    nrounds = best_round,
    verbose = 0
  )

  preds <- predict(final_model, dtest)

  if (length(unique(y_test)) < 2) {
    return(list(auc = NA_real_, best_nround = best_round))
  }

  auc_val <- as.numeric(auc(roc(y_test, preds, quiet = TRUE)))
  list(auc = auc_val, best_nround = best_round)
}

# ================================================================
# BOUCLE PRINCIPALE
# ================================================================
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# species_list <- levels(data_full$Species)
species_list <- "Limanda limanda"
results_all  <- data.frame()

for (sp in species_list) {
  cat("\n════════════════════════════════════════\n")
  cat("  ESPECE :", sp, "\n")
  cat("════════════════════════════════════════\n")

  sp_safe <- gsub(" ", "_", sp)
  sp_dir  <- file.path(OUT_DIR, sp_safe)
  dir.create(sp_dir, recursive = TRUE, showWarnings = FALSE)

  data_sp_base <- data_base %>% filter(Species == sp)
  data_sp_full <- data_full %>% filter(Species == sp)

  spl_base <- .stratified_split(data_sp_base, TARGET, TEST_RATIO, SEED)
  spl_full <- .stratified_split(data_sp_full, TARGET, TEST_RATIO, SEED)

  cat("  Split — train =", nrow(spl_base$train),
      "| test =", nrow(spl_base$test), "\n")

  # -- model.matrix (variables de base) --
  cat("  model.matrix... ")
  X_base_train <- model.matrix(FORMULA_BASE, data = spl_base$train)
  X_base_test  <- model.matrix(FORMULA_BASE, data = spl_base$test)
  X_full_train <- model.matrix(FORMULA_BASE, data = spl_full$train)
  X_full_test  <- model.matrix(FORMULA_BASE, data = spl_full$test)
  cat("OK (", ncol(X_base_train), "colonnes base)\n")

  y_base_train <- spl_base$train[[TARGET]]
  y_base_test  <- spl_base$test[[TARGET]]
  y_full_train <- spl_full$train[[TARGET]]
  y_full_test  <- spl_full$test[[TARGET]]

  # ── Baseline (variables de base uniquement) ──────────────────
  t0 <- proc.time()["elapsed"]
  cat("  [BASELINE] xgb.cv + xgb.train... ")

  res_base <- .fit_auc_xgb(
    X_base_train, y_base_train,
    X_base_test,  y_base_test,
    params      = XGB_PARAMS,
    nrounds_max = XGB_NROUNDS_MAX,
    early_stop  = XGB_EARLY_STOP,
    nfolds      = NFOLDS,
    seed        = SEED
  )
  auc_base <- res_base$auc

  cat("AUC =", round(auc_base, 4),
      "| durée :", round(proc.time()["elapsed"] - t0, 1), "sec\n")

  # ── Modèle FULL TEMP (base + toutes colonnes température) ────
  na_check <- colSums(is.na(spl_full$train[, all_temp_cols, drop = FALSE]))

  if (any(na_check > 0)) {
    cat("  [FULL TEMP] NAs détectés dans",
        sum(na_check > 0), "colonnes — modèle ignoré\n")
    next
  }

  X_train_full_temp <- cbind(
    X_full_train,
    as.matrix(spl_full$train[, all_temp_cols, drop = FALSE])
  )
  X_test_full_temp <- cbind(
    X_full_test,
    as.matrix(spl_full$test[, all_temp_cols, drop = FALSE])
  )

  t0 <- proc.time()["elapsed"]
  cat("  [FULL TEMP] xgb.cv + xgb.train (",
      length(all_temp_cols), "cols temp +",
      ncol(X_full_train), "cols base)... ")

  res_full <- .fit_auc_xgb(
    X_train_full_temp, y_full_train,
    X_test_full_temp,  y_full_test,
    params      = XGB_PARAMS,
    nrounds_max = XGB_NROUNDS_MAX,
    early_stop  = XGB_EARLY_STOP,
    nfolds      = NFOLDS,
    seed        = SEED
  )
  auc_full_temp <- res_full$auc
  delta         <- auc_full_temp - auc_base

  cat("AUC =", round(auc_full_temp, 4),
      "| Δ =", sprintf("%+.4f", delta),
      "| durée :", round(proc.time()["elapsed"] - t0, 1), "sec\n")

  # -- Sauvegarde résultats espèce --
  sp_results <- data.frame(
    Species        = sp,
    model          = "XGBoost",
    pc_label       = "ALL_TEMP",
    auc_base       = auc_base,
    auc_temp       = auc_full_temp,
    delta_auc      = delta,
    best_nround_base = res_base$best_nround,
    best_nround_temp = res_full$best_nround,
    n_temp_cols    = length(all_temp_cols),
    stringsAsFactors = FALSE
  )

  # ── Summary txt ──
  summary_lines <- c(
    "═══════════════════════════════════════════",
    paste0("  RÉSUMÉ XGBoost — Espèce : ", sp),
    "═══════════════════════════════════════════",
    "",
    paste0("   AUC baseline        : ", round(auc_base, 4),
           "  (best_round = ", res_base$best_nround, ")"),
    paste0("   AUC modèle temp     : ", round(auc_full_temp, 4),
           "  (best_round = ", res_full$best_nround, ")"),
    paste0("   Δ AUC               : ", sprintf("%+.4f", delta)),
    paste0("   N colonnes temp     : ", length(all_temp_cols)),
    "",
    "── Hyperparamètres XGBoost ──",
    paste0("   eta              : ", XGB_PARAMS$eta),
    paste0("   max_depth        : ", XGB_PARAMS$max_depth),
    paste0("   subsample        : ", XGB_PARAMS$subsample),
    paste0("   colsample_bytree : ", XGB_PARAMS$colsample_bytree),
    paste0("   min_child_weight : ", XGB_PARAMS$min_child_weight),
    paste0("   early_stopping   : ", XGB_EARLY_STOP),
    ""
  )
  writeLines(summary_lines,
             file.path(sp_dir, paste0("summary_", sp_safe, ".txt")))
  cat("  → Summary sauvegardé\n")

  results_all <- rbind(results_all, sp_results)
}

# ================================================================
# TABLE GLOBALE
# ================================================================
cat("\n\n══════════════════════════════════════════════════════\n")
cat("  RÉSULTATS PAR ESPECE — XGBoost\n")
cat("══════════════════════════════════════════════════════\n")
print(results_all %>% arrange(desc(delta_auc)), n = Inf)

write.csv(results_all,
          file.path(OUT_DIR, "results_all_species_xgb.csv"),
          row.names = FALSE)

cat("\n✓ Pipeline terminé. Résultats dans :", OUT_DIR, "\n")
