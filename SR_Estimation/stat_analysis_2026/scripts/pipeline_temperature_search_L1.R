# ================================================================
# PIPELINE : Modèle température complet (tous PCs, toutes variables)
# ================================================================

library(glmnet)
library(pROC)
library(ggplot2)
library(dplyr)

# ================================================================
# PARAMETRES
# ================================================================
SEED       <- 42
TEST_RATIO <- 0.2
NFOLDS     <- 10
TARGET     <- "Numeric_sex"
OUT_DIR    <- "../results_mensual_PC1_to_PC65"

ALPHAS <- c("L1" = 1)

FORMULA_BASE <- ~ Age_sc + LngtClassGrouped_sc + Age_x_Lngt_sc +
                  Cohorte_num_sc + Area + Cohorte_fact - 1

# ================================================================
# PREPARATION DES DONNEES
# ================================================================
pc_keep     <- 1:63
var_keep    <- c("T_mean", "T_var", "T_grad")
pc_pattern  <- paste0("_PC", pc_keep, "_", collapse = "|")
var_pattern <- paste0("^(", paste(var_keep, collapse = "|"), ")")
cols_keep   <- c("year", grep(pc_pattern, names(data_eof_flatten), value = TRUE))
cols_keep   <- cols_keep[grepl(var_pattern, cols_keep) | cols_keep == "year"]

data_model_temp <- data_expanded %>%
  left_join(
    data_eof_flatten %>% select(all_of(cols_keep)),
    by = c("Cohorte_num" = "year"))

data_full <- data_model_temp %>%
  mutate(Age_x_Lngt_sc = as.numeric(scale(Age_sc * LngtClassGrouped_sc)))

data_base <- data_expanded %>%
  mutate(Age_x_Lngt_sc = as.numeric(scale(Age_sc * LngtClassGrouped_sc)))

# -- Groupes PC puis toutes les colonnes température --
PC_GROUPS <- lapply(pc_keep, function(k) {
  cols <- grep(paste0("_PC", k, "_"), colnames(data_model_temp), value = TRUE)
  cols[grepl(var_pattern, cols)]
})
names(PC_GROUPS) <- paste0("PC", pc_keep)

all_temp_cols <- unlist(PC_GROUPS, use.names = FALSE)

cat("Groupes PC :\n")
for (nm in names(PC_GROUPS))
  cat(" ", nm, "→", length(PC_GROUPS[[nm]]), "colonnes\n")

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

.fit_auc <- function(X_train, y_train, X_test, y_test, alpha, nfolds) {
  cv_fit <- tryCatch(
    cv.glmnet(
      x = X_train,
      y = y_train,
      family = "binomial",
      alpha = alpha,
      nfolds = nfolds,
      type.measure = "deviance"
    ),
    error = function(e) { cat("    [ERROR cv.glmnet]", conditionMessage(e), "\n"); NULL }
  )
  if (is.null(cv_fit)) return(NA_real_)
  preds <- as.numeric(predict(cv_fit, newx = X_test,
                               s = "lambda.1se", type = "response"))
  if (length(unique(y_test)) < 2) return(NA_real_)
  as.numeric(auc(roc(y_test, preds, quiet = TRUE)))
}

# ================================================================
# BOUCLE PRINCIPALE
# ================================================================
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

species_list <- levels(data_full$Species)
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

  cat("  Split — train=", nrow(spl_base$train),
      "| test=", nrow(spl_base$test), "\n")

  cat("  model.matrix... ")
  X_base_train <- model.matrix(FORMULA_BASE, data = spl_base$train)
  X_base_test  <- model.matrix(FORMULA_BASE, data = spl_base$test)
  X_full_train <- model.matrix(FORMULA_BASE, data = spl_full$train)
  X_full_test  <- model.matrix(FORMULA_BASE, data = spl_full$test)
  cat("OK (", ncol(X_base_train), "colonnes)\n")

  y_base_train <- spl_base$train[[TARGET]]
  y_base_test  <- spl_base$test[[TARGET]]
  y_full_train <- spl_full$train[[TARGET]]
  y_full_test  <- spl_full$test[[TARGET]]

  sp_results <- data.frame()

  for (alpha_name in names(ALPHAS)) {
    alpha_val <- ALPHAS[[alpha_name]]

    cat("\n  ── Pénalisation :", alpha_name,
        "(alpha=", alpha_val, ") ──\n")

    # -- Baseline --
    t0 <- proc.time()["elapsed"]
    cat("    [baseline] cv.glmnet... ")
    auc_base <- .fit_auc(X_base_train, y_base_train,
                          X_base_test,  y_base_test,
                          alpha_val, NFOLDS)
    cat("AUC =", round(auc_base, 4),
        "| durée :", round(proc.time()["elapsed"] - t0, 1), "sec\n")

    # -- Modèle unique : toutes les colonnes température --
    na_check <- colSums(is.na(spl_full$train[, all_temp_cols, drop = FALSE]))
    if (any(na_check > 0)) {
      cat("    [FULL TEMP] NAs détectés dans",
          sum(na_check > 0), "colonnes — ignoré\n")
    } else {
      X_train_full_temp <- cbind(
        X_full_train,
        as.matrix(spl_full$train[, all_temp_cols, drop = FALSE])
      )
      X_test_full_temp <- cbind(
        X_full_test,
        as.matrix(spl_full$test[, all_temp_cols, drop = FALSE])
      )

      t0 <- proc.time()["elapsed"]
      cat("    [FULL TEMP] cv.glmnet (",
          length(all_temp_cols), "cols temp +",
          ncol(X_full_train), "cols base)... ")

      auc_full_temp <- .fit_auc(
        X_train_full_temp, y_full_train,
        X_test_full_temp,  y_full_test,
        alpha_val, NFOLDS
      )

      delta <- auc_full_temp - auc_base
      cat("AUC =", round(auc_full_temp, 4),
          "| Δ =", sprintf("%+.4f", delta),
          "| durée :", round(proc.time()["elapsed"] - t0, 1), "sec\n")

      sp_results <- rbind(sp_results, data.frame(
        Species    = sp,
        alpha_name = alpha_name,
        alpha_val  = alpha_val,
        pc_label   = "ALL_TEMP",
        auc_base   = auc_base,
        auc_temp   = auc_full_temp,
        delta_auc  = delta,
        stringsAsFactors = FALSE
      ))
    }

    # ── Summary txt ──
    if (nrow(sp_results) > 0) {
      df_a <- sp_results %>% filter(alpha_name == !!alpha_name)
      summary_lines <- c(
        "═══════════════════════════════════════════",
        paste0("  RÉSUMÉ — Espèce : ", sp),
        "═══════════════════════════════════════════",
        "",
        paste0("── ", alpha_name, " (alpha=", alpha_val, ") ──"),
        paste0("   AUC baseline      : ", round(unique(df_a$auc_base), 4)),
        paste0("   AUC modèle temp   : ", round(df_a$auc_temp, 4)),
        paste0("   Δ AUC             : ", sprintf("%+.4f", df_a$delta_auc)),
        paste0("   N colonnes temp   : ", length(all_temp_cols)),
        ""
      )
      writeLines(summary_lines,
                 file.path(sp_dir, paste0("summary_", sp_safe, ".txt")))
      cat("  → Summary sauvegardé\n")
    }
  }

  results_all <- rbind(results_all, sp_results)
}

# ================================================================
# TABLE GLOBALE
# ================================================================
cat("\n\n══════════════════════════════════════════════════════\n")
cat("  RÉSULTATS PAR ESPECE\n")
cat("══════════════════════════════════════════════════════\n")
print(results_all %>% arrange(desc(delta_auc)), n = Inf)

write.csv(results_all,
          file.path(OUT_DIR, "results_all_species_full_temp.csv"),
          row.names = FALSE)

cat("\n✓ Pipeline terminé. Résultats dans :", OUT_DIR, "\n")
