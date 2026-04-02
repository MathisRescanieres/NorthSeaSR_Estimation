# ================================================================
# PIPELINE : Recherche exhaustive PC température x Pénalisation
# Séquentiel — affichages cat() directs
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
NFOLDS     <- 3
TARGET     <- "Numeric_sex"
OUT_DIR    <- "../results_PC1_to_PC5"

ALPHAS <- c(
  "L1"         = 1,
  "L2"         = 0,
  "ElasticNet" = 0.5)

FORMULA_BASE <- ~ Age_sc + LngtClassGrouped_sc + Age_x_Lngt_sc +
                  Cohorte_num_sc + Area + Cohorte_fact - 1

# ================================================================
# PREPARATION DES DONNEES
# ================================================================
pc_keep     <- 1:5
var_keep    <- c("T_mean")
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

# -- Groupes PC --
PC_GROUPS <- lapply(pc_keep, function(k) {
  cols <- grep(paste0("_PC", k, "_"), colnames(data_model_temp), value = TRUE)
  cols[grepl(var_pattern, cols)]
})
names(PC_GROUPS) <- paste0("PC", pc_keep)

cat("Groupes PC :\n")
for (nm in names(PC_GROUPS))
  cat(" ", nm, "→", length(PC_GROUPS[[nm]]), "colonnes\n")

# -- Combinaisons --
n_grp     <- length(PC_GROUPS)
PC_COMBOS <- lapply(seq_len(2^n_grp - 1), function(i) {
  sel <- as.logical(intToBits(i)[1:n_grp])
  unlist(PC_GROUPS[sel], use.names = FALSE)
})
PC_LABELS <- sapply(seq_len(2^n_grp - 1), function(i) {
  sel <- as.logical(intToBits(i)[1:n_grp])
  paste(names(PC_GROUPS)[sel], collapse = "+")
})
N_COMBOS <- length(PC_COMBOS)
cat("\nNombre de combinaisons :", N_COMBOS, "\n")

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
      x = X_train, y = y_train,
      family = "binomial", alpha = alpha,
      nfolds = nfolds, type.measure = "deviance"
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

    # -- Combinaisons PC --
    for (i in seq_along(PC_COMBOS)) {
      pc_sel <- PC_COMBOS[[i]]
      label  <- PC_LABELS[[i]]

      # NAs check
      na_check <- colSums(is.na(spl_full$train[, pc_sel, drop = FALSE]))
      if (any(na_check > 0)) {
        cat("    [", sprintf("%2d", i), "/", N_COMBOS, "]",
            label, "— NAs détectés, ignoré\n")
        next
      }

      X_train_temp <- cbind(X_full_train,
                            as.matrix(spl_full$train[, pc_sel, drop = FALSE]))
      X_test_temp  <- cbind(X_full_test,
                            as.matrix(spl_full$test[,  pc_sel, drop = FALSE]))

      t0 <- proc.time()["elapsed"]
      cat("    [", sprintf("%2d", i), "/", N_COMBOS, "]",
          label, "cv.glmnet... ")

      auc_temp <- .fit_auc(X_train_temp, y_full_train,
                            X_test_temp,  y_full_test,
                            alpha_val, NFOLDS)

      delta <- auc_temp - auc_base
      cat("AUC =", round(auc_temp, 4),
          "| Δ =", sprintf("%+.4f", delta),
          "| durée :", round(proc.time()["elapsed"] - t0, 1), "sec\n")

      sp_results <- rbind(sp_results, data.frame(
        Species    = sp,
        alpha_name = alpha_name,
        alpha_val  = alpha_val,
        combo_id   = i,
        pc_label   = label,
        auc_base   = auc_base,
        auc_temp   = auc_temp,
        delta_auc  = delta,
        stringsAsFactors = FALSE
      ))
    }

    # ── Plot pour cette pénalisation ──
    df_plot <- sp_results %>%
      filter(alpha_name == !!alpha_name) %>%
      mutate(
        couleur   = ifelse(delta_auc >= 0, "Gain", "Perte"),
        label_bar = sprintf("%+.4f", delta_auc)
      )

    if (nrow(df_plot) > 0) {
      best_row  <- df_plot %>% slice_max(abs(delta_auc), n = 1, with_ties = FALSE)
      worst_row <- df_plot %>% slice_min(abs(delta_auc), n = 1, with_ties = FALSE)

      p <- ggplot(df_plot,
                  aes(x = factor(combo_id), y = delta_auc, fill = couleur)) +
        geom_col(alpha = 0.85, width = 0.75) +
        geom_text(aes(label = label_bar,
                      vjust = ifelse(delta_auc >= 0, -0.3, 1.2)),
                  size = 2.5) +
        geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
        scale_fill_manual(
          values = c("Gain" = "#185FA5", "Perte" = "#E24B4A"),
          name   = "Effet temp."
        ) +
        scale_x_discrete(
          labels = setNames(
            paste0(df_plot$combo_id, "\n(", df_plot$pc_label, ")"),
            as.character(df_plot$combo_id)
          )
        ) +
        labs(
          title    = paste0("Δ AUC-ROC (temp − base) | ", sp, " | ", alpha_name),
          subtitle = paste0("AUC baseline = ", round(unique(df_plot$auc_base), 4),
                            " | Meilleur : #", best_row$combo_id,
                            " ", best_row$pc_label,
                            " (Δ=", sprintf("%+.4f", best_row$delta_auc), ")"),
          x       = "Combinaison PC",
          y       = "Δ AUC-ROC",
          caption = paste0("Pénalisation : ", alpha_name,
                           " (alpha=", alpha_val, ")")
        ) +
        theme_bw() +
        theme(axis.text.x    = element_text(size = 7),
              legend.position = "bottom",
              plot.title      = element_text(face = "bold"))

      plot_file <- file.path(sp_dir,
                             paste0("delta_auc_", alpha_name,
                                    "_", sp_safe, ".png"))
      ggsave(plot_file, plot = p, width = 16, height = 6, dpi = 150)
      cat("  → Plot sauvegardé :", basename(plot_file), "\n")
    }
  }

  # ── Summary txt ──
  best_global <- sp_results %>%
    slice_max(abs(delta_auc), n = 1, with_ties = FALSE)

  summary_lines <- c(
    "═══════════════════════════════════════════",
    paste0("  RÉSUMÉ — Espèce : ", sp),
    "═══════════════════════════════════════════",
    ""
  )

  for (alpha_name in names(ALPHAS)) {
    df_a <- sp_results %>% filter(alpha_name == !!alpha_name)
    if (nrow(df_a) == 0) next
    br <- df_a %>% slice_max(abs(delta_auc), n = 1, with_ties = FALSE)
    wr <- df_a %>% slice_min(abs(delta_auc), n = 1, with_ties = FALSE)
    summary_lines <- c(summary_lines,
      paste0("── ", alpha_name, " (alpha=", ALPHAS[[alpha_name]], ") ──"),
      paste0("   AUC baseline    : ", round(unique(df_a$auc_base), 4)),
      paste0("   Meilleur modèle : #", br$combo_id, " — ", br$pc_label,
             " | AUC=", round(br$auc_temp, 4),
             " | Δ=", sprintf("%+.4f", br$delta_auc)),
      paste0("   Pire modèle     : #", wr$combo_id, " — ", wr$pc_label,
             " | AUC=", round(wr$auc_temp, 4),
             " | Δ=", sprintf("%+.4f", wr$delta_auc)),
      paste0("   N gains (Δ>0)   : ",
             sum(df_a$delta_auc > 0), "/", nrow(df_a)),
      ""
    )
  }

  summary_lines <- c(summary_lines,
    "── Meilleur GLOBAL ──",
    paste0("   Pénalisation : ", best_global$alpha_name),
    paste0("   Combinaison  : #", best_global$combo_id,
           " — ", best_global$pc_label),
    paste0("   Δ AUC        : ", sprintf("%+.4f", best_global$delta_auc)),
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
best_per_species <- results_all %>%
  group_by(Species) %>%
  slice_max(delta_auc, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(desc(delta_auc))

cat("\n\n══════════════════════════════════════════════════════\n")
cat("  MEILLEURS MODELES PAR ESPECE\n")
cat("══════════════════════════════════════════════════════\n")
print(best_per_species, n = Inf)

write.csv(results_all,
          file.path(OUT_DIR, "results_all_combinations.csv"),
          row.names = FALSE)
write.csv(best_per_species,
          file.path(OUT_DIR, "best_model_per_species.csv"),
          row.names = FALSE)

cat("\n✓ Pipeline terminé. Résultats dans :", OUT_DIR, "\n")
