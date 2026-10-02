# ================================================================
# Selection backward par LRT, 4 especes avec effet thermique
# detecte, methode detrend uniquement.
# Chaque fit tourne dans un sous-process R isole (run_one_fit_backward.R),
# qui reutilise build_parameters() et make_f_full() de test_temp_v4.R.
# Pas de sdreport pendant la selection, uniquement a la fin sur le
# modele final retenu.
# ================================================================

library(rprojroot)

proj_root <- find_root(has_file(".git") | is_rstudio_project)
setwd(file.path(proj_root, "SR_Estimation", "stat_analysis_2026", "data"))

BASE_OUT <- "/home/mathis/NorthSeaSR_Estimation/SR_Estimation/stat_analysis_2026/data/results_rtmb/backward_selection_LRT"

especes_cibles <- c("Clupea harengus", "Melanogrammus aeglefinus",
                    "Merlangius merlangus", "Trisopterus esmarkii")
method <- "detrend"
seuil_p <- 0.05

termes_thermiques_detrend <- c("beta_trend", "beta_sd_abs", "beta_anom_mean", "beta_anom_sd")

# ----------------------------------------------------------------
# Lance un fit dans un sous-process, puis relit le resultat depuis
# le .rds sauvegarde par run_one_fit_backward.R (cache par signature)
# ----------------------------------------------------------------
fit_model_reduit_subprocess <- function(sp, method, termes_a_fixer, out_dir_base) {

  termes_csv <- if (length(termes_a_fixer) == 0) "VIDE" else paste(termes_a_fixer, collapse = ",")

  signature <- if (length(termes_a_fixer) == 0) "modele_complet" else
    paste0("sans_", paste(sort(termes_a_fixer), collapse = "_"))
  out_dir <- file.path(out_dir_base, method, sp)
  out_file <- file.path(out_dir, paste0(signature, ".rds"))

  if (file.exists(out_file)) {
    cat("    [cache] ", signature, "\n")
    return(readRDS(out_file))
  }

  status <- system2("Rscript",
                     args = c("../scripts/run_one_fit_backward.R", shQuote(sp), shQuote(method),
                               shQuote(termes_csv), shQuote(out_dir_base)),
                     stdout = "", stderr = "")

  if (status != 0 || !file.exists(out_file)) {
    stop(paste("Echec du sous-process pour", signature, "( espece", sp, ")"))
  }

  readRDS(out_file)
}

# ----------------------------------------------------------------
# Selection backward par LRT pour une espece
# ----------------------------------------------------------------
selection_backward_LRT_espece <- function(sp, method, termes_thermiques, out_dir_base, seuil_p) {

  cat("\n\n########## SELECTION BACKWARD LRT :", sp, "-", method, "##########\n")

  out_dir <- file.path(out_dir_base, method, sp)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  termes_actifs <- termes_thermiques
  termes_fixes  <- character(0)

  modele_courant <- fit_model_reduit_subprocess(sp, method, termes_fixes, out_dir_base)
  cat("Modele complet : objective =", round(modele_courant$objective, 2),
      "| AIC =", round(modele_courant$AIC, 2), "\n")

  historique <- list()
  etape <- 1

  repeat {

    if (length(termes_actifs) == 0) {
      cat("\nPlus aucun terme actif, arret.\n")
      break
    }

    cat("\n--- Etape", etape, ": termes actifs =", paste(termes_actifs, collapse = ", "), "---\n")

    resultats_candidats <- list()
    p_values <- setNames(numeric(length(termes_actifs)), termes_actifs)

    for (terme in termes_actifs) {
      termes_fixes_test <- c(termes_fixes, terme)
      r_test <- fit_model_reduit_subprocess(sp, method, termes_fixes_test, out_dir_base)
      resultats_candidats[[terme]] <- r_test

      stat_lrt <- 2 * (r_test$objective - modele_courant$objective)
      p_values[terme] <- pchisq(stat_lrt, df = 1, lower.tail = FALSE)

      cat("  Retrait de", terme, ": LRT =", round(stat_lrt, 3),
          "| p =", round(p_values[terme], 4), "\n")
    }

    terme_a_retirer <- names(which.max(p_values))
    p_max <- max(p_values)

    historique[[etape]] <- data.frame(
      espece = sp, etape = etape, terme_teste = terme_a_retirer, p_LRT = p_max
    )

    if (p_max > seuil_p) {
      cat(">>> Retrait de", terme_a_retirer, "(p =", round(p_max, 4), ">", seuil_p, ")\n")
      termes_fixes  <- c(termes_fixes, terme_a_retirer)
      termes_actifs <- setdiff(termes_actifs, terme_a_retirer)
      modele_courant <- resultats_candidats[[terme_a_retirer]]
      etape <- etape + 1
    } else {
      cat(">>> Tous les termes restants sont significatifs (p min =", round(p_max, 4), "), arret.\n")
      break
    }
  }

  resultat_final <- list(
    species = sp, method = method,
    modele_final = modele_courant,
    termes_retenus = termes_actifs,
    termes_retires = termes_fixes,
    historique = if (length(historique) > 0) do.call(rbind, historique) else NULL
  )

  saveRDS(resultat_final, file.path(out_dir, "selection_finale_LRT.rds"))

  cat("\n=== RESULTAT FINAL", sp, "-", method, "===\n")
  cat("Termes retenus :", paste(termes_actifs, collapse = ", "), "\n")
  cat("Termes retires :", paste(termes_fixes, collapse = ", "), "\n")

  resultat_final
}

# ----------------------------------------------------------------
# Boucle sur les 4 especes
# ----------------------------------------------------------------

resultats_selection <- list()

for (sp in especes_cibles) {

  out_dir_sp <- file.path(BASE_OUT, method, sp)
  fichier_final_sp <- file.path(out_dir_sp, "selection_finale_LRT.rds")

  if (file.exists(fichier_final_sp)) {
    cat("\n>>> Deja termine, on saute :", sp, "\n")
    resultats_selection[[sp]] <- readRDS(fichier_final_sp)
    next
  }

  resultats_selection[[sp]] <- selection_backward_LRT_espece(
    sp = sp, method = method,
    termes_thermiques = termes_thermiques_detrend,
    out_dir_base = BASE_OUT, seuil_p = seuil_p
  )
}

saveRDS(resultats_selection, file.path(BASE_OUT, "selection_backward_LRT_4especes_detrend.rds"))

cat("\n\n########## SYNTHESE GLOBALE ##########\n")
for (sp in names(resultats_selection)) {
  r <- resultats_selection[[sp]]
  cat(sp, ": retenus =", paste(r$termes_retenus, collapse = ", "),
      "| retires =", paste(r$termes_retires, collapse = ", "), "\n")
}
