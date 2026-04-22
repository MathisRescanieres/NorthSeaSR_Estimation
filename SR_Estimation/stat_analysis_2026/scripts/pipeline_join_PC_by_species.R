# ==============================================================================
# pipeline_join_PC_by_species.R
# ==============================================================================

run_species_prejoin <- function(sp,
                                depths,
                                r2_thresh,
                                data_expanded,
                                data_eof_flatten,
                                dir_r2,
                                dir_out = "../data/species_prejoined_trended") {

  cat("Processing:", sp, "\n")

  

  # Variance table
  r2_tables <- map(
  setNames(1:12, sprintf("month_%02d", 1:12)),
  ~ read.csv(file.path(dir_r2, paste0("r2_", .x, ".csv"))))

  # Fonction : colonnes EOF à garder pour une espèce
    
  .get_cols_to_keep <- function(r2_tables, depths, r2_thresh) {

    cols_keep <- c()

    for (m in 1:12) {

      month_key <- sprintf("month_%02d", m)
      r2_tab    <- r2_tables[[month_key]] %>% filter(depth %in% depths)

      # Pour chaque profondeur, trouver k = dernier PC avec r2 >= seuil
      for (i in seq_len(nrow(r2_tab))) {

        d      <- r2_tab$depth[i]
        r2_row <- r2_tab[i, grepl("^PC_", names(r2_tab)), drop = FALSE]
        r2_vec <- unlist(r2_row)
        r2_vec <- r2_vec[!is.na(r2_vec)]

        # k = dernier indice où r2 >= seuil
        above  <- which(r2_vec >= r2_thresh)
        if (length(above) == 0) next
        k      <- max(above)

        pc_names  <- names(r2_vec)[1:k]           
        d_suffix  <- paste0("d", d)
        month_lbl <- .month_window$month_label[.month_window$month_num == m]

        # Reconstitue les noms de colonnes dans data_eof_flatten
        new_cols <- outer(
          paste0("T_", pc_names),
          paste0("_", month_lbl, "_", d_suffix),
          paste0
        ) %>% as.vector()

        cols_keep <- union(cols_keep, new_cols)
      }
    }

    cols_keep
  }

  # Colonnes EOF pertinentes
  cols_keep <- .get_cols_to_keep(r2_tables, depths, r2_thresh)
  cat("  Colonnes EOF retenues :", length(cols_keep), "\n")

  if (length(cols_keep) == 0) {
    warning("Aucune colonne retenue pour ", sp, " — vérifier depths et r2_thresh")
    return(invisible(NULL))
  }

  # Sous-ensemble EOF
  eof_sp <- data_eof_flatten %>%
    select(year, any_of(cols_keep))

  # Données espèce 
  data_sp <- data_expanded %>%
    filter(Species == sp) %>%
    mutate(Age_x_Lngt_sc = as.numeric(scale(Age_sc * LngtClassGrouped_sc)))

  if (nrow(data_sp) == 0) {
    warning("Aucun individu trouvé pour ", sp)
    return(invisible(NULL))
  }

  cat("  Individus :", nrow(data_sp), "\n")

  # Jointure
  data_sp_joined <- data_sp %>%
    left_join(eof_sp, by = c("Cohorte_num" = "year"))

  # Sauvegarde
  dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)
  out_path <- file.path(dir_out, paste0(gsub(" ", "_", sp), ".rds"))
  saveRDS(data_sp_joined, file = out_path)
  cat("  Sauvegardé :", out_path, "\n")

  rm(data_sp, data_sp_joined, eof_sp)
  gc()

  invisible(out_path)
}