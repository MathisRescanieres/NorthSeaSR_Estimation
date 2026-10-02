library(dplyr)

insert_thousand_sep <- function(x, digits = 2) {
  sapply(x, function(val) {
    x_rounded <- round(val, digits)
    x_str <- sprintf(paste0("%.", digits, "f"), x_rounded)
    
    parts <- strsplit(x_str, "\\.")[[1]]
    int_part <- parts[1]
    dec_part <- parts[2]
    
    int_part_rev <- paste(rev(strsplit(int_part, "")[[1]]), collapse = "")
    grouped <- gsub("(\\d{3})(?=\\d)", "\\1,", int_part_rev, perl = TRUE)
    grouped_rev <- paste(rev(strsplit(grouped, "")[[1]]), collapse = "")
    grouped_final <- gsub(",", "\\\\,", grouped_rev)
    
    paste0(grouped_final, ".", dec_part)
  })
}

# ---- Ordre décroissant d'effectifs, déjà établi dans le reste du rapport ----
ordre_especes <- c("Clupea harengus", "Merlangius merlangus", "Melanogrammus aeglefinus",
                    "Gadus morhua", "Pleuronectes platessa", "Sprattus sprattus",
                    "Trisopterus esmarkii", "Pollachius virens", "Scomber scombrus")

tab_export <- tab_all_combined %>%
  dplyr::select(species, method, shape, k, AIC, delta_AIC_vs_null, AIC_null) %>%
  dplyr::mutate(species = factor(species, levels = ordre_especes)) %>%
  dplyr::arrange(species, dplyr::desc(delta_AIC_vs_null)) %>%
  dplyr::mutate(
    species      = as.character(species),
    AIC_fmt      = insert_thousand_sep(AIC, digits = 2),
    AIC_null_fmt = insert_thousand_sep(AIC_null, digits = 2),
    delta_fmt    = sprintf("%+.2f", delta_AIC_vs_null)
  )

# ---- Repérage des lignes de changement d'espèce (pour les \hline) ----
tab_export$new_species <- c(TRUE, tab_export$species[-1] != tab_export$species[-nrow(tab_export)])

# ---- Construction manuelle du corps du tableau ----
lignes <- character(nrow(tab_export))

for (i in seq_len(nrow(tab_export))) {
  r <- tab_export[i, ]
  
  ligne <- sprintf("\\textit{%s} & %s & %s & %d & %s & %s & %s \\\\",
                    r$species, r$method, r$shape, r$k, r$AIC_null_fmt, r$AIC_fmt, r$delta_fmt)
  if (r$new_species && i > 1) {
    ligne <- paste0("\\hline\n", ligne)
  }
  lignes[i] <- ligne
}

corps_tableau <- paste(lignes, collapse = "\n")

# ---- En-tête du longtable ----
entete <- "\\begin{longtable}{|l|l|l|r|r|r|r|}
\\hline
\\textbf{Espèce} & \\textbf{Méthode} & \\textbf{Forme} & \\textbf{Nb. param.} & \\textbf{AIC réf.} & \\textbf{AIC thermique} & \\textbf{$\\Delta$AIC} \\\\
\\hline
\\endfirsthead
\\hline
\\textbf{Espèce} & \\textbf{Méthode} & \\textbf{Forme} & \\textbf{Nb. param.} & \\textbf{AIC réf.} & \\textbf{AIC thermique} & \\textbf{$\\Delta$AIC} \\\\
\\hline
\\endhead
\\hline
\\endfoot
\\hline
\\endlastfoot
"

pied <- "\\end{longtable}"

tex_complet <- paste(entete, corps_tableau, pied, sep = "\n")

writeLines(tex_complet,
           "/home/mathis/NorthSeaSR_Estimation/SR_Estimation/rapport/rapport_final/annexe/tab_aic_thermique.tex")

cat("Fichier écrit.\n")
