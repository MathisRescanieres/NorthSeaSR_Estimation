# =============================================================================
#  GAM binomiaux finaux — MODELE 2/3 (sans Cohort_num_sc)
#  Modeles espece-dependants : k et termes issus du diagnostic k.check
#  (formule reduite : effet aleatoire cohorte seul, sans tendance continue)
#  Reference methodologique : Wood (2025), mgcv 1.9-4
# =============================================================================

library(mgcv)
library(dplyr)

# =============================================================================
#  Surcharges espece-dependantes des hyperparametres k
#  (issues du diagnostic k.check sur la formule reduite, sans Cohort_num_sc)
# =============================================================================

k_time_override <- list(
  "Merlangius merlangus"  = 24,
  "Trisopterus esmarkii"  = 24,
  "Pleuronectes platessa" = 24,
  "Sprattus sprattus"     = 24
)

k_space_override <- list(
  "Pleuronectes platessa" = 240
)

# aucun k_cohort_override : s(Cohort_num_sc) n'existe plus dans ce modele

# =============================================================================
#  Termes retires par espece
# =============================================================================

terms_removed <- list(
  "Pollachius virens" = c("f4", "bc")   # s(julian_day), s(Cohort_fact)
)

# =============================================================================
#  Especes exclues de l'analyse H3 (pipeline thermique)
#  Pas de variance inter-cohorte detectable (s(Cohort_fact) retire) :
#  rien a expliquer par la temperature pour ces especes.
# =============================================================================

species_excluded_H3 <- c("Pollachius virens")

# =============================================================================
#  Boucle d'ajustement
# =============================================================================

species_list     <- unique(data_expanded_1991_2023$Species)
gam_models_final_m2 <- list()

for (sp in species_list) {

  cat("Fitting final model (modele 2/3, sans Cohort_num_sc) for :", sp, "...\n")

  data_sp <- data_expanded_1991_2023 %>%
    dplyr::filter(Species == sp) %>%
    droplevels()

  # ---- k dynamiques (base) ---------------------------
  n_age  <- n_distinct(data_sp$Age_sc)
  n_lngt <- n_distinct(data_sp$LngtClassGrouped_sc)

  k_age   <- min(15, n_age  - 1)
  k_lngt  <- min(20, n_lngt - 1)
  k_space <- 120
  k_time  <- 12

  # ---- Surcharges espece-dependantes ----------------------
  if (sp %in% names(k_space_override)) k_space <- k_space_override[[sp]]
  if (sp %in% names(k_time_override))  k_time  <- k_time_override[[sp]]

  removed <- terms_removed[[sp]]  # NULL si espece absente de la liste

  cat("  k_age =", k_age, "| k_lngt =", k_lngt,
      "| k_space =", k_space, "| k_time =", k_time, "\n")
  if (!is.null(removed))
    cat("  Termes retires :", paste(removed, collapse = ", "), "\n")
  if (sp %in% species_excluded_H3)
    cat("  ATTENTION :", sp, "exclue du pipeline thermique (modele 3) : pas de variance cohorte.\n")

  # ---- Construction de la formule terme par terme ---------------
  terms <- list()

  terms[["te"]] <- bquote(
    te(Age_sc, LngtClassGrouped_sc, k = c(.(k_age), .(k_lngt)), bs = c("cr", "cr"))
  )

  # s(Lat, Lon) toujours present
  terms[["f3_space"]] <- bquote(s(Latitude, Longitude, k = .(k_space), bs = "sos"))

  if (!"f4" %in% removed)
    terms[["f4"]] <- bquote(s(julian_day, bs = "cc", k = .(k_time)))

  if (!"bc" %in% removed)
    terms[["bc"]] <- quote(s(Cohort_fact, bs = "re"))

  rhs        <- Reduce(function(a, b) call("+", a, b), terms)
  formula_sp <- as.formula(bquote(Numeric_sex ~ .(rhs)))

  # ---- Ajustement ------------------------------
  gam_models_final_m2[[sp]] <- bam(
    formula_sp,
    family   = binomial(link = "logit"),
    data     = data_sp,
    method   = "ML",
    discrete = FALSE,
    keepData = TRUE
  )

  cat("  Done\n\n")

  # ---- Sauvegarde ------------------------------
  saveRDS(gam_models_final_m2, "../scripts/gam_models_final_m2.rds")
}
