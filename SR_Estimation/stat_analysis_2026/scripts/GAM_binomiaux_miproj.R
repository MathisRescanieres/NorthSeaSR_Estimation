# =============================================================================
#  GAM binomiaux finaux — sex-ratio secondaire, mer du Nord (NS-IBTS)
#  Modèles espèce-dépendants après diagnostic k.check + DHARMa (2 passes)
#  Référence méthodologique : Wood (2025), mgcv 1.9-4
# =============================================================================

library(mgcv)
library(dplyr)

# =============================================================================
#  Surcharges espèce-dépendantes des hyperparamètres k
# =============================================================================

k_lngt_override <- list(
  "Melanogrammus aeglefinus" = 30,  # k-index te() = 0.976, p = 0.008 → résolu passe 1
  "Merlangius merlangus"     = 25,  # k-index te() = 0.981, p = 0.053 → résolu passe 1
  "Gadus morhua"             = 25   # k-index te() = 0.979, p = 0.013 → apparu passe 1
)

k_space_override <- list(
  "Pleuronectes platessa" = 200,    # k-index s(Lat,Lon) = 0.959, p < 0.001 (persistant après 150)
  "Trisopterus esmarkii"  = 150     # k-index s(Lat,Lon) = 0.973, p = 0.030 (apparu passe 1)
)

k_depth_override <- list(
  "Pleuronectes platessa" = 15      # k-index s(Depth) = 0.966, p = 0.013
)

k_cohort_override <- list(
  "Pleuronectes platessa" = 12      # edf(f2) = 8.5 / k' = 9 → proche saturation
)

# =============================================================================
#  Termes retirés par espèce (edf ≈ 0, p > 0.05 en passe 1)
#  Codes : f2 = s(Cohort_num_sc) ; f3_depth = s(Depth) ;
#          f4 = s(julian_day) ; bc = s(Cohort_fact) ; ur = s(Area_fact)
# =============================================================================

terms_removed <- list(
  "Gadus morhua"         = c("f2"),
  "Trisopterus esmarkii" = c("f2", "ur"),
  "Pollachius virens"    = c("f2", "f3_depth", "f4", "bc", "ur")
)

# =============================================================================
#  Boucle d'ajustement
# =============================================================================

species_list     <- unique(data_expanded$Species)
gam_models_final <- list()

for (sp in species_list) {

  cat("Fitting final model for :", sp, "...\n")

  data_sp <- data_expanded %>%
    dplyr::filter(Species == sp) %>%
    droplevels()

  # ── k dynamiques (identiques au script de diagnostic initial) ──────────────
  n_age  <- n_distinct(data_sp$Age_sc)
  n_lngt <- n_distinct(data_sp$LngtClassGrouped_sc)
  n_coh  <- n_distinct(data_sp$Cohort_num_sc)

  k_age    <- min(15, n_age  - 1)
  k_lngt   <- min(20, n_lngt - 1)
  k_cohort <- min(10, n_coh  - 1)
  k_space  <- 120
  k_depth  <- 10
  k_time   <- 12

  # ── Surcharges espèce-dépendantes ──────────────────────────────────────────
  if (sp %in% names(k_lngt_override))   k_lngt   <- k_lngt_override[[sp]]
  if (sp %in% names(k_space_override))  k_space  <- k_space_override[[sp]]
  if (sp %in% names(k_depth_override))  k_depth  <- k_depth_override[[sp]]
  if (sp %in% names(k_cohort_override)) k_cohort <- k_cohort_override[[sp]]

  removed <- terms_removed[[sp]]  # NULL si espèce absente de la liste

  cat("  k_age =", k_age, "| k_lngt =", k_lngt, "| k_cohort =", k_cohort,
      "| k_space =", k_space, "| k_depth =", k_depth, "| k_time =", k_time, "\n")
  if (!is.null(removed))
    cat("  Termes retirés :", paste(removed, collapse = ", "), "\n")

  # ── Construction de la formule terme par terme ─────────────────────────────
  terms <- list()

  terms[["te"]] <- bquote(
    te(Age_sc, LngtClassGrouped_sc, k = c(.(k_age), .(k_lngt)), bs = c("cr", "cr"))
  )

  if (!"f2" %in% removed)
    terms[["f2"]] <- bquote(s(Cohort_num_sc, k = .(k_cohort), bs = "ts"))

  # s(Lat, Lon) toujours présent
  terms[["f3_space"]] <- bquote(s(Latitude, Longitude, k = .(k_space), bs = "sos"))

  if (!"f3_depth" %in% removed)
    terms[["f3_depth"]] <- bquote(s(Depth, k = .(k_depth), bs = "ts"))

  if (!"f4" %in% removed)
    terms[["f4"]] <- bquote(s(julian_day, bs = "cc", k = .(k_time)))

  if (!"bc" %in% removed)
    terms[["bc"]] <- quote(s(Cohort_fact, bs = "re"))

  if (!"ur" %in% removed)
    terms[["ur"]] <- quote(s(Area_fact, bs = "re"))

  rhs        <- Reduce(function(a, b) call("+", a, b), terms)
  formula_sp <- as.formula(bquote(Numeric_sex ~ .(rhs)))

  # ── Ajustement ─────────────────────────────────────────────────────────────
  gam_models_final[[sp]] <- bam(
    formula_sp,
    family   = binomial(link = "logit"),
    data     = data_sp,
    method   = "fREML",
    discrete = TRUE,
    keepData = TRUE
  )

  cat("  Done\n\n")

  # ── Sauvegarde ─────────────────────────────────────────────────────────────
  saveRDS(gam_models_final, "../scripts/gam_models_miproj.rds")
}