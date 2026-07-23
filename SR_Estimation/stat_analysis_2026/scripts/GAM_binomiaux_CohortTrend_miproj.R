# =============================================================================
#  GAM binomiaux finaux
#  Modeles espece-dependants : k et termes issus du diagnostic k.check
#  Reference methodologique : Wood (2025), mgcv 1.9-4
# =============================================================================

library(mgcv)
library(dplyr)

# =============================================================================
#  Surcharges espece-dependantes des hyperparametres k
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

k_cohort_override <- list(
  "Pleuronectes platessa" = 18    
)

# =============================================================================
#  Termes retires par espece
# =============================================================================

terms_removed <- list(
  "Gadus morhua"         = c("f2"),             
  "Trisopterus esmarkii" = c("f2"),                
  "Pollachius virens"    = c("f2", "f4", "bc")  
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

  # ---- k dynamiques (base) ---------------------------
  n_age  <- n_distinct(data_sp$Age_sc)
  n_lngt <- n_distinct(data_sp$LngtClassGrouped_sc)
  n_coh  <- n_distinct(data_sp$Cohort_num_sc)

  k_age    <- min(15, n_age  - 1)
  k_lngt   <- min(20, n_lngt - 1)
  k_cohort <- min(10, n_coh  - 1)
  k_space  <- 120
  k_time   <- 12

  # ---- Surcharges espece-dependantes ----------------------
  if (sp %in% names(k_space_override))  k_space  <- k_space_override[[sp]]
  if (sp %in% names(k_cohort_override)) k_cohort <- k_cohort_override[[sp]]
  if (sp %in% names(k_time_override))   k_time   <- k_time_override[[sp]]

  # garde-fou : ne pas depasser le nombre de niveaux distincts disponibles
  k_cohort <- min(k_cohort, n_coh - 1)

  removed <- terms_removed[[sp]]  # NULL si espece absente de la liste

  cat("  k_age =", k_age, "| k_lngt =", k_lngt, "| k_cohort =", k_cohort,
      "| k_space =", k_space, "| k_time =", k_time, "\n")
  if (!is.null(removed))
    cat("  Termes retires :", paste(removed, collapse = ", "), "\n")

  # ---- Construction de la formule terme par terme ---------------
  terms <- list()

  terms[["te"]] <- bquote(
    te(Age_sc, LngtClassGrouped_sc, k = c(.(k_age), .(k_lngt)), bs = c("cr", "cr"))
  )

  if (!"f2" %in% removed)
    terms[["f2"]] <- bquote(s(Cohort_num_sc, k = .(k_cohort), bs = "ts"))

  # s(Lat, Lon) toujours present
  terms[["f3_space"]] <- bquote(s(Latitude, Longitude, k = .(k_space), bs = "sos"))

  if (!"f4" %in% removed)
    terms[["f4"]] <- bquote(s(julian_day, bs = "cc", k = .(k_time)))

  if (!"bc" %in% removed)
    terms[["bc"]] <- quote(s(Cohort_fact, bs = "re"))

  rhs        <- Reduce(function(a, b) call("+", a, b), terms)
  formula_sp <- as.formula(bquote(Numeric_sex ~ .(rhs)))

  # ---- Ajustement ------------------------------
  gam_models_final[[sp]] <- bam(
    formula_sp,
    family   = binomial(link = "logit"),
    data     = data_sp,
    method   = "fREML",
    discrete = TRUE,
    keepData = TRUE
  )

  cat("  Done\n\n")

  # ---- Sauvegarde ------------------------------
  saveRDS(gam_models_final, "../scripts/gam_models_final.rds")
}