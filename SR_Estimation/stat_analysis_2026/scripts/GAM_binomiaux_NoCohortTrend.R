# =============================================================================
#  GAM binomiaux finaux — MODELE AVEC fs
#  Interaction age x taille variable par cohorte (bs="fs"), regularisee
#  Modeles espece-dependants : k et termes issus du diagnostic k.check
# =============================================================================

library(mgcv)
library(dplyr)

# =============================================================================
#  Surcharges espece-dependantes des hyperparametres k
# =============================================================================

k_age_override <- list(
  "Melanogrammus aeglefinus" = 13
)

k_time_override <- list(
  "Merlangius merlangus"  = 24,
  "Trisopterus esmarkii"  = 24,
  "Pleuronectes platessa" = 24,
  "Sprattus sprattus"     = 24
)

k_space_override <- list(
  "Pleuronectes platessa" = 240
)

# k_fs : valeur par defaut, plafonnee dynamiquement par le nombre de cohortes
# disponibles pour chaque espece (n_cohort - 1)
k_fs_default <- 6

# =============================================================================
#  Termes retires par espece
# =============================================================================

terms_removed <- list(
  "Pollachius virens" = c("f4", "bc")   # s(julian_day), s(Cohort_fact)
)

# =============================================================================
#  Boucle d'ajustement
# =============================================================================

species_list      <- unique(data_expanded_1991_2023$Species)
gam_models_final  <- list()

for (sp in species_list) {

  cat("Fit du modele final pour :", sp, "...\n")

  data_sp <- data_expanded_1991_2023 %>%
    dplyr::filter(Species == sp) %>%
    droplevels()

  n_age    <- n_distinct(data_sp$Age_sc)
  n_lngt   <- n_distinct(data_sp$LngtClassGrouped_sc)
  n_cohort <- n_distinct(data_sp$Cohort_fact)

  k_age   <- min(15, n_age  - 1)
  k_lngt  <- min(20, n_lngt - 1)
  k_space <- 120
  k_time  <- 12
  k_fs    <- min(k_fs_default, n_cohort - 1)

  if (sp %in% names(k_age_override))   k_age   <- k_age_override[[sp]]
  if (sp %in% names(k_space_override)) k_space <- k_space_override[[sp]]
  if (sp %in% names(k_time_override))  k_time  <- k_time_override[[sp]]

  removed <- terms_removed[[sp]]

  cat("  k_age =", k_age, "| k_lngt =", k_lngt,
      "| k_space =", k_space, "| k_time =", k_time,
      "| k_fs =", k_fs, "| n_cohort =", n_cohort, "\n")
  if (!is.null(removed))
    cat("  Termes retires :", paste(removed, collapse = ", "), "\n")

  terms <- list()

  terms[["te"]] <- bquote(
    te(Age_sc, LngtClassGrouped_sc, k = c(.(k_age), .(k_lngt)), bs = c("cr", "cr"))
  )

  terms[["fs"]] <- bquote(
    s(Age_sc, LngtClassGrouped_sc, Cohort_fact, bs = "fs", k = .(k_fs), m = 2)
  )

  terms[["f3_space"]] <- bquote(s(Latitude, Longitude, k = .(k_space), bs = "sos"))

  if (!"f4" %in% removed)
    terms[["f4"]] <- bquote(s(julian_day, bs = "cc", k = .(k_time)))

  if (!"bc" %in% removed)
    terms[["bc"]] <- quote(s(Cohort_fact, bs = "re"))

  rhs        <- Reduce(function(a, b) call("+", a, b), terms)
  formula_sp <- as.formula(bquote(Numeric_sex ~ .(rhs)))

  mod <- tryCatch(
    bam(
      formula_sp,
      family   = binomial(link = "logit"),
      data     = data_sp,
      method   = "ML",
      discrete = FALSE,
      keepData = TRUE
    ),
    error = function(e) { cat("  ECHEC ajustement :", conditionMessage(e), "\n"); NULL }
  )

  if (is.null(mod)) {
    rm(data_sp)
    next
  }

  gam_models_final[[sp]] <- mod

  cat("  Done\n\n")

  rm(data_sp, mod)
  gc(verbose = FALSE)
}

especes_manquantes <- setdiff(species_list, names(gam_models_final))
if (length(especes_manquantes) > 0) {
  cat("\nATTENTION : especes absentes du modele final (echec d'ajustement) :\n")
  print(especes_manquantes)
} else {
  cat("\nToutes les especes ont ete ajustees avec succes (", length(gam_models_final), "/", length(species_list), ").\n")
}

saveRDS(gam_models_final, "../scripts/gam_models_final_fs.rds")