# ================================================================
# PIPELINE COMPLET v3
# ================================================================

library(dplyr)
library(mgcv)
library(lubridate)
library(RTMB)
library(Matrix)

stopifnot(exists("df_long"))
stopifnot(exists("data_expanded_1991_2023"))

# ----------------------------------------------------------------
# 0. Constantes partagées
# ----------------------------------------------------------------

MONTH_OFFSETS <- 0:11
N_MONTHS      <- length(MONTH_OFFSETS)

SPECIES_NAME <- "Merlangius merlangus"   # à changer pour chaque espèce traitée

# k_time par espèce
K_TIME_BY_SPECIES <- list(
  "Merlangius merlangus"  = 24,
  "Trisopterus esmarkii"  = 24,
  "Sprattus sprattus"     = 24,
  "Pleuronectes platessa" = 24
)                                     
k_time_default <- 12

# k_space par espèce
K_SPACE_BY_SPECIES <- list(
  "Pleuronectes platessa" = 240
)
k_space_default <- 120

# k_age par espèce
K_AGE_OVERRIDE <- list(
  "Melanogrammus aeglefinus" = 13
)

# Cache disque pour bam_ref et le modèle nul
# Mettre FORCE_REFIT_* à TRUE pour forcer un recalcul (ex. après avoir changé
# k_age/k_lngt/k_space/k_time, la formule, ou les données en entrée).
CACHE_DIR <- "cache"
if (!dir.exists(CACHE_DIR)) dir.create(CACHE_DIR, recursive = TRUE)
FORCE_REFIT_BAM_REF <- FALSE
FORCE_REFIT_NULL    <- FALSE

# ----------------------------------------------------------------
# 1. Série temporelle mensuelle agrégée
# ----------------------------------------------------------------

temp_surface_mensuelle <- df_long %>%
  dplyr::group_by(time) %>%
  dplyr::summarise(temp_moy_bassin = mean(temperature, na.rm = TRUE)) %>%
  dplyr::rename(date = time) %>%
  dplyr::arrange(date)

# ----------------------------------------------------------------
# 2. Anomalie thermique : detrending harmonique (tendance + K=5 harmoniques)
# ----------------------------------------------------------------

K_HARMONICS <- 5

temp_surface_mensuelle <- temp_surface_mensuelle %>%
  dplyr::mutate(
    mois           = as.numeric(format(date, "%m")),
    annee          = as.numeric(format(date, "%Y")),
    annee_continue = annee + (mois - 1) / 12
  )

harm_terms <- unlist(lapply(seq_len(K_HARMONICS), function(k) {
  c(sprintf("sin(2*pi*%d*annee_continue)", k),
    sprintf("cos(2*pi*%d*annee_continue)", k))
}))

formule_decomp <- as.formula(
  paste("temp_moy_bassin ~ annee_continue +", paste(harm_terms, collapse = " + "))
)

lm_decomp <- lm(formule_decomp, data = temp_surface_mensuelle)
print(summary(lm_decomp)$coefficients)

coefs <- coef(lm_decomp)

temp_surface_mensuelle <- temp_surface_mensuelle %>%
  dplyr::mutate(
    trend_component  = coefs["(Intercept)"] + coefs["annee_continue"] * annee_continue,
    temp_anomaly_harm = residuals(lm_decomp) 
  )

cat("R2 detrending harmonique (K =", K_HARMONICS, ") :", summary(lm_decomp)$r.squared, "\n")
cat("SD anomalie harmonique :", sd(temp_surface_mensuelle$temp_anomaly_harm), "\n")
cat("Corrélation anomalie harmonique vs temps :",
    cor(as.numeric(temp_surface_mensuelle$date), temp_surface_mensuelle$temp_anomaly_harm), "\n")

# ----------------------------------------------------------------
# 3. Données espèce
# ----------------------------------------------------------------

data_sp <- data_expanded_1991_2023 %>%
  dplyr::filter(Species == SPECIES_NAME, !is.na(Numeric_sex)) %>%
  droplevels()

cat("N =", nrow(data_sp), "\n")

k_age <- if (!is.null(K_AGE_OVERRIDE[[SPECIES_NAME]])) {
  K_AGE_OVERRIDE[[SPECIES_NAME]]
} else {
  min(15, n_distinct(data_sp$Age_sc) - 1)
}
k_lngt  <- min(20, n_distinct(data_sp$LngtClassGrouped_sc) - 1)
k_space <- if (!is.null(K_SPACE_BY_SPECIES[[SPECIES_NAME]])) {
  K_SPACE_BY_SPECIES[[SPECIES_NAME]]
} else {
  k_space_default
}
k_time  <- if (!is.null(K_TIME_BY_SPECIES[[SPECIES_NAME]])) {
  K_TIME_BY_SPECIES[[SPECIES_NAME]]
} else {
  k_time_default
}

# ----------------------------------------------------------------
# 4. Formule GAM construite une seule fois (version fs)
# ----------------------------------------------------------------

k_fs_default <- 6

k_fs <- min(k_fs_default, n_distinct(data_sp$Cohort_fact) - 1)

# Termes retires par espece (doit rester identique au pipeline mgcv)
terms_removed <- list(
  "Pollachius virens" = c("f4", "bc")
)
removed_sp <- terms_removed[[SPECIES_NAME]]

build_formula_sp <- function(k_age, k_lngt, k_space, k_time, k_fs, removed) {
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

  rhs <- Reduce(function(a, b) call("+", a, b), terms)
  as.formula(bquote(Numeric_sex ~ .(rhs)))
}

formula_sp <- build_formula_sp(k_age, k_lngt, k_space, k_time, k_fs, removed_sp)
print(formula_sp)

# ----------------------------------------------------------------
# 5. GAM de reference (bam) : chargement depuis le cache produit
# par le pipeline mgcv, ou ajustement si absent
# ----------------------------------------------------------------

# Signature GAM pure : ne depend PAS de MONTH_OFFSETS.
# Doit matcher exactement celle utilisee lors de la sauvegarde initiale
# des bam_ref depuis gam_models_final_fs.rds.
model_sig_gam <- paste(SPECIES_NAME, k_age, k_lngt, k_space, k_time, k_fs,
                        nrow(data_sp), sep = "_")
bam_ref_cache_file <- file.path(CACHE_DIR, paste0("bam_ref_", model_sig_gam, ".rds"))

if (!FORCE_REFIT_BAM_REF && file.exists(bam_ref_cache_file)) {
  cat("bam_ref charge depuis le cache :", bam_ref_cache_file, "\n")
  bam_ref <- readRDS(bam_ref_cache_file)
} else {
  bam_ref <- bam(
    formula_sp,
    family   = binomial(link = "logit"),
    data     = data_sp,
    method   = "ML",
    discrete = FALSE,
    keepData = TRUE
  )
  saveRDS(bam_ref, bam_ref_cache_file)
  cat("bam_ref sauvegarde dans :", bam_ref_cache_file, "\n")
}

summary(bam_ref)$s.table

lambda_bam_ref <- bam_ref$sp
print(lambda_bam_ref)

# ----------------------------------------------------------------
# 6. Extraction de la structure GAM (matrices des beta + penalites)
# ----------------------------------------------------------------

extract_gam_structure <- function(data_sp, formula_sp) {
  gam_setup <- mgcv::gam(
    formula_sp,
    family = binomial(link = "logit"),
    data   = data_sp,
    fit    = FALSE
  )

  smooth_info <- gam_setup$smooth

  # s(Cohort_fact, bs="re") est un effet aleatoire pur : retire de X_fixed,
  # pas de matrice de penalite classique a porter dans penalty_list.
  re_idx <- which(sapply(smooth_info, function(s) s$term[1] == "Cohort_fact" &&
                            inherits(s, "random.effect")))

  cols_re <- if (length(re_idx) > 0) {
    smooth_info[[re_idx]]$first.para:smooth_info[[re_idx]]$last.para
  } else {
    integer(0)
  }
  cols_fixed <- setdiff(seq_len(ncol(gam_setup$X)), cols_re)

  penalty_list <- list()
  for (i in seq_along(smooth_info)) {
    if (i %in% re_idx) next
    s_term <- smooth_info[[i]]
    cols_term <- s_term$first.para:s_term$last.para
    local_start <- 1 + (cols_term[1] - cols_fixed[1])
    for (j in seq_along(s_term$S)) {
      penalty_list[[length(penalty_list) + 1]] <- list(
        S = s_term$S[[j]],
        cols_local = local_start:(local_start + nrow(s_term$S[[j]]) - 1)
      )
    }
  }

  list(
    X_fixed      = gam_setup$X[, cols_fixed, drop = FALSE],
    penalty_list = penalty_list,
    n_penalty    = length(penalty_list),
    na_action    = gam_setup$na.action
  )
}

gam_struct_full <- extract_gam_structure(data_sp, formula_sp)

cat("Dim X_fixed :", paste(dim(gam_struct_full$X_fixed), collapse = " x "), "\n")
cat("Colonne 1 == intercept :", all(gam_struct_full$X_fixed[, 1] == 1), "\n")
cat("Nb blocs de penalite extraits :", gam_struct_full$n_penalty, "\n")

if (!is.null(gam_struct_full$na_action)) {
  cat("ATTENTION :", length(gam_struct_full$na_action),
      "lignes supprimees par na.omit - realignement de data_sp.\n")
  data_sp <- data_sp[-gam_struct_full$na_action, , drop = FALSE]
  data_sp$Cohort_fact <- droplevels(data_sp$Cohort_fact)
}

stopifnot(nrow(gam_struct_full$X_fixed) == nrow(data_sp))

cohort_levels     <- levels(data_sp$Cohort_fact)
cohort_id_per_obs <- as.integer(data_sp$Cohort_fact)
n_cohort          <- length(cohort_levels)

# ----------------------------------------------------------------
# 7. Retrait de l'intercept GAM redondant
# ----------------------------------------------------------------

X_fixed_no_intercept <- gam_struct_full$X_fixed[, -1, drop = FALSE]

penalty_list_adjusted <- lapply(gam_struct_full$penalty_list, function(p) {
  p$cols_local <- p$cols_local - 1
  p
})

cat("Dim X_fixed_no_intercept :", paste(dim(X_fixed_no_intercept), collapse = " x "), "\n")

# ----------------------------------------------------------------
# 8. Alignement des lambda REML/ML avec les blocs de penalty_list
# ----------------------------------------------------------------

# Ordre des blocs dans penalty_list, tel que pose par mgcv pour la
# formule "fs" standard :
#   1. te(Age_sc, LngtClassGrouped_sc) -> 2 blocs (une penalite par marginale)
#   2. s(Age_sc, LngtClassGrouped_sc, Cohort_fact, bs="fs") -> 1 bloc
#   3. s(Latitude, Longitude, bs="sos") -> 1 bloc
#   4. s(julian_day, bs="cc") -> 1 bloc, ABSENT si "f4" dans terms_removed
# s(Cohort_fact, bs="re") n'apparait jamais ici : effet aleatoire pur,
# deja retire via cols_re dans extract_gam_structure.

LAMBDA_NAMES_STANDARD <- c(
  "te(Age_sc,LngtClassGrouped_sc)1",
  "te(Age_sc,LngtClassGrouped_sc)2",
  "s(Age_sc,LngtClassGrouped_sc,Cohort_fact)",
  "s(Latitude,Longitude)",
  "s(julian_day)"
)

LAMBDA_NAMES_NO_F4 <- c(
  "te(Age_sc,LngtClassGrouped_sc)1",
  "te(Age_sc,LngtClassGrouped_sc)2",
  "s(Age_sc,LngtClassGrouped_sc,Cohort_fact)",
  "s(Latitude,Longitude)"
)

lambda_names_sp <- if (!is.null(removed_sp) && "f4" %in% removed_sp) {
  LAMBDA_NAMES_NO_F4
} else {
  LAMBDA_NAMES_STANDARD
}

# Verification manuelle recommandee avant de lancer la boucle complete :
# print(names(bam_ref$sp))  # doit correspondre terme a terme a lambda_names_sp

lambda_bam_ref_aligned <- as.numeric(lambda_bam_ref[lambda_names_sp])

stopifnot(!anyNA(lambda_bam_ref_aligned))
stopifnot(length(lambda_bam_ref_aligned) == length(penalty_list_adjusted))

cat("Lambda alignes (", SPECIES_NAME, ") :",
    paste(round(lambda_bam_ref_aligned, 3), collapse = ", "), "\n")

XtX <- t(X_fixed_no_intercept) %*% X_fixed_no_intercept
S_full <- matrix(0, ncol(X_fixed_no_intercept), ncol(X_fixed_no_intercept))
for (k in seq_along(penalty_list_adjusted)) {
  cols_k <- penalty_list_adjusted[[k]]$cols_local
  S_full[cols_k, cols_k] <- S_full[cols_k, cols_k] +
    lambda_bam_ref_aligned[k] * penalty_list_adjusted[[k]]$S
}
rank_penalized <- Matrix::rankMatrix(XtX + S_full)
cat("Rang de X'X + lambda*S :", rank_penalized[1], "sur", ncol(XtX), "\n")

# ----------------------------------------------------------------
# 9. Deux constructions concurrentes des covariables thermiques
#    Methode 1 (harm) : T_mean, T_sd, T_min, T_max (fixes, brutes)
#                        + T_index = fenetre ponderee sur l'ANOMALIE
#    Methode 2 (raw)  : T_sd, T_min, T_max (fixes, brutes)
#                        + T_index = fenetre ponderee sur la BRUTE
#                        (remplace T_mean : si w_month uniforme, T_index == T_mean)
# ----------------------------------------------------------------

build_temp_extended <- function(monthly_series, cohort_years, month_offsets) {
  n_cohort <- length(cohort_years)
  temp_extended <- matrix(NA_real_, nrow = n_cohort, ncol = length(month_offsets))

  for (i in seq_along(cohort_years)) {
    yr <- cohort_years[i]
    target_dates <- as.Date(paste0(yr, "-01-01")) %m+% months(month_offsets)
    idx_match <- match(target_dates, monthly_series$date)
    temp_extended[i, ] <- monthly_series$temp_moy_bassin[idx_match]
  }

  temp_extended
}

cohort_years_sp <- as.numeric(as.character(cohort_levels))

# --- Matrice brute (non detrendee) ---
temp_extended_raw <- build_temp_extended(
  monthly_series = temp_surface_mensuelle %>%
    dplyr::select(date, temp_moy_bassin),
  cohort_years  = cohort_years_sp,
  month_offsets = MONTH_OFFSETS
)

# --- Matrice d'anomalies (detrending harmonique) ---
temp_extended_anomaly <- build_temp_extended(
  monthly_series = temp_surface_mensuelle %>%
    dplyr::select(date, temp_anomaly_harm) %>%
    dplyr::rename(temp_moy_bassin = temp_anomaly_harm),
  cohort_years  = cohort_years_sp,
  month_offsets = MONTH_OFFSETS
)

for (nm in c("temp_extended_raw", "temp_extended_anomaly")) {
  m <- get(nm)
  n_na <- sum(is.na(m))
  if (n_na > 0) {
    na_cohorts <- cohort_years_sp[apply(m, 1, anyNA)]
    stop(sprintf(
      paste0(
        "%s contient %d NA (cohortes hors couverture temporelle). Cohortes concernees : %s. ",
        "Choix a faire explicitement : retirer ces cohortes ou reduire MONTH_OFFSETS."
      ),
      nm, n_na, paste(na_cohorts, collapse = ", ")
    ))
  }
}

w_uniform <- rep(1 / N_MONTHS, N_MONTHS)
cat("Correlation cohorte vs temp brute (moyenne uniforme) :",
    cor(cohort_years_sp, as.vector(temp_extended_raw %*% w_uniform)), "\n")
cat("Correlation cohorte vs anomalie (moyenne uniforme) :",
    cor(cohort_years_sp, as.vector(temp_extended_anomaly %*% w_uniform)), "\n")

# --- Covariables annuelles fixes, TOUJOURS calculees sur la brute,
# independamment de la methode de construction de T_index ---
T_mean <- rowMeans(temp_extended_raw)
T_sd   <- apply(temp_extended_raw, 1, sd)
T_min  <- apply(temp_extended_raw, 1, min)
T_max  <- apply(temp_extended_raw, 1, max)

cat("T_mean : range =", paste(round(range(T_mean), 3), collapse = " - "), "\n")
cat("T_sd   : range =", paste(round(range(T_sd),   3), collapse = " - "), "\n")
cat("T_min  : range =", paste(round(range(T_min),  3), collapse = " - "), "\n")
cat("T_max  : range =", paste(round(range(T_max),  3), collapse = " - "), "\n")

# Verification de la contrainte : sous ponderation uniforme, T_index(raw) == T_mean
stopifnot(isTRUE(all.equal(as.vector(temp_extended_raw %*% w_uniform), T_mean)))
cat("Contrainte T_index(raw, uniforme) == T_mean : OK\n")

# --- Diagnostic de colinearite entre les 4 covariables fixes ---
mat_diag <- cbind(T_mean, T_sd, T_min, T_max)
cat("\nMatrice de correlation (T_mean, T_sd, T_min, T_max) :\n")
print(round(cor(mat_diag), 3))

vif_diag <- sapply(colnames(mat_diag), function(v) {
  fit_v <- lm(mat_diag[, v] ~ mat_diag[, setdiff(colnames(mat_diag), v)])
  1 / (1 - summary(fit_v)$r.squared)
})
cat("VIF (T_mean, T_sd, T_min, T_max) :\n")
print(round(vif_diag, 2))
if (any(vif_diag > 5)) {
  cat("ATTENTION : au moins un VIF > 5, colinearite a surveiller dans l'interpretation des beta.\n")
}

# Signature RTMB : depend de MONTH_OFFSETS, utilisee pour le modele nul
# et pour la boucle methode x forme (temp_extended_raw/anomaly, T_mean, etc.)
model_sig_rtmb <- paste(model_sig_gam, paste(range(MONTH_OFFSETS), collapse = "-"), sep = "_")

# ----------------------------------------------------------------
# 10. Modele NUL pour valider la NLL de RTMB contre bam_ref
# ----------------------------------------------------------------

data_null <- list(
  y            = data_sp$Numeric_sex,
  X_fixed      = X_fixed_no_intercept,
  cohort_id    = cohort_id_per_obs,
  penalty_list = penalty_list_adjusted,
  lambda_fixed = lambda_bam_ref_aligned
)

parameters_null <- list(
  beta_fixed       = rep(0, ncol(X_fixed_no_intercept)),
  b_cohort         = rep(0, n_cohort),
  log_sigma_cohort = 0
)

make_f_null_iid <- function(data_null) {
  function(parms) {
    getAll(parms, data_null)

    sigma_cohort <- exp(log_sigma_cohort)

    eta <- as.vector(X_fixed %*% beta_fixed) + b_cohort[cohort_id]

    log_prob    <- -log1p(exp(-eta))
    log_1m_prob <- -log1p(exp(eta))
    nll_obs <- -sum(y * log_prob + (1 - y) * log_1m_prob)

    nll_penalty <- 0
    for (k in seq_len(length(penalty_list))) {
      cols_k <- penalty_list[[k]]$cols_local
      beta_k <- beta_fixed[cols_k]
      S_k    <- penalty_list[[k]]$S
      nll_penalty <- nll_penalty + 0.5 * lambda_fixed[k] * as.numeric(t(beta_k) %*% S_k %*% beta_k)
    }

    nll_resid_cohort <- -sum(dnorm(b_cohort, mean = 0, sd = sigma_cohort, log = TRUE))

    nll <- nll_obs + nll_penalty + nll_resid_cohort

    REPORT(eta)
    REPORT(sigma_cohort)
    REPORT(b_cohort)

    nll
  }
}

cat("\n--- Modele nul (cohorte IID, sans index thermique) ---\n")
obj_null <- RTMB::MakeADFun(make_f_null_iid(data_null), parameters_null,
                             random = "b_cohort", silent = TRUE)

null_model_cache_file <- file.path(CACHE_DIR, paste0("null_model_", model_sig_rtmb, ".rds"))

if (!FORCE_REFIT_NULL && file.exists(null_model_cache_file)) {
  cat("Modele nul charge depuis le cache :", null_model_cache_file, "\n")
  null_cache <- readRDS(null_model_cache_file)
  opt_null   <- null_cache$opt_null
  rep_null   <- null_cache$rep_null
} else {
  opt_null_1 <- nlminb(obj_null$par, obj_null$fn, obj_null$gr,
                        control = list(trace = 1, iter.max = 5000, eval.max = 10000,
                                       rel.tol = 1e-12, x.tol = 1e-10))
  opt_null <- nlminb(opt_null_1$par, obj_null$fn, obj_null$gr,
                      control = list(trace = 1, iter.max = 5000, eval.max = 10000,
                                     rel.tol = 1e-12, x.tol = 1e-10))
  rep_null <- obj_null$report(obj_null$env$last.par.best)
  saveRDS(list(opt_null = opt_null, rep_null = rep_null), null_model_cache_file)
  cat("Modele nul sauvegarde dans :", null_model_cache_file, "\n")
}
cat("Convergence modele nul :", opt_null$convergence, "| Message :", opt_null$message, "\n")
g_null <- obj_null$gr(opt_null$par)
cat("Max |gradient| modele nul :", max(abs(g_null)), "\n")


# ----------------------------------------------------------------
# 11. La NLL RTMB reproduit-elle bam_ref ?
# ----------------------------------------------------------------

eta_bam_ref   <- bam_ref$linear.predictors
eta_rtmb_null <- rep_null$eta

stopifnot(length(eta_bam_ref) == length(eta_rtmb_null))

cor_eta  <- cor(eta_bam_ref, eta_rtmb_null)
rmse_eta <- sqrt(mean((eta_bam_ref - eta_rtmb_null)^2))

cat("\n--- Validation RTMB vs bam_ref (modele nul, cohorte IID) ---\n")
cat("Cor(eta_bam_ref, eta_rtmb) :", cor_eta, "\n")
cat("RMSE(eta_bam_ref, eta_rtmb) :", rmse_eta, "\n")

if (cor_eta < 0.99) {
  warning(paste0(
    "Le predicteur lineaire RTMB (modele nul) ne reproduit pas bam_ref d'assez pres ",
    "(cor = ", round(cor_eta, 4), "). Ne pas interpreter le modele complet avant d'avoir ",
    "diagnostique cet ecart (alignement colonnes X_fixed/penalty_list, echelle du lien, ",
    "parametrisation intercept/cohorte/fs)."
  ))
}

# ----------------------------------------------------------------
# 13. BOUCLE : 2 methodes de construction thermique x 4 formes de fenetre
# ----------------------------------------------------------------

data_full_real <- list(
  y                      = data_sp$Numeric_sex,
  X_fixed                = X_fixed_no_intercept,
  cohort_id              = cohort_id_per_obs,
  temp_extended_raw      = temp_extended_raw,
  temp_extended_anomaly  = temp_extended_anomaly,
  T_mean                 = T_mean,
  T_sd                   = T_sd,
  T_min                  = T_min,
  T_max                  = T_max,
  penalty_list           = penalty_list_adjusted,
  lambda_fixed           = lambda_bam_ref_aligned,
  month_offsets          = MONTH_OFFSETS
)

beta_null <- opt_null$par[names(opt_null$par) == "beta_fixed"]

build_parameters <- function(method, shape) {
  base <- list(
    beta_fixed              = beta_null,
    intercept_cohort        = 0,
    slope_cohort             = 0.3,
    b_cohort_resid           = rep_null$b_cohort,
    log_sigma_cohort_resid  = log(rep_null$sigma_cohort),
    beta_sd                 = 0,
    beta_min                = 0,
    beta_max                = 0
  )
  # T_mean n'est un parametre libre qu'en methode harm : en methode raw,
  # T_index le remplace (meme contenu si w_month est uniforme).
  method_params <- if (method == "harm") {
    list(beta_mean = 0)
  } else {
    list()
  }
  shape_params <- switch(shape,
    "gaussian"    = list(mu = mean(MONTH_OFFSETS), log_sigma = log(3)),
    "uniform"     = list(),
    "linear"      = list(slope_w = 0),
    "skewnormal"  = list(xi = mean(MONTH_OFFSETS), log_omega = log(3), alpha = 0)
  )
  c(base, method_params, shape_params)
}

make_f_full <- function(data_full_w, method, shape) {
  function(parms) {
    getAll(parms, data_full_w)

    sigma_cohort_resid <- exp(log_sigma_cohort_resid)
    lambda              <- lambda_fixed
    N_MONTHS_local       <- length(month_offsets)

    temp_mat <- if (method == "harm") temp_extended_anomaly else temp_extended_raw

    if (shape == "gaussian") {
      sigma_w <- exp(log_sigma)
      w_raw   <- dnorm(month_offsets, mu, sigma_w)
      nll_prior_shape <- -dnorm(log_sigma, mean = log(3), sd = 1.5, log = TRUE)

    } else if (shape == "uniform") {
      w_raw   <- rep(1, N_MONTHS_local)
      nll_prior_shape <- 0

    } else if (shape == "linear") {
      offset_ctr <- month_offsets - mean(month_offsets)
      w_raw <- exp(slope_w * offset_ctr)
      nll_prior_shape <- -dnorm(slope_w, mean = 0, sd = 0.5, log = TRUE)

    } else if (shape == "skewnormal") {
      omega <- exp(log_omega)
      z     <- (month_offsets - xi) / omega
      w_raw <- (2 / omega) * dnorm(z) * pnorm(alpha * z)
      nll_prior_shape <- -dnorm(log_omega, mean = log(3), sd = 1.5, log = TRUE) +
                          (-dnorm(alpha, mean = 0, sd = 3, log = TRUE))
    }

    w_month <- w_raw / sum(w_raw)
    thermal_index <- as.vector(temp_mat %*% w_month)

    cohort_pred_thermal <- if (method == "harm") {
      intercept_cohort +
        beta_mean * T_mean + beta_sd * T_sd + beta_min * T_min + beta_max * T_max +
        slope_cohort * thermal_index
    } else {
      intercept_cohort +
        beta_sd * T_sd + beta_min * T_min + beta_max * T_max +
        slope_cohort * thermal_index
    }
    cohort_effect_total <- cohort_pred_thermal + b_cohort_resid

    eta <- as.vector(X_fixed %*% beta_fixed) + cohort_effect_total[cohort_id]

    log_prob    <- -log1p(exp(-eta))
    log_1m_prob <- -log1p(exp(eta))
    nll_obs <- -sum(y * log_prob + (1 - y) * log_1m_prob)

    nll_penalty <- 0
    for (k in seq_len(length(penalty_list))) {
      cols_k <- penalty_list[[k]]$cols_local
      beta_k <- beta_fixed[cols_k]
      S_k    <- penalty_list[[k]]$S
      nll_penalty <- nll_penalty + 0.5 * lambda[k] * as.numeric(t(beta_k) %*% S_k %*% beta_k)
    }

    nll_resid_cohort <- -sum(dnorm(b_cohort_resid, mean = 0, sd = sigma_cohort_resid, log = TRUE))

    nll <- nll_obs + nll_penalty + nll_resid_cohort + nll_prior_shape

    REPORT(w_month)
    REPORT(slope_cohort)
    REPORT(sigma_cohort_resid)
    REPORT(thermal_index)
    REPORT(beta_sd); REPORT(beta_min); REPORT(beta_max)
    ADREPORT(w_month)
    ADREPORT(slope_cohort)
    ADREPORT(beta_sd); ADREPORT(beta_min); ADREPORT(beta_max)
    if (method == "harm") { REPORT(beta_mean); ADREPORT(beta_mean) }
    if (shape == "gaussian")   { REPORT(mu); REPORT(sigma_w); ADREPORT(mu); ADREPORT(sigma_w) }
    if (shape == "linear")     { REPORT(slope_w); ADREPORT(slope_w) }
    if (shape == "skewnormal") { REPORT(xi); REPORT(omega); REPORT(alpha)
                                 ADREPORT(xi); ADREPORT(omega); ADREPORT(alpha) }

    nll
  }
}

methods <- c("harm", "raw")
shapes  <- c("gaussian", "uniform", "linear", "skewnormal")
combos  <- expand.grid(method = methods, shape = shapes, stringsAsFactors = FALSE)

resultats_par_combo <- list()

for (i in seq_len(nrow(combos))) {

  method <- combos$method[i]
  shape  <- combos$shape[i]
  combo_key <- paste(method, shape, sep = "_")

  cat("\n=== Methode :", method, "| Forme :", shape, "===\n")

  set.seed(42)
  parameters_combo <- build_parameters(method, shape)
  f_combo <- make_f_full(data_full_real, method, shape)

  obj_combo <- RTMB::MakeADFun(
    f_combo, parameters_combo,
    random = "b_cohort_resid",
    silent = TRUE
  )

  opt_combo <- tryCatch(
    nlminb(obj_combo$par, obj_combo$fn, obj_combo$gr,
           control = list(trace = 1, iter.max = 3000, eval.max = 6000)),
    error = function(e) { cat("  ECHEC optimisation :", conditionMessage(e), "\n"); NULL }
  )

  if (is.null(opt_combo)) {
    resultats_par_combo[[combo_key]] <- list(method = method, shape = shape, echec = TRUE)
    next
  }

  rep_combo <- obj_combo$report(obj_combo$env$last.par.best)
  sd_combo  <- tryCatch(sdreport(obj_combo), error = function(e) NULL)

  k_combo   <- length(opt_combo$par)
  AIC_combo <- 2 * opt_combo$objective + 2 * k_combo

  z_slope <- NA; p_slope <- NA; se_slope <- NA
  if (!is.null(sd_combo) && isTRUE(sd_combo$pdHess)) {
    tab_combo <- summary(sd_combo, select = "report")
    se_slope <- tab_combo["slope_cohort", "Std. Error"]
    z_slope  <- rep_combo$slope_cohort / se_slope
    p_slope  <- 2 * pnorm(-abs(z_slope))
  }

  resultats_par_combo[[combo_key]] <- list(
    method            = method,
    shape             = shape,
    convergence       = opt_combo$convergence,
    message           = opt_combo$message,
    pdHess            = if (!is.null(sd_combo)) sd_combo$pdHess else NA,
    k                 = k_combo,
    objective         = opt_combo$objective,
    AIC               = AIC_combo,
    slope_est         = rep_combo$slope_cohort,
    se_slope          = se_slope,
    z                 = z_slope,
    p                 = p_slope,
    w_month           = rep_combo$w_month,
    beta_mean         = if (method == "harm") rep_combo$beta_mean else NA,
    beta_sd           = rep_combo$beta_sd,
    beta_min          = rep_combo$beta_min,
    beta_max          = rep_combo$beta_max,
    sd_report_summary = if (!is.null(sd_combo) && isTRUE(sd_combo$pdHess))
                         as.data.frame(summary(sd_combo, select = "report")) else NULL,
    echec = FALSE
  )

  cat("  convergence :", opt_combo$convergence, "| pdHess :",
      if (!is.null(sd_combo)) sd_combo$pdHess else NA, "\n")
  cat("  slope :", round(rep_combo$slope_cohort, 4),
      "| z :", round(z_slope, 3), "| p :", round(p_slope, 4),
      "| AIC :", round(AIC_combo, 2), "\n")

  saveRDS(resultats_par_combo,
          file.path(CACHE_DIR, paste0("resultats_par_combo_", model_sig_rtmb, ".rds")))

  rm(obj_combo, opt_combo, rep_combo, sd_combo, parameters_combo, f_combo)
  gc(verbose = FALSE)
  cat("  [memoire liberee]\n")
}

# ----------------------------------------------------------------
# 14. Tableau comparatif des 8 combinaisons methode x forme
# ----------------------------------------------------------------

k_null <- length(opt_null$par)
AIC_null <- 2 * opt_null$objective + 2 * k_null

tab_comparatif <- do.call(rbind, lapply(resultats_par_combo, function(r) {
  if (isTRUE(r$echec)) {
    data.frame(method = r$method, shape = r$shape, convergence = NA, pdHess = NA,
               k = NA, AIC = NA, delta_AIC_vs_null = NA, slope = NA, z = NA, p = NA)
  } else {
    data.frame(method = r$method, shape = r$shape, convergence = r$convergence,
               pdHess = r$pdHess, k = r$k, AIC = round(r$AIC, 2),
               delta_AIC_vs_null = round(AIC_null - r$AIC, 2),
               slope = round(r$slope_est, 4), z = round(r$z, 3), p = round(r$p, 4))
  }
}))
rownames(tab_comparatif) <- NULL
tab_comparatif <- tab_comparatif[order(tab_comparatif$AIC), ]

cat("\n=== TABLEAU COMPARATIF (2 methodes x 4 formes) ===\n")
cat("AIC modele nul :", round(AIC_null, 2), "\n")
print(tab_comparatif)

saveRDS(tab_comparatif, file.path(CACHE_DIR, paste0("tab_comparatif_combos_", model_sig_rtmb, ".rds")))

# ----------------------------------------------------------------
# 15. Visualisation comparative des 8 fenetres
# ----------------------------------------------------------------

par(mfrow = c(2, 4))
for (method in methods) {
  for (shape in shapes) {
    r <- resultats_par_combo[[paste(method, shape, sep = "_")]]
    if (isTRUE(r$echec)) { plot.new(); title(paste(method, shape, "- ECHEC")); next }
    plot(MONTH_OFFSETS, r$w_month, type = "b", pch = 19,
         col = if (method == "harm") "red" else "blue",
         xlab = "Offset mensuel", ylab = "Poids",
         main = paste0(method, " / ", shape, " (p=", round(r$p, 3), ")"))
    abline(h = 1/N_MONTHS, lty = 3, col = "grey50")
  }
}
par(mfrow = c(1, 1))

# ----------------------------------------------------------------
# 16. Sauvegarde finale - synthese des 2 methodes x 4 formes
# ----------------------------------------------------------------

resultats_finaux <- list(
  species             = SPECIES_NAME,
  MONTH_OFFSETS       = MONTH_OFFSETS,
  N_MONTHS            = N_MONTHS,
  AIC_null            = AIC_null,
  tab_comparatif      = tab_comparatif,
  resultats_par_combo = resultats_par_combo
)

saveRDS(resultats_finaux,
        file.path(CACHE_DIR, paste0("resultats_finaux_combos_", model_sig_rtmb, ".rds")))
cat("\n=== Resultats finaux (2 methodes x 4 formes) sauvegardes dans cache/resultats_finaux_combos_",
    model_sig_rtmb, ".rds ===\n")