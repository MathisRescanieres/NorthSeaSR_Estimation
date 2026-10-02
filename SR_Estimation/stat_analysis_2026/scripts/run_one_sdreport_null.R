# ================================================================
# run_one_sdreport_null.R
# Reconstruit COMPLETEMENT le modele NUL pour UNE espece, depuis
# bam_ref (pas depuis data_null_used, absent pour certaines especes).
# Calcule le sdreport, sauvegarde le resultat.
# Usage : Rscript run_one_sdreport_null.R "<espece>"
# ================================================================

args <- commandArgs(trailingOnly = TRUE)
sp <- args[1]

library(RTMB)
library(mgcv)
library(dplyr)
library(rprojroot)

proj_root <- find_root(has_file(".git") | is_rstudio_project)
setwd(file.path(proj_root, "SR_Estimation", "stat_analysis_2026", "data"))
load("temp_v4.RData")  # doit fournir data_expanded_1991_2023 / data_expanded

rtmb_null_dir <- "/home/mathis/NorthSeaSR_Estimation/SR_Estimation/stat_analysis_2026/data/results_rtmb/detrend/rtmb_null_models"
base_dir_bam  <- "/home/mathis/NorthSeaSR_Estimation/SR_Estimation/stat_analysis_2026/data/results_rtmb/detrend/bam_null_models"
out_root_sdreport <- "/home/mathis/NorthSeaSR_Estimation/SR_Estimation/stat_analysis_2026/data/results_rtmb/rtmb_null_sdreport"

if (!dir.exists(out_root_sdreport)) dir.create(out_root_sdreport, recursive = TRUE)

# ---- Config identique au pipeline principal ----
K_TENSOR_BY_SPECIES <- list(
  "Trisopterus esmarkii"      = c(7, 8, 8),
  "Pleuronectes platessa"     = c(6, 6, 8),
  "Clupea harengus"           = c(6, 6, 8),
  "Merlangius merlangus"      = c(6, 6, 8),
  "Melanogrammus aeglefinus"  = c(6, 6, 8)
)
k_tensor_default <- c(8, 8, 8)

K_SPACE_BY_SPECIES <- list(
  "Pollachius virens"     = 120,
  "Pleuronectes platessa" = 240,
  "Trisopterus esmarkii"  = 240,
  "Sprattus sprattus"     = 240
)
k_space_default <- 120
k_time_default  <- 12

terms_removed <- list("Pollachius virens" = c("f4", "bc"))

MONTH_OFFSETS <- 0:11  

# ---- Bloc 4 : formule (identique au pipeline) ----
build_formula_sp <- function(k_age, k_lngt, k_space, k_time, k_cohort, removed) {
  terms <- list()
  terms[["te3d"]] <- bquote(
    te(Age_sc, LngtClassGrouped_sc, Cohort_num_sc,
       bs = c("cr", "cr", "cs"),
       k  = c(.(k_age), .(k_lngt), .(k_cohort)))
  )
  terms[["f3_space"]] <- bquote(s(Latitude, Longitude, k = .(k_space), bs = "sos"))
  if (!"f4" %in% removed) terms[["f4"]] <- bquote(s(julian_day, bs = "cc", k = .(k_time)))
  if (!"bc" %in% removed) terms[["bc"]] <- quote(s(Cohort_fact, bs = "re"))
  rhs <- Reduce(function(a, b) call("+", a, b), terms)
  as.formula(bquote(Numeric_sex ~ .(rhs)))
}

# ---- Bloc 6 : structure BAM (identique au pipeline) ----
extract_bam_structure <- function(data_sp, formula_sp, bam_ref) {
  bam_setup <- mgcv::gam(formula_sp, family = binomial(link = "logit"), data = data_sp, fit = FALSE)
  smooth_info <- bam_setup$smooth
  re_idx <- which(sapply(smooth_info, function(s) s$term[1] == "Cohort_fact" &&
                            inherits(s, "random.effect")))
  cols_re <- if (length(re_idx) > 0) {
    smooth_info[[re_idx]]$first.para:smooth_info[[re_idx]]$last.para
  } else integer(0)
  cols_fixed <- setdiff(seq_len(ncol(bam_setup$X)), cols_re)

  Xp_bam <- predict(bam_ref, type = "lpmatrix")
  X_fixed <- Xp_bam[, cols_fixed, drop = FALSE]

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
  list(X_fixed = X_fixed, penalty_list = penalty_list, na_action = bam_setup$na.action)
}

# ================================================================
#  Reconstruction complete pour l'espece sp
# ================================================================

cat("\n==========", sp, "==========\n")

data_sp <- data_expanded_1991_2023 %>%
  dplyr::filter(Species == sp, !is.na(Numeric_sex)) %>%
  droplevels()

cat("N =", nrow(data_sp), "\n")

removed_sp <- terms_removed[[sp]]

k_tensor <- if (!is.null(K_TENSOR_BY_SPECIES[[sp]])) K_TENSOR_BY_SPECIES[[sp]] else k_tensor_default
k_age  <- k_tensor[1]; k_lngt <- k_tensor[2]; k_fs <- k_tensor[3]
k_space <- if (!is.null(K_SPACE_BY_SPECIES[[sp]])) K_SPACE_BY_SPECIES[[sp]] else k_space_default
k_time  <- k_time_default

formula_sp <- build_formula_sp(k_age, k_lngt, k_space, k_time, k_fs, removed_sp)
cat("Formule :\n")
print(formula_sp)

# ---- Chargement de bam_ref (deja en cache normalement) ----
sp_dir_bam <- file.path(base_dir_bam, sp)
rds_files_bam <- list.files(sp_dir_bam, pattern = "^bam_ref_.*\\.rds$", full.names = TRUE)
if (length(rds_files_bam) == 0) stop("bam_ref introuvable pour ", sp)
if (length(rds_files_bam) > 1) {
  info <- file.info(rds_files_bam)
  rds_file_bam <- rds_files_bam[which.max(info$mtime)]
} else {
  rds_file_bam <- rds_files_bam[1]
}
bam_ref <- readRDS(rds_file_bam)
cat("bam_ref charge :", basename(rds_file_bam), "\n")

lambda_bam_ref <- bam_ref$sp

# ---- Structure BAM ----
bam_struct_full <- extract_bam_structure(data_sp, formula_sp, bam_ref)

if (!is.null(bam_struct_full$na_action)) {
  cat("ATTENTION :", length(bam_struct_full$na_action), "lignes supprimees par na.omit.\n")
  data_sp <- data_sp[-bam_struct_full$na_action, , drop = FALSE]
  data_sp$Cohort_fact <- droplevels(data_sp$Cohort_fact)
}

stopifnot(nrow(bam_struct_full$X_fixed) == nrow(data_sp))

cohort_levels     <- levels(data_sp$Cohort_fact)
cohort_id_per_obs <- as.integer(data_sp$Cohort_fact)
n_cohort          <- length(cohort_levels)

X_fixed_full      <- bam_struct_full$X_fixed
penalty_list_full <- bam_struct_full$penalty_list

# ---- Alignement lambda ----
LAMBDA_NAMES_STANDARD <- c(
  "te(Age_sc,LngtClassGrouped_sc,Cohort_num_sc)1",
  "te(Age_sc,LngtClassGrouped_sc,Cohort_num_sc)2",
  "te(Age_sc,LngtClassGrouped_sc,Cohort_num_sc)3",
  "s(Latitude,Longitude)", "s(julian_day)"
)
LAMBDA_NAMES_NO_F4 <- c(
  "te(Age_sc,LngtClassGrouped_sc,Cohort_num_sc)1",
  "te(Age_sc,LngtClassGrouped_sc,Cohort_num_sc)2",
  "te(Age_sc,LngtClassGrouped_sc,Cohort_num_sc)3",
  "s(Latitude,Longitude)"
)
lambda_names_sp <- if (!is.null(removed_sp) && "f4" %in% removed_sp) {
  LAMBDA_NAMES_NO_F4
} else {
  LAMBDA_NAMES_STANDARD
}

lambda_bam_ref_aligned <- as.numeric(lambda_bam_ref[lambda_names_sp])
stopifnot(!anyNA(lambda_bam_ref_aligned))
stopifnot(length(lambda_bam_ref_aligned) == length(penalty_list_full))

# ---- data_null ----
data_null <- list(
  y = data_sp$Numeric_sex, X_fixed = X_fixed_full,
  cohort_id = cohort_id_per_obs, penalty_list = penalty_list_full,
  lambda_fixed = lambda_bam_ref_aligned
)

beta_fixed_init <- as.numeric(coef(bam_ref))[seq_len(ncol(X_fixed_full))]

has_bc_term <- is.null(removed_sp) || !("bc" %in% removed_sp)
if (has_bc_term) {
  b_cohort_init <- as.numeric(coef(bam_ref)[grep("^s\\(Cohort_fact\\)", names(coef(bam_ref)))])
  stopifnot(length(b_cohort_init) == n_cohort)
  log_sigma_cohort_init <- log(sd(b_cohort_init))
} else {
  b_cohort_init <- rep(0, n_cohort)
  log_sigma_cohort_init <- 0
}

# ---- Fonction objectif, identique au pipeline principal ----
make_f_null_iid_with_re <- function(data_null) {
  function(parms) {
    getAll(parms, data_null)
    sigma_cohort <- exp(log_sigma_cohort)
    eta <- as.vector(X_fixed %*% beta_fixed) + b_cohort[cohort_id]
    p_hat <- 1 / (1 + exp(-eta))
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
    REPORT(eta); REPORT(sigma_cohort); REPORT(b_cohort)
    REPORT(p_hat)
    nll
  }
}

make_f_null_no_re <- function(data_null) {
  function(parms) {
    getAll(parms, data_null)
    eta <- as.vector(X_fixed %*% beta_fixed)
    p_hat <- 1 / (1 + exp(-eta))
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
    nll <- nll_obs + nll_penalty
    REPORT(eta)
    REPORT(p_hat)
    nll
  }
}

if (has_bc_term) {
  parameters_null <- list(
    beta_fixed = beta_fixed_init,
    b_cohort = b_cohort_init,
    log_sigma_cohort = log_sigma_cohort_init
  )
  obj_null <- RTMB::MakeADFun(make_f_null_iid_with_re(data_null), parameters_null,
                               random = "b_cohort", silent = TRUE)
} else {
  parameters_null <- list(
    beta_fixed = beta_fixed_init
  )
  obj_null <- RTMB::MakeADFun(make_f_null_no_re(data_null), parameters_null,
                               silent = TRUE)
}

opt_final <- nlminb(obj_null$par, obj_null$fn, obj_null$gr,
                     control = list(iter.max = 5000, eval.max = 10000,
                                     rel.tol = 1e-12, x.tol = 1e-10))

cat("Convergence :", opt_final$convergence, "| Objective :", round(opt_final$objective, 2), "\n")

sd_final <- tryCatch(
  sdreport(obj_null),
  error = function(e) { cat("Echec sdreport :", conditionMessage(e), "\n"); NULL }
)

resultat <- list(
  species = sp,
  objective_final = opt_final$objective,
  convergence = opt_final$convergence,
  sdreport_full = sd_final,
  pdHess = if (!is.null(sd_final)) sd_final$pdHess else NA,
  data_null_used = data_null,
  data_sp_used = data_sp,
  last_par_best = obj_null$env$last.par.best
)

# ---- Sauvegarde dans le sous-dossier de l'espece ----
sp_slug <- gsub(" ", "_", sp)
out_dir_sp <- file.path(out_root_sdreport, sp_slug)
dir.create(out_dir_sp, recursive = TRUE, showWarnings = FALSE)

saveRDS(resultat, file.path(out_dir_sp, paste0("sdreport_null_", sp_slug, ".rds")))

cat("Termine pour", sp, "\n")
