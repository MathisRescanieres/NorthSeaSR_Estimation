# ================================================================
# Reconstruction modele nul RTMB, une espece, appele en sous-process
# Usage : Rscript run_one_species.R <sp> <method> <k_age> <k_lngt> <k_fs> <k_space> <k_time> <bam_ref_file>
# ================================================================

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 8) {
  stop("Usage: Rscript run_one_species.R <sp> <method> <k_age> <k_lngt> <k_fs> <k_space> <k_time> <bam_ref_file>")
}

sp             <- args[1]
method         <- args[2]
k_age          <- as.integer(args[3])
k_lngt         <- as.integer(args[4])
k_fs           <- as.integer(args[5])
k_space        <- as.integer(args[6])
k_time         <- as.integer(args[7])
bam_ref_file   <- args[8]

cat("=== Debut :", sp, "===\n")

load("temp_v4.RData")

suppressPackageStartupMessages({
  library(dplyr)
  library(mgcv)
  library(RTMB)
  library(Matrix)
})

data_sp <- data_expanded_1991_2023 %>%
  dplyr::filter(Species == sp, !is.na(Numeric_sex)) %>%
  droplevels()

bam_ref <- readRDS(bam_ref_file)

formula_sp <- as.formula(
  Numeric_sex ~ te(Age_sc, LngtClassGrouped_sc, Cohort_num_sc,
                    bs = c("cr", "cr", "cs"), k = c(k_age, k_lngt, k_fs)) +
    s(Latitude, Longitude, k = k_space, bs = "sos") +
    s(julian_day, bs = "cc", k = k_time) +
    s(Cohort_fact, bs = "re")
)

bam_setup <- mgcv::gam(formula_sp, family = binomial(link = "logit"), data = data_sp, fit = FALSE)

smooth_info <- bam_setup$smooth
re_idx <- which(sapply(smooth_info, function(s) s$term[1] == "Cohort_fact" && inherits(s, "random.effect")))
cols_re <- if (length(re_idx) > 0) smooth_info[[re_idx]]$first.para:smooth_info[[re_idx]]$last.para else integer(0)
cols_fixed <- setdiff(seq_len(ncol(bam_setup$X)), cols_re)

Xp_bam <- predict(bam_ref, type = "lpmatrix")
X_fixed_full <- Xp_bam[, cols_fixed, drop = FALSE]
rm(Xp_bam, bam_setup)

penalty_list_full <- list()
for (i in seq_along(smooth_info)) {
  if (i %in% re_idx) next
  s_term <- smooth_info[[i]]
  cols_term <- s_term$first.para:s_term$last.para
  local_start <- 1 + (cols_term[1] - cols_fixed[1])
  for (j in seq_along(s_term$S)) {
    penalty_list_full[[length(penalty_list_full) + 1]] <- list(
      S = s_term$S[[j]],
      cols_local = local_start:(local_start + nrow(s_term$S[[j]]) - 1)
    )
  }
}

if (!is.null(bam_ref$na.action)) {
  data_sp <- data_sp[-bam_ref$na.action, , drop = FALSE]
  data_sp$Cohort_fact <- droplevels(data_sp$Cohort_fact)
}

cohort_levels <- levels(data_sp$Cohort_fact)
cohort_id_per_obs <- as.integer(data_sp$Cohort_fact)
n_cohort <- length(cohort_levels)

cat("Dimension X_fixed :", ncol(X_fixed_full), "colonnes,", nrow(X_fixed_full), "lignes\n")

LAMBDA_NAMES_STANDARD <- c(
  "te(Age_sc,LngtClassGrouped_sc,Cohort_num_sc)1",
  "te(Age_sc,LngtClassGrouped_sc,Cohort_num_sc)2",
  "te(Age_sc,LngtClassGrouped_sc,Cohort_num_sc)3",
  "s(Latitude,Longitude)", "s(julian_day)"
)
lambda_bam_ref_aligned <- as.numeric(bam_ref$sp[LAMBDA_NAMES_STANDARD])

beta_fixed_init <- as.numeric(coef(bam_ref))[seq_len(ncol(X_fixed_full))]
b_cohort_init <- as.numeric(coef(bam_ref)[grep("^s\\(Cohort_fact\\)", names(coef(bam_ref)))])
stopifnot(length(b_cohort_init) == n_cohort)

eta_check <- as.vector(X_fixed_full %*% beta_fixed_init) + b_cohort_init[cohort_id_per_obs]
max_diff <- max(abs(eta_check - bam_ref$linear.predictors))
cat("Verif alignement b_cohort_init :", max_diff, "\n")
stopifnot(max_diff < 1e-8)

log_sigma_cohort_init <- log(sd(b_cohort_init))

data_null <- list(
  y = data_sp$Numeric_sex, X_fixed = X_fixed_full,
  cohort_id = cohort_id_per_obs, penalty_list = penalty_list_full,
  lambda_fixed = lambda_bam_ref_aligned
)

parameters_null <- list(
  beta_fixed = beta_fixed_init,
  b_cohort = b_cohort_init,
  log_sigma_cohort = log_sigma_cohort_init
)

make_f_null_iid <- function(data_null) {
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
    REPORT(eta); REPORT(sigma_cohort); REPORT(b_cohort); REPORT(p_hat)
    nll
  }
}

obj_null <- RTMB::MakeADFun(make_f_null_iid(data_null), parameters_null,
                             random = "b_cohort", silent = TRUE)

cat("Nb parametres optimises (hors random) :", length(obj_null$par), "\n")

t0 <- Sys.time()
opt_null_1 <- nlminb(obj_null$par, obj_null$fn, obj_null$gr,
                      control = list(trace = 50, iter.max = 5000, eval.max = 30000,
                                     rel.tol = 1e-12, x.tol = 1e-10))
cat("Passe 1 terminee en", round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 2),
    "min | convergence =", opt_null_1$convergence, "| objective =", round(opt_null_1$objective, 2), "\n")

t0 <- Sys.time()
opt_null <- nlminb(opt_null_1$par, obj_null$fn, obj_null$gr,
                    control = list(trace = 50, iter.max = 5000, eval.max = 30000,
                                   rel.tol = 1e-12, x.tol = 1e-10))
cat("Passe 2 terminee en", round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 2),
    "min | convergence =", opt_null$convergence, "| objective =", round(opt_null$objective, 2), "\n")

rep_null <- obj_null$report(obj_null$env$last.par.best)

rtmb_null_dir <- file.path("results_rtmb", method, "rtmb_null_models", sp)
if (!dir.exists(rtmb_null_dir)) dir.create(rtmb_null_dir, recursive = TRUE)

model_sig_bam <- paste(sp, k_age, k_lngt, k_space, k_time, k_fs, "cs3", nrow(data_sp), sep = "_")
model_sig_rtmb <- paste(model_sig_bam, "0-11", sep = "_")
null_model_cache_file <- file.path(rtmb_null_dir, paste0("null_model_", model_sig_rtmb, ".rds"))

saveRDS(list(
  opt_null = opt_null, rep_null = rep_null,
  parameters_init = parameters_null, data_null_used = data_null,
  data_sp_used = data_sp, last_par_best = obj_null$env$last.par.best
), null_model_cache_file)

cat("Sauvegarde :", null_model_cache_file, "\n")

cor_eta <- cor(bam_ref$linear.predictors, rep_null$eta)
cat("Cor(eta_bam, eta_rtmb) :", round(cor_eta, 5), "\n")

grad_norm <- max(abs(obj_null$gr(opt_null$par)))
cat("grad_norm final :", grad_norm, "\n")

cat("=== Fin :", sp, "===\n")
quit(save = "no", status = 0)
