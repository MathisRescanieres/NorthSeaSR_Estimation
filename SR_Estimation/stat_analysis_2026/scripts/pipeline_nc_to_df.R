# ==============================================================================
# pipeline_nc_to_df.R
# ------------------------------------------------------------------------------
# Lit un fichier NetCDF de températures 4D (lon x lat x depth x time),
# calcule les covariables verticales (moyenne, variance, gradient) et
# retourne trois data.frames : df_T_mean, df_T_var, df_T_grad
#
# Usage :
#   source("pipeline_nc_to_df.R")
#   result    <- run_nc_pipeline(file_nc = "mon_fichier.nc", max_depth = 750)
#   df_T_mean <- result$df_T_mean
#   df_T_var  <- result$df_T_var
#   df_T_grad <- result$df_T_grad
# ==============================================================================

library(ncdf4)
library(tidyverse)
library(lubridate)

# ── Fonction interne : poids des couches (trapèzes) ───────────────────────────
.compute_depth <- function(z) {
  n  <- length(z)
  dz <- numeric(n)
  if (n == 1) { dz[1] <- z[1]; return(dz) }
  dz[1] <- (z[2] - z[1]) / 2
  dz[n] <- (z[n] - z[n - 1]) / 2
  if (n > 2) dz[2:(n - 1)] <- (z[3:n] - z[1:(n - 2)]) / 2
  return(dz)
}

# ── Fonction interne : conversion array → data.frame ─────────────────────────
.array_to_df <- function(arr, varname, lon, lat, time) {
  dimnames(arr) <- list(
    lon  = as.character(round(lon, 3)),
    lat  = as.character(round(lat, 3)),
    time = as.character(time)
  )
  as.data.frame.table(arr, responseName = varname) %>%
    mutate(
      lon   = as.numeric(as.character(lon)),
      lat   = as.numeric(as.character(lat)),
      time  = as.Date(as.character(time)),
      year  = year(time),
      month = month(time)
    ) %>%
    filter(!is.na(.data[[varname]]))
}

# ══════════════════════════════════════════════════════════════════════════════
# FONCTION PRINCIPALE
# ══════════════════════════════════════════════════════════════════════════════
#
# Paramètres :
#   file_nc   : chemin vers le fichier NetCDF
#   max_depth : profondeur maximale retenue (en mètres)
#
# Retourne une liste avec :
#   $df_T_mean  : data.frame lon / lat / time / year / month / T_mean
#   $df_T_var   : data.frame lon / lat / time / year / month / T_var
#   $df_T_grad  : data.frame lon / lat / time / year / month / T_grad

run_nc_pipeline <- function(file_nc, max_depth = 750) {

  # ── Lecture des coordonnées ──────────────────────────────────────────────────
  cat(">>> [1/3] Lecture du fichier NetCDF\n")
  nc    <- nc_open(file_nc)
  lon   <- ncvar_get(nc, "longitude")
  lat   <- ncvar_get(nc, "latitude")
  depth <- ncvar_get(nc, "depth")
  time  <- as.Date(ncvar_get(nc, "time"), origin = "1950-01-01")

  idx_z_candidates <- which(depth <= max_depth)
  depth_sel        <- depth[idx_z_candidates]
  n_z              <- length(depth_sel)
  cat("    Couches retenues (<= ", max_depth, " m) :", depth_sel, "\n")

  # ── Lecture du tenseur de température ───────────────────────────────────────
  T_sel <- ncvar_get(
    nc, "TEMP",
    start = c(1, 1, min(idx_z_candidates), 1),
    count = c(length(lon), length(lat), n_z, length(time))
  )
  T_sel[is.nan(T_sel)] <- NA
  nc_close(nc)
  cat("    Dimensions T_sel :", dim(T_sel), "\n")

  # ── Calcul des covariables verticales ────────────────────────────────────────
  cat(">>> [2/3] Calcul des covariables verticales\n")
  n_lon  <- length(lon)
  n_lat  <- length(lat)
  n_time <- length(time)

  T_bar_z  <- array(NA_real_, dim = c(n_lon, n_lat, n_time))
  T_var_z  <- array(NA_real_, dim = c(n_lon, n_lat, n_time))
  T_grad_z <- array(NA_real_, dim = c(n_lon, n_lat, n_time))

  pb   <- txtProgressBar(min = 0, max = n_lon * n_lat, style = 3)
  cmpt <- 0L

  for (i in seq_len(n_lon)) {
    for (j in seq_len(n_lat)) {
      profil <- T_sel[i, j, , ]
      for (t in seq_len(n_time)) {
        col       <- profil[, t]
        first_nan <- which(is.na(col))
        k_max     <- if (length(first_nan) > 0) first_nan[1] - 1L else n_z
        if (k_max == 0L) next

        z_loc  <- depth_sel[1:k_max]
        T_loc  <- col[1:k_max]
        dz_loc <- .compute_depth(z_loc)

        T_moy                <- sum(T_loc * dz_loc) / sum(dz_loc)
        T_bar_z[i, j, t]    <- T_moy
        T_var_z[i, j, t]    <- sum(dz_loc * (T_loc - T_moy)^2) / sum(dz_loc)
        T_grad_z[i, j, t]   <- if (k_max >= 2) {
          (T_loc[k_max] - T_loc[1]) / (z_loc[k_max] - z_loc[1])
        } else {
          0
        }
      }
      cmpt <- cmpt + 1L
      setTxtProgressBar(pb, cmpt)
    }
  }
  close(pb)

  # ── Conversion en data.frames ────────────────────────────────────────────────
  cat("\n>>> [3/3] Conversion en data.frames\n")
  df_T_mean <- .array_to_df(T_bar_z,  "T_mean", lon, lat, time)
  df_T_var  <- .array_to_df(T_var_z,  "T_var",  lon, lat, time)
  df_T_grad <- .array_to_df(T_grad_z, "T_grad", lon, lat, time)

  cat("\n✔ Pipeline terminé\n")
  cat("  df_T_mean :", nrow(df_T_mean), "lignes\n")
  cat("  df_T_var  :", nrow(df_T_var),  "lignes\n")
  cat("  df_T_grad :", nrow(df_T_grad), "lignes\n")

  list(
    df_T_mean = df_T_mean,
    df_T_var  = df_T_var,
    df_T_grad = df_T_grad
  )
}
