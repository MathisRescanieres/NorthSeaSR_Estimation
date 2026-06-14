# ==============================================================================
# pipeline_nc_to_df.R
# ------------------------------------------------------------------------------
# Lit un fichier NetCDF de températures 4D (lon x lat x depth x time),
# et retourne un data.frame long avec les colonnes :
#   lon | lat | depth | time | year | month | temp
#
# Usage :
#   source("pipeline_nc_to_df.R")
#   df <- run_nc_pipeline(file_nc = "mon_fichier.nc", max_depth = 750)
# ==============================================================================

library(ncdf4)
library(dplyr)
library(tidyr)
library(lubridate)
library(purrr)
library(conflicted)

# ══════════════════════════════════════════════════════════════════════════════
# FONCTION PRINCIPALE
# ══════════════════════════════════════════════════════════════════════════════
#
# Paramètres :
#   file_nc   : chemin vers le fichier NetCDF
#   max_depth : profondeur maximale retenue (en mètres, défaut = 750)
#
# Retourne :
#   un data.frame avec lon / lat / depth / time / year / month / temp

run_nc_pipeline <- function(file_nc, max_depth = 750) {

  # ── [1/3] Lecture des coordonnées ───────────────────────────────────────────
  cat(">>> [1/3] Lecture du fichier NetCDF\n")
  nc    <- nc_open(file_nc)
  lon   <- ncvar_get(nc, "longitude")
  lat   <- ncvar_get(nc, "latitude")
  depth <- ncvar_get(nc, "depth")
  time  <- as.Date(ncvar_get(nc, "time"), origin = "1950-01-01")

  idx_z    <- which(depth <= max_depth)
  depth_sel <- depth[idx_z]
  n_z      <- length(depth_sel)
  cat("    Couches retenues (<= ", max_depth, " m) :", depth_sel, "\n")

  # ── [2/3] Lecture du tenseur ─────────────────────────────────────────────────
  cat(">>> [2/3] Lecture du tenseur TEMP\n")
  T_sel <- ncvar_get(
    nc, "TEMP",
    start = c(1, 1, min(idx_z), 1),
    count = c(length(lon), length(lat), n_z, length(time))
  )
  T_sel[is.nan(T_sel)] <- NA
  nc_close(nc)
  cat("    Dimensions T_sel :", dim(T_sel), "\n")   # lon x lat x depth x time

  # ── [3/3] Conversion en data.frame long ─────────────────────────────────────
  cat(">>> [3/3] Conversion en data.frame long\n")

  dimnames(T_sel) <- list(
    lon   = round(lon, 3),
    lat   = round(lat, 3),
    depth = depth_sel,
    time  = as.character(time)
  )

  df <- as.data.frame.table(T_sel, responseName = "temp") %>%
    mutate(
      lon   = as.numeric(as.character(lon)),
      lat   = as.numeric(as.character(lat)),
      depth = as.numeric(as.character(depth)),
      time  = as.Date(as.character(time)),
      year  = year(time),
      month = month(time)
    ) %>%
    dplyr::filter(!is.na(temp)) %>%
    dplyr::select(lon, lat, depth, time, year, month, temp)

  cat("\n✔ Pipeline terminé —", nrow(df), "observations,", ncol(df), "colonnes\n")
  cat("  Colonnes :", paste(names(df), collapse = " | "), "\n")

  df
}