library(ncdf4)
library(tidyverse)
library(lubridate)

# ==============================================================================
# run_nc_pipeline()
# ------------------------------------------------------------------------------
# Lit un dossier de fichiers NetCDF mensuels CORA OA (1 fichier = 1 mois),
# extrait une zone géographique et une profondeur max, et retourne un
# data.frame long : lon | lat | depth | time | year | month | temp
#
# Robuste aux changements de grille entre fichiers.
#
# Paramètres :
#   data_dir  : dossier contenant les fichiers fld_TEMP.nc
#   lon_min/max, lat_min/max : bornes de la zone d'extraction
#   max_depth : profondeur max retenue (m)
# ==============================================================================

run_nc_pipeline <- function(
    data_dir,
    lon_min = -4.5, lon_max = 13.5,
    lat_min = 48.5, lat_max = 62.5,
    max_depth = 750
) {
  nc_files <- list.files(data_dir, pattern = "fld_TEMP\\.nc$",
                         full.names = TRUE) |> sort()
  cat("Fichiers trouvés :", length(nc_files), "\n")

  result <- vector("list", length(nc_files))

  for (i in seq_along(nc_files)) {
    if (i %% 100 == 0 || i == 1)
      cat(sprintf("  [%d/%d] %s\n", i, length(nc_files), basename(nc_files[i])))

    result[[i]] <- tryCatch({
      nc    <- nc_open(nc_files[i])
      lon   <- ncvar_get(nc, "longitude")
      lat   <- ncvar_get(nc, "latitude")
      depth <- ncvar_get(nc, "depth")
      time  <- as.Date(ncvar_get(nc, "time"), origin = "1950-01-01")

      ilon <- which(lon   >= lon_min & lon   <= lon_max)
      ilat <- which(lat   >= lat_min & lat   <= lat_max)
      idep <- which(depth <= max_depth)

      T_sel <- ncvar_get(nc, "TEMP",
        start = c(ilon[1], ilat[1], idep[1], 1),
        count = c(length(ilon), length(ilat), length(idep), 1))
      nc_close(nc)

      T_sel[is.nan(T_sel)] <- NA

      dimnames(T_sel) <- list(
        lon   = round(lon[ilon], 3),
        lat   = round(lat[ilat], 3),
        depth = depth[idep]
      )

      as.data.frame.table(T_sel, responseName = "temp") |>
        mutate(
          lon   = as.numeric(as.character(lon)),
          lat   = as.numeric(as.character(lat)),
          depth = as.numeric(as.character(depth)),
          time  = time,
          year  = year(time),
          month = month(time)
        ) |>
        filter(!is.na(temp)) |>
        select(lon, lat, depth, time, year, month, temp)

    }, error = function(e) {
      cat("ERREUR :", basename(nc_files[i]), "-", conditionMessage(e), "\n")
      NULL
    })
  }

  df <- bind_rows(result)
  cat("\n✔ Pipeline terminé —", nrow(df), "observations,", ncol(df), "colonnes\n")
  cat("  Période :", as.character(min(df$time)), "→", as.character(max(df$time)), "\n")
  cat("  Points lon/lat uniques :", n_distinct(paste(df$lon, df$lat)), "\n")
  df
}