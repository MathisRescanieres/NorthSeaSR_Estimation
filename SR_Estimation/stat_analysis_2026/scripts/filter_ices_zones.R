library(sf)
library(dplyr)

within_ices_zones <- function(data, ices_shp_path) {

  sf::sf_use_s2(FALSE)

  ices_areas <- sf::st_read(ices_shp_path) %>%
    sf::st_make_valid() %>%
    sf::st_transform(crs = 4326)

  data_sf <- sf::st_as_sf(
    data %>% dplyr::mutate(.row_id = dplyr::row_number()),
    coords = c("Longitude", "Latitude"),
    crs    = 4326,
    remove = FALSE
  )

  data_checked <- sf::st_join(data_sf, ices_areas["Area_27"], join = sf::st_within) %>%
    sf::st_drop_geometry()

  data_hors_zone <- data_checked %>%
    dplyr::filter(is.na(Area_27))

  data_in_zone <- data_checked %>%
    dplyr::filter(!is.na(Area_27)) %>%
    dplyr::select(-.row_id, -Area_27)

  message(nrow(data_hors_zone), " individus supprimés (hors zone ICES).")
  message(nrow(data_in_zone),   " individus conservés.")

  invisible(data_in_zone)
}
