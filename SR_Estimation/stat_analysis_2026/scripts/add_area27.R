library(sf)
library(dplyr)

add_area27 <- function(data, ices_shp_path, cod_shp_path) {

  sf::sf_use_s2(FALSE)

  ices_areas <- sf::st_read(ices_shp_path) %>%
    sf::st_make_valid() %>%
    sf::st_transform(crs = 4326) %>%
    dplyr::mutate(Area_27 = tolower(Area_27))

  cod_areas <- sf::st_read(cod_shp_path) %>%
    sf::st_make_valid() %>%
    sf::st_transform(crs = 4326) %>%
    dplyr::rename(Area_27 = Substock)

  data_sf <- sf::st_as_sf(
    data %>% dplyr::mutate(.row_id = dplyr::row_number()),
    coords = c("Longitude", "Latitude"),
    crs    = 4326,
    remove = FALSE
  )

  data_cod <- data_sf %>%
    dplyr::filter(Species == "Gadus morhua") %>%
    sf::st_join(cod_areas["Area_27"], join = sf::st_within) %>%
    sf::st_drop_geometry()

  data_other <- data_sf %>%
    dplyr::filter(Species != "Gadus morhua") %>%
    sf::st_join(ices_areas["Area_27"], join = sf::st_within) %>%
    sf::st_drop_geometry()

  result <- dplyr::bind_rows(data_cod, data_other) %>%
    dplyr::distinct(.row_id, .keep_all = TRUE) %>%
    dplyr::select(-.row_id)

  n_na <- sum(is.na(result$Area_27))
  if (n_na > 0) message("Attention : ", n_na, " poissons hors zone (Area_27 = NA)")

  result
}
