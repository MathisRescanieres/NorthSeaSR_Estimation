library(sf)
library(dplyr)
library(purrr)
library(conflicted)

build_cod_substocks <- function(ices_areas, statrect_path, output_shp_path) {

  # --- 1. Charger la table StatRect ---
  grid <- read.csv(statrect_path, stringsAsFactors = FALSE)

  # --- 2. Coordonnées des rectangles ---
  ices_lon <- c(
    "E6"=-4,"E7"=-3,"E8"=-2,"E9"=-1,
    "F0"=0, "F1"=1, "F2"=2, "F3"=3,
    "F4"=4, "F5"=5, "F6"=6, "F7"=7,
    "F8"=8, "F9"=9, "G0"=10,"G1"=11,"G2"=12
  )

  grid_coords_plot <- grid %>%
    dplyr::mutate(
      col  = sub("^[0-9]+", "", StatRect),
      row  = as.numeric(sub("[A-Z][0-9]$", "", StatRect)),
      lon  = ices_lon[col],
      lat  = row * 0.5 + 35.5,
      xmin = lon,
      xmax = lon + 1,
      ymin = lat,
      ymax = lat + 0.5
    ) %>%
    dplyr::filter(!is.na(Substock))

  # --- 3. Convertir en polygones sf ---
  grid_sf <- grid_coords_plot %>%
    dplyr::filter(!is.na(lon) & !is.na(lat)) %>%
    dplyr::mutate(geometry = purrr::pmap(
      list(xmin, xmax, ymin, ymax),
      function(x1, x2, y1, y2) {
        sf::st_polygon(list(matrix(
          c(x1, y1, x2, y1, x2, y2, x1, y2, x1, y1),
          ncol = 2, byrow = TRUE
        )))
      }
    )) %>%
    sf::st_as_sf(crs = 4326)

  # --- 4. Intersection avec les zones ICES ---
  ices_stock_area <- ices_areas %>%
    dplyr::filter(Area_27 %in% c("4.a", "4.b", "4.c", "7.d", "3.a.20")) %>%
    sf::st_intersection(grid_sf %>% dplyr::select(StatRect, Substock)) %>%
    dplyr::group_by(Substock) %>%
    dplyr::summarise(geometry = sf::st_union(geometry)) %>%
    dplyr::ungroup()

  # --- 4.bis. Ajouter 6.a à Northwestern ---
  zone_6a <- ices_areas %>%
    dplyr::filter(Area_27 == "6.a") %>%
    dplyr::summarise(geometry = sf::st_union(geometry))

  ices_stock_area$geometry[ices_stock_area$Substock == "Northwestern"] <- sf::st_union(
    ices_stock_area$geometry[ices_stock_area$Substock == "Northwestern"] %>% sf::st_buffer(0.001),
    zone_6a$geometry %>% sf::st_buffer(0.001)
  ) %>%
    sf::st_union() %>%
    sf::st_make_valid()

  # --- 4.ter. Ajouter 3.a.21, 3.b.23, 3.c.22 à Viking ---
  zone_extra <- ices_areas %>%
    dplyr::filter(Area_27 %in% c("3.a.21", "3.b.23", "3.c.22")) %>%
    dplyr::summarise(geometry = sf::st_union(geometry))

  ices_stock_area$geometry[ices_stock_area$Substock == "Viking"] <- sf::st_union(
    ices_stock_area$geometry[ices_stock_area$Substock == "Viking"] %>% sf::st_buffer(0.001),
    zone_extra$geometry %>% sf::st_buffer(0.001)
  ) %>%
    sf::st_union() %>%
    sf::st_make_valid()

  # --- 5. Exporter ---
  sf::st_write(ices_stock_area, output_shp_path, delete_dsn = TRUE)
  message("Shapefile écrit : ", output_shp_path)

  invisible(ices_stock_area)
}
