library(sf)
library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(conflicted)

species_with_stock_assessment <- c(
  "Gadus morhua",
  "Pleuronectes platessa",
  "Trisopterus esmarkii",
  "Melanogrammus aeglefinus",
  "Merlangius merlangus",
  "Pollachius virens"
)

species_fcum_ncum <- c(
  "Trisopterus esmarkii",
  "Melanogrammus aeglefinus",
  "Merlangius merlangus"
)

# --- Fonctions internes ---

.parse_area_token <- function(area_token) {
  token <- gsub("^Subdivisions?", "", area_token)
  if (!grepl("\\.", token)) return(token)
  tolower(strsplit(token, "&")[[1]])
}

.parse_entry <- function(name, type) {
  parts   <- strsplit(name, "_")[[1]]
  zone    <- parts[length(parts)]
  species <- paste(parts[2:(length(parts) - 1)], collapse = " ")
  zones   <- .parse_area_token(zone)
  data.frame(
    df_name = name,
    type    = type,
    species = species,
    Area_27 = zones,
    stringsAsFactors = FALSE
  )
}

.build_lookup <- function(tables_path) {
  csv_files   <- list.files(tables_path, pattern = "\\.csv$", full.names = FALSE)
  names_all   <- tools::file_path_sans_ext(csv_files)
  fm_names    <- names_all[grepl("^FM_",    names_all)]
  popnb_names <- names_all[grepl("^PopNb_", names_all)]
  dplyr::bind_rows(
    lapply(fm_names,    .parse_entry, type = "FM"),
    lapply(popnb_names, .parse_entry, type = "PopNb")
  )
}

.load_tables <- function(tables_path) {
  csv_files     <- list.files(tables_path, pattern = "\\.csv$", full.names = TRUE)
  tables        <- lapply(csv_files, read_csv, show_col_types = FALSE)
  names(tables) <- tools::file_path_sans_ext(basename(csv_files))

  tables <- purrr::imap(tables, function(table, name) {
    if (!grepl("^PopNb_", name)) return(table)

    facteur <- if (grepl("Trisopterus", name)) 1e6 else 1e3

    table %>%
      dplyr::mutate(N = N * facteur)
  })

  tables
}

.prepare_ref <- function(table) {
  table %>%
    dplyr::select(-dplyr::any_of("Year_numeric")) %>%
    dplyr::mutate(Age_num = as.integer(sub("plus$", "", Age))) %>%
    dplyr::select(-Age) %>%
    dplyr::rename(Age = Age_num)
}

# Fusionne une table FM et une table PopNb sur les colonnes communes.
# Sert de référence pour la table Fcum + Ncum.
.prepare_combined_ref <- function(fm_table, popnb_table) {
  fm  <- .prepare_ref(fm_table)
  pop <- .prepare_ref(popnb_table)
  join_cols <- base::intersect(c("Year", "Quarter", "Age"), base::intersect(names(fm), names(pop)))
  dplyr::inner_join(fm, pop, by = join_cols)
}

# --- Calcul cumulatif générique, granularité annuelle ---
# value_cols accepte une ou plusieurs colonnes ("F", ou c("F", "N")).

.compute_cum_annual <- function(fish_group, ref, max_age, value_cols) {

  fish_group <- fish_group %>%
    dplyr::mutate(
      .fish_id = dplyr::row_number(),
      Age_cap  = pmin(Age, max_age)
    )

  trajectories <- fish_group %>%
    dplyr::select(.fish_id, Year_join, Age, Age_cap) %>%
    dplyr::mutate(
      cohort  = Year_join - Age,
      age_seq = purrr::map(Age_cap, ~ seq(0L, .x, by = 1L))
    ) %>%
    tidyr::unnest(age_seq) %>%
    dplyr::rename(age_life = age_seq) %>%
    dplyr::mutate(
      year_life       = cohort + age_life,
      age_lookup      = pmin(age_life, max_age),
      ages_from_catch = Age_cap - age_life
    )

  ref_lookup <- ref %>%
    dplyr::rename(year_life = Year_join, age_lookup = Age)

  trajectories <- trajectories %>%
    dplyr::left_join(ref_lookup, by = c("year_life", "age_lookup"))

  cum <- trajectories %>%
    dplyr::group_by(.fish_id) %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(value_cols),
        list(
          `1`  = ~ sum(.x[ages_from_catch <= 0], na.rm = TRUE),
          `3`  = ~ sum(.x[ages_from_catch <= 2], na.rm = TRUE),
          `5`  = ~ sum(.x[ages_from_catch <= 4], na.rm = TRUE),
          `10` = ~ sum(.x[ages_from_catch <= 9], na.rm = TRUE),
          tot  = ~ sum(.x, na.rm = TRUE)
        ),
        .names = "{.col}cum_{.fn}"
      ),
      .groups = "drop"
    )

  fish_group %>%
    dplyr::left_join(cum, by = ".fish_id") %>%
    dplyr::select(-.fish_id, -Age_cap)
}

# --- Calcul cumulatif générique, granularité trimestrielle (Trisopterus) ---

.compute_cum_quarterly <- function(fish_group, ref, max_age, value_cols) {

  fish_group <- fish_group %>%
    dplyr::mutate(
      .fish_id    = dplyr::row_number(),
      Age_cap     = pmin(Age, max_age),
      cohort_year = Year - Age_cap,
      q_catch_abs = Year * 4L + (Quarter - 1L),
      q_birth_abs = cohort_year * 4L
    )

  trajectories <- fish_group %>%
    dplyr::select(.fish_id, q_catch_abs, q_birth_abs) %>%
    dplyr::mutate(
      q_seq = purrr::map2(q_birth_abs, q_catch_abs, ~ seq(.x, .y, by = 1L))
    ) %>%
    tidyr::unnest(q_seq) %>%
    dplyr::rename(q_abs = q_seq) %>%
    dplyr::mutate(
      year_life    = q_abs %/% 4L,
      quarter_life = q_abs %% 4L + 1L,
      age_life     = (q_abs - q_birth_abs) %/% 4L,
      age_lookup   = pmin(age_life, max_age),
      q_from_catch = q_catch_abs - q_abs
    )

  ref_lookup <- ref %>%
    dplyr::rename(
      year_life    = Year,
      quarter_life = Quarter,
      age_lookup   = Age
    )

  trajectories <- trajectories %>%
    dplyr::left_join(ref_lookup, by = c("year_life", "quarter_life", "age_lookup"))

  cum <- trajectories %>%
    dplyr::group_by(.fish_id) %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(value_cols),
        list(
          `1`  = ~ sum(.x[q_from_catch <= 0], na.rm = TRUE),
          `3`  = ~ sum(.x[q_from_catch <= 2], na.rm = TRUE),
          `5`  = ~ sum(.x[q_from_catch <= 4], na.rm = TRUE),
          `10` = ~ sum(.x[q_from_catch <= 9], na.rm = TRUE),
          tot  = ~ sum(.x, na.rm = TRUE)
        ),
        .names = "{.col}cum_{.fn}"
      ),
      .groups = "drop"
    )

  fish_group %>%
    dplyr::left_join(cum, by = ".fish_id") %>%
    dplyr::select(-.fish_id, -Age_cap, -cohort_year, -q_catch_abs, -q_birth_abs)
}

# --- Table 1 : Fcum uniquement, 6 espèces avec stock assessment ---

.join_fcum_table <- function(fish_ices, lookup, tables, species_list) {

  fm_lookup <- lookup %>%
    dplyr::filter(type == "FM", species %in% species_list) %>%
    dplyr::select(species, Area_27, df_name) %>%
    dplyr::rename(fm_df = df_name)

  fish_ref <- fish_ices %>%
    dplyr::filter(Species %in% species_list) %>%
    dplyr::left_join(fm_lookup, by = c("Species" = "species", "Area_27"))

  n_no_fm <- sum(is.na(fish_ref$fm_df))
  if (n_no_fm > 0) message("Attention : ", n_no_fm, " poissons sans FM associé (table Fcum)")

  fish_ref %>%
    dplyr::group_by(fm_df) %>%
    dplyr::group_modify(~ {
      nm <- unique(.y$fm_df)
      if (is.na(nm)) return(.x)
      ref     <- .prepare_ref(tables[[nm]])
      max_age <- max(ref$Age, na.rm = TRUE)
      is_tris <- "Quarter" %in% names(ref)

      if (is_tris) {
        .compute_cum_quarterly(.x, ref, max_age, value_cols = "F")
      } else {
        ref <- ref %>% dplyr::rename(Year_join = Year)
        .compute_cum_annual(.x, ref, max_age, value_cols = "F")
      }
    }) %>%
    dplyr::ungroup() %>%
    dplyr::select(-fm_df, -Year_join)
}

# --- Table 2 : Fcum ET Ncum, 3 espèces communes ---

.join_fcum_ncum_table <- function(fish_ices, lookup, tables, species_list) {

  fm_lookup <- lookup %>%
    dplyr::filter(type == "FM", species %in% species_list) %>%
    dplyr::select(species, Area_27, df_name) %>%
    dplyr::rename(fm_df = df_name)

  popnb_lookup <- lookup %>%
    dplyr::filter(type == "PopNb", species %in% species_list) %>%
    dplyr::select(species, Area_27, df_name) %>%
    dplyr::rename(popnb_df = df_name)

  fish_ref <- fish_ices %>%
    dplyr::filter(Species %in% species_list) %>%
    dplyr::left_join(fm_lookup,    by = c("Species" = "species", "Area_27")) %>%
    dplyr::left_join(popnb_lookup, by = c("Species" = "species", "Area_27"))

  n_no_fm    <- sum(is.na(fish_ref$fm_df))
  n_no_popnb <- sum(is.na(fish_ref$popnb_df))
  if (n_no_fm    > 0) message("Attention : ", n_no_fm,    " poissons sans FM associé (table Fcum+Ncum)")
  if (n_no_popnb > 0) message("Attention : ", n_no_popnb, " poissons sans PopNb associé (table Fcum+Ncum)")

  fish_ref %>%
    dplyr::group_by(fm_df, popnb_df) %>%
    dplyr::group_modify(~ {
      fm_nm    <- unique(.y$fm_df)
      popnb_nm <- unique(.y$popnb_df)
      if (is.na(fm_nm) || is.na(popnb_nm)) return(.x)

      ref     <- .prepare_combined_ref(tables[[fm_nm]], tables[[popnb_nm]])
      max_age <- max(ref$Age, na.rm = TRUE)
      is_tris <- "Quarter" %in% names(ref)

      if (is_tris) {
        .compute_cum_quarterly(.x, ref, max_age, value_cols = c("F", "N"))
      } else {
        ref <- ref %>% dplyr::rename(Year_join = Year)
        .compute_cum_annual(.x, ref, max_age, value_cols = c("F", "N"))
      }
    }) %>%
    dplyr::ungroup() %>%
    dplyr::select(-fm_df, -popnb_df, -Year_join)
}

# --- Fonction principale ---

join_stock <- function(data, ices_shp_path, cod_shp_path, tables_path,
                        output_path_fcum, output_path_fcum_ncum) {

  sf::sf_use_s2(FALSE)

  ices_areas <- sf::st_read(ices_shp_path) %>%
    sf::st_make_valid() %>%
    sf::st_transform(crs = 4326) %>%
    dplyr::mutate(Area_27 = tolower(Area_27))

  cod_areas <- sf::st_read(cod_shp_path) %>%
    sf::st_make_valid() %>%
    sf::st_transform(crs = 4326) %>%
    dplyr::rename(Area_27 = Substock)

  tables <- .load_tables(tables_path)
  message(length(tables), " tables chargées depuis ", tables_path)

  lookup <- .build_lookup(tables_path)
  message("Lookup table construite : ", nrow(lookup), " entrées")

  fish_sf <- sf::st_as_sf(
    data %>% dplyr::mutate(.row_id = dplyr::row_number()),
    coords = c("Longitude", "Latitude"),
    crs    = 4326,
    remove = FALSE
  )

  fish_cod <- fish_sf %>%
    dplyr::filter(Species == "Gadus morhua") %>%
    sf::st_join(cod_areas["Area_27"], join = sf::st_within) %>%
    sf::st_drop_geometry()

  fish_other <- fish_sf %>%
    dplyr::filter(Species != "Gadus morhua") %>%
    sf::st_join(ices_areas["Area_27"], join = sf::st_within) %>%
    sf::st_drop_geometry()

  fish_ices <- dplyr::bind_rows(fish_cod, fish_other) %>%
    dplyr::distinct(.row_id, .keep_all = TRUE)

  n_na <- sum(is.na(fish_ices$Area_27))
  if (n_na > 0) message("Attention : ", n_na, " poissons hors zone (Area_27 = NA)")

  fish_ices <- fish_ices %>%
    dplyr::mutate(Year_join = dplyr::case_when(
      Species == "Trisopterus esmarkii" ~ NA_real_,
      TRUE                              ~ as.numeric(Year)
    ))

  # Table 1 : Fcum, 6 espèces avec stock assessment
  data_fcum <- .join_fcum_table(fish_ices, lookup, tables, species_with_stock_assessment)
  message("Table Fcum construite : ", nrow(data_fcum), " lignes")

  # Table 2 : Fcum + Ncum, 3 espèces communes
  data_fcum_ncum <- .join_fcum_ncum_table(fish_ices, lookup, tables, species_fcum_ncum)
  message("Table Fcum+Ncum construite : ", nrow(data_fcum_ncum), " lignes")

  data_fcum <- data_fcum %>%
    dplyr::select(-.row_id) %>%
    dplyr::mutate(dplyr::across(where(is.matrix), as.numeric))

  data_fcum_ncum <- data_fcum_ncum %>%
    dplyr::select(-.row_id) %>%
    dplyr::mutate(dplyr::across(where(is.matrix), as.numeric))

  readr::write_csv(data_fcum,      output_path_fcum)
  readr::write_csv(data_fcum_ncum, output_path_fcum_ncum)

  message("Fichiers écrits : ", output_path_fcum, " et ", output_path_fcum_ncum)

  invisible(list(fcum = data_fcum, fcum_ncum = data_fcum_ncum))
}