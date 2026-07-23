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
  "Pollachius virens",
  "Clupea harengus",
  "Sprattus sprattus",
  "Scomber scombrus"
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
  fm_names    <- names_all[grepl("^FM_", names_all)]
  .parse_entry_fm <- function(nm) .parse_entry(nm, type = "FM")
  dplyr::bind_rows(lapply(fm_names, .parse_entry_fm))
}

.load_tables <- function(tables_path) {
  csv_files     <- list.files(tables_path, pattern = "\\.csv$", full.names = TRUE)
  csv_files     <- csv_files[grepl("^FM_", basename(tools::file_path_sans_ext(csv_files)))]
  tables        <- lapply(csv_files, read_csv, show_col_types = FALSE)
  names(tables) <- tools::file_path_sans_ext(basename(csv_files))
  tables
}

.prepare_ref <- function(table) {
  table %>%
    dplyr::select(-dplyr::any_of("Year_numeric")) %>%
    dplyr::mutate(Age_num = as.integer(sub("plus$", "", Age))) %>%
    dplyr::select(-Age) %>%
    dplyr::rename(Age = Age_num)
}

# --- Helper commun : somme "stricte" qui devient NA si un seul trou existe ---

.sum_strict <- function(x) {
  if (anyNA(x)) NA_real_ else sum(x)
}

# --- Calcul cumulatif générique, granularité annuelle ---

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

  # Diagnostic : combien de points de trajectoire n'ont trouve aucune
  # valeur F correspondante dans la table de reference
  n_missing <- sum(is.na(trajectories[[value_cols]]))
  if (n_missing > 0) {
    message("    -> ", n_missing, " points de trajectoire sans F correspondant (annuel), ",
            "propages en NA dans les cumuls concernes")
  }

  cum <- trajectories %>%
    dplyr::group_by(.fish_id) %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(value_cols),
        list(
          `1`  = ~ .sum_strict(.x[ages_from_catch <= 0]),
          `3`  = ~ .sum_strict(.x[ages_from_catch <= 2]),
          `5`  = ~ .sum_strict(.x[ages_from_catch <= 4]),
          `10` = ~ .sum_strict(.x[ages_from_catch <= 9]),
          tot  = ~ .sum_strict(.x)
        ),
        .names = "{.col}cum_{.fn}"
      ),
      .groups = "drop"
    )

  fish_group %>%
    dplyr::left_join(cum, by = ".fish_id") %>%
    dplyr::select(-.fish_id, -Age_cap)
}

# --- Calcul cumulatif générique, granularité trimestrielle (Trisopterus, Sprattus) ---

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

  n_missing <- sum(is.na(trajectories[[value_cols]]))
  if (n_missing > 0) {
    message("    -> ", n_missing, " points de trajectoire sans F correspondant (trimestriel), ",
            "propages en NA dans les cumuls concernes")
  }

  cum <- trajectories %>%
    dplyr::group_by(.fish_id) %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(value_cols),
        list(
          `1`  = ~ .sum_strict(.x[q_from_catch <= 0]),
          `3`  = ~ .sum_strict(.x[q_from_catch <= 2]),
          `5`  = ~ .sum_strict(.x[q_from_catch <= 4]),
          `10` = ~ .sum_strict(.x[q_from_catch <= 9]),
          tot  = ~ .sum_strict(.x)
        ),
        .names = "{.col}cum_{.fn}"
      ),
      .groups = "drop"
    )

  fish_group %>%
    dplyr::left_join(cum, by = ".fish_id") %>%
    dplyr::select(-.fish_id, -Age_cap, -cohort_year, -q_catch_abs, -q_birth_abs)
}

# --- Table Fcum, especes avec stock assessment ---

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

  result <- fish_ref %>%
    dplyr::group_by(fm_df) %>%
    dplyr::group_modify(~ {
      nm <- unique(.y$fm_df)
      if (is.na(nm)) return(.x)
      message("  Traitement du groupe : ", nm, " (", nrow(.x), " poissons)")
      ref     <- .prepare_ref(tables[[nm]])
      max_age <- max(ref$Age, na.rm = TRUE)
      is_seasonal <- "Quarter" %in% names(ref)

      if (is_seasonal) {
        .compute_cum_quarterly(.x, ref, max_age, value_cols = "F")
      } else {
        ref <- ref %>% dplyr::rename(Year_join = Year)
        .compute_cum_annual(.x, ref, max_age, value_cols = "F")
      }
    }) %>%
    dplyr::ungroup() %>%
    dplyr::select(-fm_df, -Year_join)

  # Diagnostic final : nombre de NA par colonne cumulative et par espece
  cum_cols <- grep("^Fcum_", names(result), value = TRUE)
  if (length(cum_cols) > 0) {
    na_summary <- result %>%
      dplyr::group_by(Species) %>%
      dplyr::summarise(dplyr::across(dplyr::all_of(cum_cols), ~ sum(is.na(.x))), .groups = "drop")
    message("\n=== Recapitulatif des NA dans les colonnes Fcum, par espece ===")
    print(na_summary)
  }

  result
}

# --- Fonction principale ---

join_stock <- function(data, ices_shp_path, cod_shp_path, tables_path,
                        output_path_fcum) {

  if ("Area_27" %in% names(data)) {

    message("Colonne Area_27 deja presente dans data, jointure spatiale ignoree.")
    fish_ices <- data %>% dplyr::mutate(.row_id = dplyr::row_number())

  } else {

    sf::sf_use_s2(FALSE)

    ices_areas <- sf::st_read(ices_shp_path) %>%
      sf::st_make_valid() %>%
      sf::st_transform(crs = 4326) %>%
      dplyr::mutate(Area_27 = tolower(Area_27))

    cod_areas <- sf::st_read(cod_shp_path) %>%
      sf::st_make_valid() %>%
      sf::st_transform(crs = 4326) %>%
      dplyr::rename(Area_27 = Substock)

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

  }

  n_na <- sum(is.na(fish_ices$Area_27))
  if (n_na > 0) message("Attention : ", n_na, " poissons hors zone (Area_27 = NA)")

  tables <- .load_tables(tables_path)
  message(length(tables), " tables FM chargées depuis ", paste(tables_path, collapse = " et "))

  lookup <- .build_lookup(tables_path)
  message("Lookup table construite : ", nrow(lookup), " entrées")

  fish_ices <- fish_ices %>%
    dplyr::mutate(Year_join = dplyr::case_when(
      Species %in% c("Trisopterus esmarkii", "Sprattus sprattus") ~ NA_real_,
      TRUE                                                         ~ as.numeric(Year)
    ))

  # Table Fcum, especes avec stock assessment
  data_fcum <- .join_fcum_table(fish_ices, lookup, tables, species_with_stock_assessment)
  message("Table Fcum construite : ", nrow(data_fcum), " lignes")

  data_fcum <- data_fcum %>%
    dplyr::select(-.row_id) %>%
    dplyr::mutate(dplyr::across(where(is.matrix), as.numeric))

  readr::write_csv(data_fcum, output_path_fcum)

  message("Fichier écrit : ", output_path_fcum)

  invisible(data_fcum)
}