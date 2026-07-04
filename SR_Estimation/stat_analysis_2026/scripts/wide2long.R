library(dplyr)
library(tidyr)
library(readr)
library(conflicted)

# --- Fonction de pivot ---
.wide_to_long <- function(filepath, variable = c("F", "N")) {
  variable <- match.arg(variable)
  df <- read_csv(filepath, show_col_types = FALSE)
  message("Colonnes disponibles : ", paste(names(df), collapse = ", "))
  age_cols <- grep(paste0("^", variable, "[0-9]+(plus)?$"), names(df), value = TRUE)
  if (length(age_cols) == 0) {
    stop("Aucune colonne correspondant au motif '", variable, "k' ou '", variable, "kplus' trouvée.")
  }
  message("Colonnes retenues    : ", paste(age_cols, collapse = ", "))

  result <- df %>%
    pivot_longer(cols = all_of(age_cols), names_to = "Age", values_to = variable) %>%
    mutate(Age = sub(paste0("^", variable), "", Age))

  # Cas particulier Trisopterus : Year décimale
  if (any(result$Year != floor(result$Year), na.rm = TRUE)) {
    result <- result %>%
      mutate(
        Year_numeric = Year,
        Quarter      = as.integer(round((Year - floor(Year)) / 0.25)) + 1L,
        Year         = as.integer(floor(Year))
      )
  }

  result %>% select(Year, any_of(c("Year_numeric", "Quarter")), Age, all_of(variable))
}

# --- Parsing du nom de fichier ---
.parse_filename <- function(filename) {
  base  <- tools::file_path_sans_ext(filename)
  parts <- strsplit(base, "_")[[1]]
  type    <- parts[1]
  species <- paste(tail(parts, 2), collapse = "_")
  area    <- paste(parts[2:(length(parts) - 2)], collapse = "_")
  if (grepl("^FishingMortality", type)) {
    prefix <- "FM"; variable <- "F"
  } else if (grepl("^PopulationNumbers", type)) {
    prefix <- "PopNb"; variable <- "N"
  } else {
    return(NULL)
  }
  df_name <- paste0(prefix, "_", species, "_", gsub("^Subdivisions?", "", area))
  list(variable = variable, df_name = df_name)
}

# --- Traitement du dossier ---
process_folder <- function(input_path, output_path, envir = .GlobalEnv) {
  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE)
    message("Dossier de sortie créé : ", output_path)
  }
  csv_files <- list.files(input_path, pattern = "\\.csv$", full.names = FALSE)
  if (length(csv_files) == 0) stop("Aucun fichier CSV trouvé dans le dossier.")
  for (f in csv_files) {
    message("\n>>> Traitement : ", f)
    meta <- .parse_filename(f)
    if (is.null(meta)) { message("    Fichier ignoré."); next }
    df <- tryCatch(
      .wide_to_long(file.path(input_path, f), meta$variable),
      error = function(e) { message("    Erreur : ", e$message); NULL }
    )
    if (!is.null(df)) {
      assign(meta$df_name, df, envir = envir)
      message("    Dataframe créé : ", meta$df_name)
      write_csv(df, file.path(output_path, paste0(meta$df_name, ".csv")))
      message("    Fichier écrit  : ", meta$df_name, ".csv")
    }
  }
}