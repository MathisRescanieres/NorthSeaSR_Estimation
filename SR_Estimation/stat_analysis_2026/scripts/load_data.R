library(conflicted)
library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)

# Pathway to the data set (TO BE MODIFIED IF NECESSERY)
raw_data_path <- file.path("trawling_data/SMALK_2022-01-06 11_47_25.csv")

# Importation of the data set
raw_data <- read.csv("../data/trawling_data/DATRAS-NS-IBTS.csv",
                 header = TRUE,
                 sep = ",",
                 stringsAsFactors = FALSE)

factor_cols <- c(
  "reference_id",
  "original_binomial_name",   # espèce = variable clé
  "original_age_unit",        # unité d'âge (probablement "year" partout)
  "original_body_size_type",  # type de mesure (TL, SL...)
  "original_body_size_unit",  # unité (mm, cm...)
  "original_body_mass_type",
  "original_body_mass_unit",
  "maturity_stage_scale",     # échelle de maturité
  "maturity_stage",           # stade de maturité
  "sexing_method_phenotypic", # méthode de sexage
  "capture_method",           # méthode de capture
  "biological_scale",         # échelle biologique
  "location"                  # zone géographique
)

raw_data[factor_cols] <- lapply(raw_data[factor_cols], factor)

data <- raw_data[!is.na(raw_data$number_female) & !is.na(raw_data$number_male), ]

# # Pretraitement of the data set
# data <- raw_data %>%
#   select(-PlusGr, -Survey, -IndWgt, -DateofCalculation) %>%
#   mutate(
#     Sex = as.factor(Sex),
#     Maturity = as.factor(Maturity),
#     Species = as.factor(Species),
#     Area = as.factor(Area)
#   ) %>%
#   filter(
#     !is.na(Age),
#     !is.na(Sex),
#     !is.na(CANoAtLngt)
#   ) %>%
#   mutate(
#     Numeric_sex = ifelse(Sex == "M", 1L, 0L),
#     Age = as.double(Age),
#     Year = as.double(Year),
#     Cohorte = Year - Age,
#     Species = droplevels(Species),
#     Maturity = droplevels(Maturity),
#     Sex = droplevels(Sex)) %>%
#   mutate(Sex = na_if(as.character(Sex), "U"),
#          Sex = as.factor(Sex),
#          Sex = droplevels(Sex)) %>%
#   filter(!is.na(Sex))

cat("\nDimensions before filtering NAs on sex data   :", nrow(raw_data), "x", ncol(raw_data), "\n")
cat("Dimensions after filtering NAs on sex data    :", nrow(data), "x", ncol(data), "\n")
cat("Discarded individuals                         :", nrow(raw_data) - nrow(data), "\n")

# Freeing memory
rm(raw_data, raw_data_path)
gc()