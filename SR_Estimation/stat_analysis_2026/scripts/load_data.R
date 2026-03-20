library(tidyverse)

# Pathway to the data set (TO BE MODIFIED IF NECESSERY)
raw_data_path <- file.path("SMALK_2022-01-06 11_47_25.csv")

# Importation of the data set
raw_data <- read.csv(raw_data_path,
                 header = TRUE,
                 sep = ";",
                 na.strings = c("NA", "-9", ""),
                 stringsAsFactors = FALSE)

# Pretraitement of the data set
data <- raw_data %>%
  select(-PlusGr, -Survey, -IndWgt, -DateofCalculation) %>%
  mutate(
    Sex = as.factor(Sex),
    Maturity = as.factor(Maturity),
    Species = as.factor(Species),
    Area = as.factor(Area)
  ) %>%
  filter(
    !is.na(Age),
    !is.na(Sex),
    !is.na(CANoAtLngt)
  ) %>%
  mutate(
    Numeric_sex = ifelse(Sex == "M", 1L, 0L),
    Age = as.double(Age),
    Year = as.double(Year),
    Cohorte = Year - Age,
    Species = droplevels(Species),
    Maturity = droplevels(Maturity),
    Sex = droplevels(Sex)) %>%
  mutate(Sex = na_if(as.character(Sex), "U"),
         Sex = as.factor(Sex),
         Sex = droplevels(Sex)) %>%
  filter(!is.na(Sex))

data <- data %>%
  mutate(
    Numeric_maturity = case_when(
      # Simple digit system
      Maturity == "1"  ~ 0L,  # Immature
      Maturity == "2"  ~ 1L,  # Mature
      Maturity == "3"  ~ 1L,
      Maturity == "4"  ~ 1L,
      Maturity == "5"  ~ 1L,
      Maturity == "6"  ~ 1L,
      # ICES system
      Maturity == "61" ~ 0L,  # Immature
      Maturity == "62" ~ 1L,  # Mature
      Maturity == "63" ~ 1L,
      Maturity == "64" ~ 1L,
      Maturity == "65" ~ 1L,
      Maturity == "66" ~ 1L,
      # Alphanumeric system
      Maturity == "A"  ~ 0L,  # Immature
      Maturity == "B"  ~ 1L,  # Mature
      Maturity == "Ba" ~ 1L,
      Maturity == "Bb" ~ 1L,
      Maturity == "C"  ~ 1L,
      Maturity == "Ca" ~ 1L,
      Maturity == "Cb" ~ 1L,
      Maturity == "D"  ~ 1L,
      Maturity == "Da" ~ 1L,
      Maturity == "E"  ~ 1L,
      Maturity == "F"  ~ 1L,
      Maturity == "I"  ~ 0L,  # Immature
      Maturity == "M"  ~ 1L,  # Mature
    )
  )

cat("\nDimensions before filtering NAs :", nrow(raw_data), "x", ncol(raw_data), "\n")
cat("Dimensions after filtering NAs  :", nrow(data), "x", ncol(data), "\n")
cat("Discarded individuals           :", nrow(raw_data) - nrow(data), "\n")
cat("\nNAs left :\n")
print(sapply(data, function(x) sum(is.na(x))))

# Freeing memory
rm(raw_data, raw_data_path)
gc()