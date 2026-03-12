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
    Species = as.factor(Species)
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
    Sex = droplevels(Sex)
  )

cat("\nDimensions before filtering NAs :", nrow(raw_data), "x", ncol(raw_data), "\n")
cat("Dimensions after filtering NAs  :", nrow(data), "x", ncol(data), "\n")
cat("Discarded individuals           :", nrow(raw_data) - nrow(data), "\n")
cat("\nNAs left :\n")
print(sapply(data, function(x) sum(is.na(x))))

# Freeing memory
rm(raw_data, raw_data_path)
gc()