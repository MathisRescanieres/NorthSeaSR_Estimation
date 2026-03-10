
setwd("E:\\github\\travail_github\\Stage_M2_Calzan")

tableau_AMO_detendance <- read.csv("AMO unsmoothed, detrended from the Kaplan SST V2.csv", header = FALSE, sep = ";",  na ="NA", stringsAsFactors = FALSE)

tableau_AMO_brute <- read.csv("AMO_non_detandence.csv", header = FALSE, sep = ";",  na ="NA", stringsAsFactors = FALSE)

tableau_AMO_brute <- tableau_AMO_brute[-which(tableau_AMO_brute$V1 < 1948),]
tableau_AMO_brute <- tableau_AMO_brute[-which(tableau_AMO_brute$V1 == "AMO"),]#on enlève la dernière ligne

summary(tableau_AMO_brute)


tableau_AMO_brute$V1 <- as.factor(tableau_AMO_brute$V1)
tableau_AMO_detendance$V1 <- as.factor(tableau_AMO_detendance$V1)
tableau_AMO_brute$V2 <- as.numeric(tableau_AMO_brute$V2)
tableau_AMO_brute$V3 <- as.numeric(tableau_AMO_brute$V3)
tableau_AMO_brute$V4 <- as.numeric(tableau_AMO_brute$V4)

sapply(tableau_AMO_detendance, FUN = class)
sapply(tableau_AMO_brute, FUN = class)
summary(tableau_AMO_detendance)
sum(is.na(tableau_AMO_brute))
sum(is.na(tableau_AMO_detendance))

names(tableau_AMO_brute)[names(tableau_AMO_brute) == "V2"] <- "jan_brute"
names(tableau_AMO_brute)[names(tableau_AMO_brute) == "V3"] <- "fev_brute"
names(tableau_AMO_brute)[names(tableau_AMO_brute) == "V4"] <- "mar_brute"
names(tableau_AMO_brute)[names(tableau_AMO_brute) == "V5"] <- "avr_brute"
names(tableau_AMO_brute)[names(tableau_AMO_brute) == "V6"] <- "mai_brute"
names(tableau_AMO_brute)[names(tableau_AMO_brute) == "V7"] <- "juin_brute"
names(tableau_AMO_brute)[names(tableau_AMO_brute) == "V8"] <- "juil_brute"
names(tableau_AMO_brute)[names(tableau_AMO_brute) == "V9"] <- "aout_brute"
names(tableau_AMO_brute)[names(tableau_AMO_brute) == "V10"] <- "sep_brute"
names(tableau_AMO_brute)[names(tableau_AMO_brute) == "V11"] <- "oct_brute"
names(tableau_AMO_brute)[names(tableau_AMO_brute) == "V12"] <- "nov_brute"
names(tableau_AMO_brute)[names(tableau_AMO_brute) == "V13"] <- "dec_brute"

names(tableau_AMO_detendance)[names(tableau_AMO_detendance) == "V2"] <- "jan_detendance"
names(tableau_AMO_detendance)[names(tableau_AMO_detendance) == "V3"] <- "fev_detendance"
names(tableau_AMO_detendance)[names(tableau_AMO_detendance) == "V4"] <- "mar_detendance"
names(tableau_AMO_detendance)[names(tableau_AMO_detendance) == "V5"] <- "avr_detendance"
names(tableau_AMO_detendance)[names(tableau_AMO_detendance) == "V6"] <- "mai_detendance"
names(tableau_AMO_detendance)[names(tableau_AMO_detendance) == "V7"] <- "juin_detendance"
names(tableau_AMO_detendance)[names(tableau_AMO_detendance) == "V8"] <- "juil_detendance"
names(tableau_AMO_detendance)[names(tableau_AMO_detendance) == "V9"] <- "aout_detendance"
names(tableau_AMO_detendance)[names(tableau_AMO_detendance) == "V10"] <- "sep_detendance"
names(tableau_AMO_detendance)[names(tableau_AMO_detendance) == "V11"] <- "oct_detendance"
names(tableau_AMO_detendance)[names(tableau_AMO_detendance) == "V12"] <- "nov_detendance"
names(tableau_AMO_detendance)[names(tableau_AMO_detendance) == "V13"] <- "dec_detendance"

library(dplyr)
tableau_tendance <- left_join(tableau_AMO_brute, tableau_AMO_detendance, by = "V1")
tableau_tendance <- mutate(tableau_tendance, jan_tendance = jan_brute - jan_detendance)
tableau_tendance <- mutate(tableau_tendance, fev_tendance = fev_brute - fev_detendance)
tableau_tendance <- mutate(tableau_tendance, mar_tendance = mar_brute - mar_detendance)
tableau_tendance <- mutate(tableau_tendance, avr_tendance = avr_brute - avr_detendance)
tableau_tendance <- mutate(tableau_tendance, mai_tendance = mai_brute - mai_detendance)
tableau_tendance <- mutate(tableau_tendance, juin_tendance = juin_brute - juin_detendance)
tableau_tendance <- mutate(tableau_tendance, juil_tendance = juil_brute - juil_detendance)
tableau_tendance <- mutate(tableau_tendance, aout_tendance = aout_brute - aout_detendance)
tableau_tendance <- mutate(tableau_tendance, sep_tendance = sep_brute - sep_detendance)
tableau_tendance <- mutate(tableau_tendance, oct_tendance = oct_brute - oct_detendance)
tableau_tendance <- mutate(tableau_tendance, nov_tendance = nov_brute - nov_detendance)
tableau_tendance <- mutate(tableau_tendance, dec_tendance = dec_brute - dec_detendance)

library(tidyverse)
tableau_tendance_2 <- select(tableau_tendance, V1, jan_tendance, fev_tendance, mar_tendance, avr_tendance, mai_tendance, juin_tendance, juil_tendance, aout_tendance, sep_tendance, oct_tendance, nov_tendance, dec_tendance)
tableau_tendance_longer <- pivot_longer(tableau_tendance_2, cols = c(jan_tendance, fev_tendance, mar_tendance, avr_tendance, mai_tendance, juin_tendance, juil_tendance, aout_tendance, sep_tendance, oct_tendance, nov_tendance, dec_tendance))
names(tableau_tendance_longer)[names(tableau_tendance_longer) == "name"] <- "mois_tendance"
names(tableau_tendance_longer)[names(tableau_tendance_longer) == "value"] <- "AMO_tendance"
tableau_tendance_longer <- separate(tableau_tendance_longer, col = mois_tendance, into = c("mois", "tendance"), sep = "_")
tableau_tendance_longer <- select(tableau_tendance_longer, !tendance)

tableau_AMO_brute_longer <- pivot_longer(tableau_AMO_brute, cols = c(jan_brute, fev_brute, mar_brute, avr_brute, mai_brute, juin_brute, juil_brute, aout_brute, sep_brute, oct_brute, nov_brute, dec_brute))
names(tableau_AMO_brute_longer)[names(tableau_AMO_brute_longer) == "name"] <- "mois_brutes"
names(tableau_AMO_brute_longer)[names(tableau_AMO_brute_longer) == "value"] <- "AMO_brute"
tableau_AMO_brute_longer <- separate(tableau_AMO_brute_longer, col = mois_brutes, into = c("mois", "brute"), sep = "_")
tableau_AMO_brute_longer <- select(tableau_AMO_brute_longer, !brute)

tableau_AMO_detendance_longer <- pivot_longer(tableau_AMO_detendance, cols = c(jan_detendance, fev_detendance, mar_detendance, avr_detendance, mai_detendance, juin_detendance, juil_detendance, aout_detendance, sep_detendance, oct_detendance, nov_detendance, dec_detendance))
names(tableau_AMO_detendance_longer)[names(tableau_AMO_detendance_longer) == "name"] <- "mois_detendances"
names(tableau_AMO_detendance_longer)[names(tableau_AMO_detendance_longer) == "value"] <- "AMO_detendance"
tableau_AMO_detendance_longer <- separate(tableau_AMO_detendance_longer, col = mois_detendances, into = c("mois", "detendance"), sep = "_")
tableau_AMO_detendance_longer <- select(tableau_AMO_detendance_longer, !detendance)

tableau_tendance_longer2<- left_join(tableau_tendance_longer, tableau_AMO_detendance_longer, by = c("V1", "mois"))
tableau_tendance_longer2 <- left_join(tableau_tendance_longer2, tableau_AMO_brute_longer, by = c("V1", "mois"))

tableau_tendance_longer2 <- tableau_tendance_longer2[-c(891 : 900),]


library(ggplot2)
ggplot() +
  geom_point(data = tableau_tendance_longer2, aes(x = V1, y = AMO_tendance), color = "#339966") +
  #geom_point(data = tableau_tendance_longer2, aes(x = V1, y = AMO_detendance), colour = "#003399") +
  geom_point(data = tableau_tendance_longer2, aes(x = V1, y = AMO_brute), colour = "#990066") #+
  facet_wrap(~ mois)

ggplot(tableau_tendance_longer2, aes(x = V1, y = AMO_detendance)) +
  geom_point() +
  facet_wrap(~mois)


tableau_AMO_brute2 <- mutate(tableau_AMO_brute, Moy_AMO_brute = rowMeans(select(tableau_AMO_brute, jan_brute : dec_brute), na.rm = TRUE))
tableau_AMO_detendance2 <- mutate(tableau_AMO_detendance, Moy_AMO_detendance = rowMeans(select(tableau_AMO_detendance, jan_detendance : dec_detendance), na.rm = TRUE))
tableau_tendance_2 <- mutate(tableau_tendance_2, Moy_AMO_tendance = rowMeans(select(tableau_tendance_2, jan_tendance : dec_tendance)))

tableau_tendance_3 <- select(tableau_tendance_2, V1, Moy_AMO_tendance)
tableau_AMO_brute3 <- select(tableau_AMO_brute2, V1, Moy_AMO_brute)
tableau_AMO_detendance3 <- select(tableau_AMO_detendance2, V1, Moy_AMO_detendance)

tableau_moy_annuelles <- left_join(tableau_tendance_3, tableau_AMO_brute3, by = "V1")
tableau_moy_annuelles <- left_join(tableau_moy_annuelles, tableau_AMO_detendance3, by = "V1")
tableau_moy_annuelles <- tableau_moy_annuelles[-75,]

ggplot(tableau_moy_annuelles, aes(x = V1)) +
  geom_point(aes(y = Moy_AMO_tendance), colour = "#339966", alpha=0.5) +
  geom_point(aes(y = Moy_AMO_brute), colour = "black", alpha = 0.5) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, size = 8))

summary(tableau_moy_annuelles$Moy_AMO_detendance)
