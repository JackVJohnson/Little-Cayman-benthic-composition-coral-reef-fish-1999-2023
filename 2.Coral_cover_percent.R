########################################################################################
############################ AGGRA combine coral cover  ################################
########################################################################################

# objective: combine coral cover
# Author: Jack Johnson (jackvjohnson@hotmail.com)
# Date created: July 2023
# Last edited: 

# clear workspace
rm(list=ls())

# libraries

library(tidyverse)

# working directories 

tidy_wd <- "C:/Users/jackv/OneDrive - Central Caribbean Marine Institute, Inc/Projects/AGGRA/Data_tidy"
messy_wd <- "C:/Users/jackv/OneDrive - Central Caribbean Marine Institute, Inc/Projects/AGGRA/Data_messy"

# surface area of a coral calculation 
# 2*pi()*(max_diameter/2)*max_height

coral_df <- read.csv(file=file.path(tidy_wd, "AGRRA_coral_1999_2023.csv"))

# coral_df$Transect <- as.numeric(coral_df$Transect)
coral_df$length <- as.numeric(coral_df$length)
coral_df$width <- as.numeric(coral_df$width)
coral_df$height <- as.numeric(coral_df$height)
coral_df$coral_SA <- as.numeric(coral_df$coral_SA)

coral_df$coral_SA <- 2*pi*(coral_df$length/2)*coral_df$height

# coral %cover dataframe 

table(coral_df$Transect)

# coral df with transects is an ABSOLUTE MESS (culprits are GGG, EC, and MF).
# I've fixed this in the raw csv so from here import new csv EDITED. 
############################################################################################

coral_df <- read.csv(file=file.path(tidy_wd, "AGRRA_coral_1999_2023_EDITED.csv"))

coral_df$length <- as.numeric(coral_df$length)
coral_df$width <- as.numeric(coral_df$width)
coral_df$height <- as.numeric(coral_df$height)
coral_df$coral_SA <- as.numeric(coral_df$coral_SA)

coral_df$coral_SA <- 2*pi*(coral_df$length/2)*coral_df$height

# coral %cover dataframe 

table(coral_df$Transect)

# Install and load the stringr package if you haven't already
# install.packages("stringr")


coral_df$Transect <- as.numeric(
  str_replace_all(
    coral_df$Transect,
    c("T1" = "1", "T2" = "2", "T3" = "3", "T4" = "4", "T5" = "5", "T6" = "6")
  )
)

# for cover dataframe transects are 1m wide and 10m long 

coral_df$cover <- coral_df$length/100/10

# ggg in the dog house for poor data entry (it's 2023 so lazy dragging has occurred)
coral_df$Year <- gsub("2024|2025", "2023", coral_df$Year)

###########################################################################################
############################## coral cover data frames ####################################

coral_cover_1999_2021 <- read.csv(file=file.path(messy_wd, "benthic", "coral_percent_cover_1999_2021.csv"))

benthic_2022_GGG <- read.csv(file=file.path(messy_wd, "benthic", "AGRRA_benthic_GGG_2022.csv"))
benthic_2022_AM <- read.csv(file=file.path(messy_wd, "benthic", "AGRRA_benthic_AM_2022.csv"))
benthic_2022_JLR <- read.csv(file=file.path(messy_wd, "benthic", "AGRRA_benthic_JLR_2022.csv"))
benthic_2022_ZP <- read.csv(file=file.path(messy_wd, "benthic", "AGRRA_benthic_ZP_2022.csv"))
benthic_2023_GGG <- read.csv(file=file.path(messy_wd, "benthic", "AGRRA_benthic_GGG_2023.csv"))
benthic_2023_HAD <- read.csv(file=file.path(messy_wd, "benthic", "AGRRA_benthic_HAD_2023.csv"))
benthic_2023_KE <- read.csv(file=file.path(messy_wd, "benthic", "AGRRA_benthic_KE_2023.csv"))
benthic_2023_LLG <- read.csv(file=file.path(messy_wd, "benthic", "AGRRA_benthic_LLG_2023.csv"))

coral_cover_2022_2023 <- rbind(benthic_2022_GGG, benthic_2022_AM, benthic_2022_JLR,
                               benthic_2022_ZP, benthic_2023_GGG, benthic_2023_HAD, benthic_2023_KE, benthic_2023_LLG)
coral_cover_2022_2023 <- subset(coral_cover_2022_2023, Functional_Group == "coral")

cover_early_df <- coral_cover_1999_2021 %>%
  group_by(year, site, observ, transect) %>%
  summarise(Perc_cover = mean(percent_cover), depth = mean(depth))

cover_early_df$Perc_cover <- cover_early_df$Perc_cover*100

cover_late_df <- coral_cover_2022_2023 %>%
  group_by(Year, Site, Observer, Transect) %>%
  summarise(Perc_cover = mean(Perc_cover), Depth = mean(Depth))

names(coral_cover_1999_2021)
names(coral_cover_2022_2023)
# year site transect obersver perc_cover
new_df <- data.frame(Year = cover_early_df$year)
new_df$Site <- cover_early_df$site
new_df$Transect <- cover_early_df$transect
new_df$Observer <- cover_early_df$observ
new_df$Perc_cover <- cover_early_df$Perc_cover
new_df$Depth <- cover_early_df$depth



new_df <- rbind(new_df, cover_late_df)

write.csv(new_df, file=file.path(tidy_wd, "Coral_cover_1999_2023.csv"))
