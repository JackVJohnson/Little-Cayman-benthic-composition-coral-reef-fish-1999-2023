########################################################################################
#################################### Ecology Letters ###################################
########################################################################################

# objective: TIDY SITE NAMES 
# Author: Jack Johnson (jackvjohnson@hotmail.com)
# Date created: July 2023
# Last edited: 

# clear workspace
rm(list=ls())

# libraries

library(tidyverse)

# working directories 

tidy_wd <- "C:/Users/jackv/OneDrive - Central Caribbean Marine Institute, Inc/Projects/AGGRA/Data_tidy"

output_directory <-  "C:/Users/jackv/OneDrive - Central Caribbean Marine Institute, Inc/Projects/AGGRA/Fish benthos relationship/Output_directory"

########################################################################################
################################### read in files ######################################

coral_cover_df <- read.csv(file=file.path(tidy_wd, "Coral_cover_1999_2023.csv"))
coral_cover_df$Year <- as.factor(coral_cover_df$Year)

algae_df <- read.csv(file=file.path(tidy_wd, "AGRRA_algae_1999_2023.csv"))
algae_df$Year <- as.factor(algae_df$Year)

recruits_df <- read.csv(file=file.path(tidy_wd, "AGRRA_recruits_1999_2023.csv"))
recruits_df$Year <- as.factor(recruits_df$Year)

########################################################################################

coral_df <- read.csv(file=file.path(tidy_wd, "AGRRA_coral_1999_2023_EDITED.csv"))
coral_df$Year <- as.factor(coral_df$Year)

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


# ggg in the dog house for poor data entry (it's 2023 so lazy dragging has occurred)
coral_df$Year <- gsub("2024|2025", "2023", coral_df$Year)

########################################################################################
################################## load in fish data ###################################

fish_df <- read.csv(file=file.path(tidy_wd, "AGRRA_fish_1999_2023.csv"))
summary(fish_df)
fish_df$biomass <- as.numeric(fish_df$biomass)
fish_df$density <- as.numeric(fish_df$density)
fish_df$count <- as.numeric(fish_df$count)
fish_df$year <- as.factor(fish_df$year)


########################################################################################
################################# site names need tidying ##############################

coral_df$Site <- trimws(coral_df$Site)

coral_RAW_TIDY <- coral_df %>%
  mutate(Site_Name = recode(Site, 
                            "Anns Attic" = "Three Fathom Wall",
                            "Ann's Attic" = "Three Fathom Wall",
                            "Black Tip Tunnels" = "Blacktip Blvd.", 
                            "Charles Bay" = "Charlie's Chimney", 
                            "CORAL CITY" = "Coral City",
                            "Coral City" =  "Coral City" ,
                            "Crystal Palace" = "Crystal Palace Wall", 
                            "Fins" = "Martha's Finyard", 
                            "Grundys" = "Grundy's Gardens" , 
                            "Grundys Gardens" = "Grundy's Gardens" , 
                            "Grundys Gardens" = "Grundy's Gardens" ,
                            "Grundys  Gardens" = "Grundy's Gardens" ,
                            "GRUNDYS" = "Grundy's Gardens" ,
                            "Grundy's  Gardens" = "Grundy's Gardens" ,
                            "Icon" = "Icon", 
                            "Icon Reef" = "Icon",
                            "ICON" = "Icon", 
                            "ICON Reef" = "Icon", 
                            "Jigsaw" = "Jigsaw Puzzle", 
                            "JIGSAW" = "Jigsaw Puzzle",
                            "Joys Joy" = "Joy's Joy", 
                            "Lucass Ledge" = "Luca's Ledges",
                            "Main Channel - East Side" = "Great Wall East", 
                            "Marilyns" = "Marylin's Cut", 
                            "Marilyns Cut" = "Marylin's Cut", 
                            "Marilyn's Cut" = "Marylin's Cut",
                            "Marilyns_cut" = "Marylin's Cut",
                            "Marthas" = "Martha's Finyard",
                            "Marthas Finyard" = "Martha's Finyard", 
                            "MARTHAS" = "Martha's Finyard",
                            "Meadows" = "The Meadows", 
                            "MEADOWS" = "The Meadows", 
                            "meadows" = "The Meadows", 
                            "Mixing Bowl" = "Sailfin Miniwall", 
                            "MIXING BOWL" = "Sailfin Miniwall",
                            "MIXINGBOWL" = "Sailfin Miniwall", 
                            "Nancys Cup of Tea" = "Paul's Anchor",
                            "Nancy's Cup of Tea" = "Paul's Anchor", 
                            "PAULS ANCHOR" = "Paul's Anchor", 
                            "No Name" = "Icon", 
                            "Pauls Anchor" = "Paul's Anchor", 
                            "Mary's Bay" = "Penguin's Leap", 
                            "Penguins Leap" = "Penguin's Leap", 
                            "RichardsReef" = "Richard's Reef",
                            "Richard'sReef" = "Richard's Reef",
                            "Rock Bottom" = "Rock Bottom Wall", 
                            "Sailfin" = "Sailfin Reef",
                            "SAILFIN" = "Sailfin Reef",
                            "Snap Shot" = "Snap Shot", 
                            "Snapshot" = "Snap Shot", 
                            "SNAPSHOT" = "Snap Shot",
                            "West Point" = "West Point",
                            "Westpoint" = "West Point", 
                            "WestPoint" = "West Point",
                            "WESTPOINT" = "West Point", 
                            "Wreck" = "Soto Trader"))
table(coral_RAW_TIDY$Site_Name)

coral_cover_df$Site <- trimws(coral_cover_df$Site)
coral_cover_TIDY <- coral_cover_df %>%
  mutate(Site_Name = recode(Site, 
                            "Anns Attic" = "Three Fathom Wall",
                            "Ann's Attic" = "Three Fathom Wall",
                            "Black Tip Tunnels" = "Blacktip Blvd.", 
                            "Charles Bay" = "Charlie's Chimney", 
                            "CORAL CITY" = "Coral City",
                            "Coral City" =  "Coral City" ,
                            "Crystal Palace" = "Crystal Palace Wall", 
                            "Fins" = "Martha's Finyard", 
                            "Grundys" = "Grundy's Gardens" , 
                            "Grundys Gardens" = "Grundy's Gardens" , 
                            "Grundys Gardens" = "Grundy's Gardens" ,
                            "Grundys  Gardens" = "Grundy's Gardens" ,
                            "GRUNDYS" = "Grundy's Gardens" ,
                            "Grundy's  Gardens" = "Grundy's Gardens" ,
                            "Icon" = "Icon", 
                            "Icon Reef" = "Icon",
                            "ICON" = "Icon", 
                            "ICON Reef" = "Icon", 
                            "Jigsaw" = "Jigsaw Puzzle", 
                            "JIGSAW" = "Jigsaw Puzzle",
                            "Joys Joy" = "Joy's Joy", 
                            "Lucass Ledge" = "Luca's Ledges",
                            "Main Channel - East Side" = "Great Wall East", 
                            "Marilyns" = "Marylin's Cut", 
                            "Marilyns Cut" = "Marylin's Cut", 
                            "Marilyn's Cut" = "Marylin's Cut",
                            "Marilyns_cut" = "Marylin's Cut",
                            "Marthas" = "Martha's Finyard",
                            "Marthas Finyard" = "Martha's Finyard", 
                            "MARTHAS" = "Martha's Finyard",
                            "Meadows" = "The Meadows", 
                            "MEADOWS" = "The Meadows", 
                            "meadows" = "The Meadows", 
                            "Mixing Bowl" = "Sailfin Miniwall", 
                            "MIXING BOWL" = "Sailfin Miniwall",
                            "MIXINGBOWL" = "Sailfin Miniwall", 
                            "Nancys Cup of Tea" = "Paul's Anchor",
                            "Nancy's Cup of Tea" = "Paul's Anchor", 
                            "PAULS ANCHOR" = "Paul's Anchor", 
                            "No Name" = "Icon", 
                            "Pauls Anchor" = "Paul's Anchor", 
                            "Mary's Bay" = "Penguin's Leap", 
                            "Penguins Leap" = "Penguin's Leap", 
                            "RichardsReef" = "Richard's Reef",
                            "Richard'sReef" = "Richard's Reef",
                            "Rock Bottom" = "Rock Bottom Wall", 
                            "Sailfin" = "Sailfin Reef",
                            "SAILFIN" = "Sailfin Reef",
                            "Snap Shot" = "Snap Shot", 
                            "Snapshot" = "Snap Shot", 
                            "SNAPSHOT" = "Snap Shot",
                            "West Point" = "West Point",
                            "Westpoint" = "West Point", 
                            "WestPoint" = "West Point",
                            "WESTPOINT" = "West Point", 
                            "Wreck" = "Soto Trader"))

table(coral_cover_TIDY$Site_Name)


algae_df$Site <- trimws(algae_df$Site)
algae_cover_TIDY <- algae_df %>%
  mutate(Site_Name = recode(Site, 
                            "Anns Attic" = "Three Fathom Wall",
                            "Ann's Attic" = "Three Fathom Wall",
                            "Black Tip Tunnels" = "Blacktip Blvd.", 
                            "Charles Bay" = "Charlie's Chimney", 
                            "CORAL CITY" = "Coral City",
                            "Coral City" =  "Coral City" ,
                            "Crystal Palace" = "Crystal Palace Wall", 
                            "Fins" = "Martha's Finyard", 
                            "Grundys" = "Grundy's Gardens" , 
                            "Grundys Gardens" = "Grundy's Gardens" , 
                            "Grundys Gardens" = "Grundy's Gardens" ,
                            "Grundys  Gardens" = "Grundy's Gardens" ,
                            "GRUNDYS" = "Grundy's Gardens" ,
                            "Grundy's  Gardens" = "Grundy's Gardens" ,
                            "Icon" = "Icon", 
                            "Icon Reef" = "Icon",
                            "ICON" = "Icon", 
                            "ICON Reef" = "Icon",
                            "ICON CREWS" = "Icon",
                            "Jigsaw" = "Jigsaw Puzzle", 
                            "JIGSAW" = "Jigsaw Puzzle",
                            "Joys Joy" = "Joy's Joy", 
                            "Lucass Ledge" = "Luca's Ledges",
                            "Main Channel - East Side" = "Great Wall East", 
                            "Marilyns" = "Marylin's Cut", 
                            "Marilyns Cut" = "Marylin's Cut", 
                            "Marilyn's Cut" = "Marylin's Cut",
                            "Marilyns_cut" = "Marylin's Cut",
                            "Marthas" = "Martha's Finyard",
                            "Marthas Finyard" = "Martha's Finyard", 
                            "MARTHAS" = "Martha's Finyard",
                            "Meadows" = "The Meadows", 
                            "MEADOWS" = "The Meadows", 
                            "meadows" = "The Meadows", 
                            "Mixing Bowl" = "Sailfin Miniwall", 
                            "MIXING BOWL" = "Sailfin Miniwall",
                            "MIXINGBOWL" = "Sailfin Miniwall", 
                            "Nancys Cup of Tea" = "Paul's Anchor",
                            "Nancy's Cup of Tea" = "Paul's Anchor", 
                            "PAULS ANCHOR" = "Paul's Anchor", 
                            "No Name" = "Icon", 
                            "Pauls Anchor" = "Paul's Anchor", 
                            "Mary's Bay" = "Penguin's Leap", 
                            "Penguins Leap" = "Penguin's Leap", 
                            "RichardsReef" = "Richard's Reef",
                            "Richard'sReef" = "Richard's Reef",
                            "Richardss Reef" = "Richard's Reef",
                            "Rock Bottom" = "Rock Bottom Wall", 
                            "Sailfin" = "Sailfin Reef",
                            "SAILFIN" = "Sailfin Reef",
                            "Snap Shot" = "Snap Shot", 
                            "Snapshot" = "Snap Shot", 
                            "SNAPSHOT" = "Snap Shot",
                            "West Point" = "West Point",
                            "Westpoint" = "West Point", 
                            "WestPoint" = "West Point",
                            "WESTPOINT" = "West Point", 
                            "Wreck" = "Soto Trader"))
table(algae_cover_TIDY$Site_Name)

recruits_df$Site <- trimws(recruits_df$Site)
recruits_TIDY <- recruits_df %>%
  mutate(Site_Name = recode(Site, 
                            "Anns Attic" = "Three Fathom Wall",
                            "Ann's Attic" = "Three Fathom Wall",
                            "Black Tip Tunnels" = "Blacktip Blvd.", 
                            "Charles Bay" = "Charlie's Chimney", 
                            "CORAL CITY" = "Coral City",
                            "Coral City" =  "Coral City" ,
                            "Crystal Palace" = "Crystal Palace Wall", 
                            "Fins" = "Martha's Finyard", 
                            "Grundys" = "Grundy's Gardens" , 
                            "Grundys Gardens" = "Grundy's Gardens" , 
                            "Grundys Gardens" = "Grundy's Gardens" ,
                            "Grundys  Gardens" = "Grundy's Gardens" ,
                            "GRUNDYS" = "Grundy's Gardens" ,
                            "Grundy's  Gardens" = "Grundy's Gardens" ,
                            "Icon" = "Icon", 
                            "Icon Reef" = "Icon",
                            "ICON" = "Icon", 
                            "ICON Reef" = "Icon",
                            "ICON CREWS" = "Icon",
                            "Jigsaw" = "Jigsaw Puzzle", 
                            "JIGSAW" = "Jigsaw Puzzle",
                            "Joys Joy" = "Joy's Joy", 
                            "Lucass Ledge" = "Luca's Ledges",
                            "Main Channel - East Side" = "Great Wall East", 
                            "Marilyns" = "Marylin's Cut", 
                            "Marilyns Cut" = "Marylin's Cut", 
                            "Marilyn's Cut" = "Marylin's Cut",
                            "Marilyns_cut" = "Marylin's Cut",
                            "Marthas" = "Martha's Finyard",
                            "Marthas Finyard" = "Martha's Finyard", 
                            "MARTHAS" = "Martha's Finyard",
                            "marthas" = "marthas", 
                            "Meadows" = "The Meadows", 
                            "MEADOWS" = "The Meadows", 
                            "meadows" = "The Meadows", 
                            "Mixing Bowl" = "Sailfin Miniwall", 
                            "MIXING BOWL" = "Sailfin Miniwall",
                            "MIXINGBOWL" = "Sailfin Miniwall", 
                            "Nancys Cup of Tea" = "Paul's Anchor",
                            "Nancy's Cup of Tea" = "Paul's Anchor", 
                            "PAULS ANCHOR" = "Paul's Anchor", 
                            "No Name" = "Icon", 
                            "Pauls Anchor" = "Paul's Anchor", 
                            "Mary's Bay" = "Penguin's Leap", 
                            "Penguins Leap" = "Penguin's Leap", 
                            "RichardsReef" = "Richard's Reef",
                            "Richard'sReef" = "Richard's Reef",
                            "Richardss Reef" = "Richard's Reef", 
                            "Rock Bottom" = "Rock Bottom Wall", 
                            "Sailfin" = "Sailfin Reef",
                            "SAILFIN" = "Sailfin Reef",
                            "Snap Shot" = "Snap Shot", 
                            "Snapshot" = "Snap Shot", 
                            "SNAPSHOT" = "Snap Shot",
                            "West Point" = "West Point",
                            "Westpoint" = "West Point", 
                            "WestPoint" = "West Point",
                            "WESTPOINT" = "West Point",
                            "west point" = "West Point", 
                            "Wreck" = "Soto Trader"))

table(recruits_TIDY$Site_Name)

fish_df$site <- trimws(fish_df$site)
fish_TIDY <- fish_df %>%
  mutate(Site_Name = recode(site, 
                            "Anns Attic" = "Three Fathom Wall",
                            "Ann's Attic" = "Three Fathom Wall",
                            "Black Tip Tunnels" = "Blacktip Blvd.", 
                            "Blacktip Tunnels" = "Blacktip Blvd.",
                            "Charles Bay" = "Charlie's Chimney", 
                            "CORAL CITY" = "Coral City",
                            "Coral City" =  "Coral City" ,
                            "coral_city" = "Coral City", 
                            "Crystal Palace" = "Crystal Palace Wall", 
                            "Fins" = "Martha's Finyard", 
                            "Grundys" = "Grundy's Gardens" , 
                            "Grundy's" = "Grundy's Gardens",
                            "grundys" = "Grundy's Gardens",
                            "Grundys Gardens" = "Grundy's Gardens" , 
                            "Grundys Gardens" = "Grundy's Gardens" ,
                            "Grundys  Gardens" = "Grundy's Gardens" ,
                            "GRUNDYS" = "Grundy's Gardens" ,
                            "Grundy's  Gardens" = "Grundy's Gardens" ,
                            "Icon" = "Icon", 
                            "Icon Reef" = "Icon",
                            "ICON" = "Icon", 
                            "ICON Reef" = "Icon",
                            "ICON CREWS" = "Icon",
                            "jigsaw" = "Jigsaw Puzzle", 
                            "Jigsaw" = "Jigsaw Puzzle", 
                            "JIGSAW" = "Jigsaw Puzzle",
                            "Joys Joy" = "Joy's Joy", 
                            "Lucass Ledge" = "Luca's Ledges",
                            "Lucas Ledge" = "Luca's Ledges",
                            "Main Channel - East Side" = "Great Wall East", 
                            "Marilyns" = "Marylin's Cut", 
                            "Marilyns Cut" = "Marylin's Cut", 
                            "Marilyn's Cut" = "Marylin's Cut",
                            "Marilyns_cut" = "Marylin's Cut",
                            "marilyns_cut" = "Marylin's Cut", 
                            "Marlyns cut" = "Marylin's Cut", 
                            "marthas" = "Martha's Finyard",  
                            "Marthas" = "Martha's Finyard",
                            "Marthas Finyard" = "Martha's Finyard", 
                            "MARTHAS" = "Martha's Finyard",
                            "marthas" = "marthas", 
                            "Meadows" = "The Meadows", 
                            "MEADOWS" = "The Meadows", 
                            "meadows" = "The Meadows", 
                            "Mixing Bowl" = "Sailfin Miniwall", 
                            "MIXING BOWL" = "Sailfin Miniwall",
                            "MIXINGBOWL" = "Sailfin Miniwall", 
                            "Nancys Cup of Tea" = "Paul's Anchor",
                            "Nancy's Cup of Tea" = "Paul's Anchor",
                            "Nancy's" = "Paul's Anchor", 
                            "Pauls Anchors" = "Paul's Anchor", 
                            "pauls_anchor" = "Paul's Anchor", 
                            "PAULS ANCHOR" = "Paul's Anchor", 
                            "No Name" = "Icon", 
                            "Pauls Anchor" = "Paul's Anchor", 
                            "Mary's Bay" = "Penguin's Leap", 
                            "Penguins Leap" = "Penguin's Leap", 
                            "RichardsReef" = "Richard's Reef",
                            "Richard'sReef" = "Richard's Reef",
                            "Richardss Reef" = "Richard's Reef", 
                            "Rock Bottom" = "Rock Bottom Wall", 
                            "Sailfin" = "Sailfin Reef",
                            "SAILFIN" = "Sailfin Reef",
                            "Snap Shot" = "Snap Shot", 
                            "Snapshot" = "Snap Shot", 
                            "SNAPSHOT" = "Snap Shot",
                            "snapshot" = "Snap Shot", 
                            "West Point" = "West Point",
                            "Westpoint" = "West Point", 
                            "WestPoint" = "West Point",
                            "WESTPOINT" = "West Point",
                            "west point" = "west Point", 
                            "westpoint" = "West Point",
                            "Wreck" = "Soto Trader"))

table(fish_TIDY$Site_Name)

names(fish_df)

########################################################################################
################################ correct any fish spp name #############################
 
table(fish_TIDY$fish_spp)
table(fish_TIDY$common_name)
table(fish_TIDY$common_family)
table(fish_TIDY$foodweb) # ok

# common family

fish_TIDY <- fish_TIDY %>%
  mutate(common_family = recode_factor(common_family,
                                       "Filefishes" = "Filefish"))

fish_TIDY$fish_spp <- gsub("/", " ", fish_TIDY$fish_spp)
fish_TIDY$fish_spp <- gsub("_", " ", fish_TIDY$fish_spp)


fish_TIDY <- fish_TIDY %>%
  mutate(fish_spp = recode_factor(fish_spp,
                                  "7" = "NA",
                                  "sparisoma chrysopterum" = "Sparisoma chrysopterum",
                                  "cephalopholis fulva" = "Cephalopholis fulva",
                                  "Scarus   Sparisoma"= "NA"))
                                  

table(fish_TIDY$fish_spp)

tidy_completeWD <- "C:/Users/jackv/OneDrive - Central Caribbean Marine Institute, Inc/Projects/AGGRA/Data_tidy/TIDY_COMPLETE"

write.csv(algae_cover_TIDY, file=file.path(tidy_completeWD, "ALGAE_TIDY.csv"))
write.csv(coral_cover_TIDY, file=file.path(tidy_completeWD, "CORALCOVER_TIDY.csv"))
write.csv(coral_RAW_TIDY, file=file.path(tidy_completeWD, "CORALRAW_TIDY.csv"))
write.csv(recruits_TIDY, file=file.path(tidy_completeWD, "RECRUITS_TIDY.csv"))
write.csv(fish_TIDY, file=file.path(tidy_completeWD, "FISH_TIDY.csv"))

