########################################################################################
############################# fish benthos relationship ################################
########################################################################################

# objective: Supplementary materials 
# Author: Jack Johnson (jackvjohnson@hotmail.com)
# Date created: January 2024
# Last edited: 

# clear workspace
rm(list=ls())

# libraries

library(tidyverse)
library(fishualize)
library(patchwork)
library(brms)
library(vegan)
library(tidybayes)
library(ggcorrplot)
library(parallel)
library(viridis)

#########################################################################################
################################## working directories ##################################

output_directory <-  "C:/Users/jackv/OneDrive - Central Caribbean Marine Institute, Inc/Projects/AGGRA/Fish benthos relationship/Output_directory"

tidy_wd <- "C:/Users/jackv/OneDrive - Central Caribbean Marine Institute, Inc/Projects/AGGRA/Data_tidy/TIDY_COMPLETE"

##########################################################################################
################################### theme for figures ####################################

my_theme <- theme_classic() +
  theme(axis.title.x = element_text(size = 20, color = "black"), axis.title.y = element_text(size = 20, color = "black"), text=element_text(size=16)) +
  theme(axis.text.y = element_text(size=16, color="black"), axis.text.x = element_text(size=16, color="black", angle = 0, vjust = .0)) +
  theme(legend.text = element_text(size = 16),
        legend.title = element_text(size = 18, face = "bold")) +
  theme(plot.title = element_text(size = 22, face = "bold"))


#########################################################################################
################################### read in data sheets #################################

coral_raw_df <- read.csv(file=file.path(tidy_wd, "CORALRAW_TIDY.csv"))
fish_df <- read.csv(file=file.path(tidy_wd, "FISH_TIDY.csv"))

coral_raw_df <- coral_raw_df[,-c(1,2,6)]
fish_df <- fish_df[,-c(1,2,7)]

table(fish_df$fish_spp)

# tidy 

names(fish_df)
colnames(fish_df)[2] <- "Year"

fish_df <- fish_df[!is.na(fish_df$fish_spp),]
fish_df <- fish_df[!is.na(fish_df$count),]

fish_df <- mutate(fish_df,
                  foodweb = case_when(
                    fish_spp == "Sparisoma aurofrenatum" | fish_spp == "Sparisoma viride" ~ "Herbivore",
                    # Add more species and corresponding food webs as needed
                    TRUE ~ foodweb  # Keep the original 'foodweb' for other species
                  )
)

##########################################################################################
############################## matrix of coral genera ####################################

# Agaraicia = Agaricia 
# ESTO = NA
# Helioceris = Leptoseris
# Helioseris = Leptoseris 
# MEAN = Meandrina
# Mancina = Manicina
# MPHA = NA
# Mycetophellia = Mycetophyllia
# scolymia = Scolymia 
# SHYA = Solenastrea
# Sample dataframe with a column named 'genus'


# Replacement rules
coral_raw_df$Coral_genus <- trimws(coral_raw_df$Coral_genus)


replacement_rules <- list(
  "Agaraicia" = "Agaricia",
  "ESTO" = NA,
  "Helioceris" = "Leptoseris",
  "Helioseris" = "Leptoseris",
  "MEAN" = "Meandrina",
  "Mancina" = "Manicina",
  "MPHA" = NA,
  "Mycetophellia" = "Mycetophyllia",
  "scolymia" = "Scolymia",
  "SHYA" = "Solenastrea",
  "Leptoseris" = "Helioseris"
)

# Apply the replacement rules using gsub
coral_raw_df$Coral_genus <- gsub(pattern = paste(names(replacement_rules), collapse = "|"),
                                 replacement = unname(replacement_rules),
                                 x = coral_raw_df$Coral_genus)


coral_genusabundance <- coral_raw_df %>%
  group_by(Year, Coral_genus) %>%
  summarise(Abundance = n())

coral_genusabundance <- na.omit(coral_genusabundance)
tapply(coral_genusabundance$Abundance, coral_genusabundance$Coral_genus, sum)
# remove Oculina, Manicina, Mussa, Isophyllia, Favia, Solenastrea, Scolymia

coral_genusabundance <- coral_genusabundance %>%
  filter(Coral_genus != "" & Coral_genus !="#N/A")
coral_genusabundance <- coral_genusabundance %>%
  filter(!(Coral_genus %in% c("Mussa", "Oculina", "Scolymia", "Manicina", "Isophyllia", "Favia", "Solenastrea")))

summary(coral_genusabundance$Abundance)
hist(coral_genusabundance$Abundance)
hist(log1p(coral_genusabundance$Abundance))

coral_heatmap <- ggplot(coral_genusabundance , aes(x = reorder(Coral_genus, Abundance), y = as.factor(Year), fill = (Abundance))) +
  geom_tile(color = "white") +
  labs(x = "Coral Genus", y = "Year", fill = "Abundance", title = "") +
  #scale_fill_viridis_c(option = "A", direction = -1, breaks = c(1,3,5,7))+#, labels = c("Low", "High")) +
  scale_fill_fish(option = "Gramma_loreto", direction = -1)+#, breaks = c(1,3,5,7))+
  coord_flip() +
  my_theme +
  theme(legend.position = "right", plot.title = element_text(hjust = 0.5, vjust = 1.5, face = "plain", size = 14), 
        axis.text.y = element_text(face = "italic"),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))  # Set angle to 45 degrees and adjust hjust as needed)
#theme(legend.title = element_text(hjust = 0.5, vjust = 1.8))

Supplementary <- "Supplementary"

png(file=file.path(output_directory, Supplementary, "coral_species_heatmap.png"), height = 2500, width = 5500, res = 350)
coral_heatmap
dev.off()


coral_hist1 <- ggplot(coral_genusabundance, aes(x=Abundance)) +
  geom_histogram(bins=50, fill = "coral", color = "black") +
  my_theme
coral_hist1

coral_hist2 <- ggplot(coral_genusabundance, aes(x=log1p(Abundance))) +
  geom_histogram(bins=50, fill = "coral", color = "black") +
  my_theme
coral_hist2


png(file=file.path(output_directory, Supplementary, "Coral_genus_hist.png"), height = 2500, width = 5500, res = 350)
coral_hist1 + coral_hist2 + plot_annotation(tag_levels = "A")  &
  theme(plot.tag = element_text(size = 30, face = "bold"))
dev.off()

#############################################################################################
################################ fish species heatmap #######################################

fishSP_df <- fish_df %>%
  group_by(Year, fish_spp) %>%
  summarise(Abundance = sum(count)) %>%
  arrange(desc(fish_spp)) %>%
  head(250)

fishSP_heatmap <- ggplot(fishSP_df, aes(x = reorder(fish_spp, Abundance), y = as.factor(Year), fill = (Abundance))) +
  geom_tile(color = "white") +
  labs(x = "Fish Species", y = "Year", fill = "Abundance", title = "") +
  scale_fill_fish(option = "Gramma_loreto", direction = -1)+#, breaks = c(1,3,5,7))+
  #scale_fill_viridis_c(option = "A", direction = -1, breaks = c(6.8, 8.889), labels = c("Low", "High")) +
  #scale_fill_gradient(low="blue", high = "red")+
  #scale_fill_gradient(colours=viridis(4))+
  coord_flip() +
  my_theme +
  theme(legend.position = "right", plot.title = element_text(hjust = 0.5, vjust = 1.5, face = "plain", size = 14), 
        axis.text.y = element_text(face = "italic"),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) 
#theme(legend.title = element_text(hjust = 0.5, vjust = 1.8))
fishSP_heatmap

hist(fishSP_df$Abundance)
hist(log1p(fishSP_df$Abundance))

png(file=file.path(output_directory, Supplementary, "FishSP_heatmap.png"), height = 2500, width = 5500, res = 350)
fishSP_heatmap
dev.off()

##############################################################################################
###################################### trophic guilds ########################################

foodweb_df <- fish_df %>%
  group_by(Year, foodweb) %>%
  summarise(Count = sum(count))

foodweb_heatmap <- ggplot(foodweb_df, aes(x = reorder(foodweb, Count), y = as.factor(Year), fill = (Count))) +
  geom_tile(color = "white") +
  labs(x = "Foodweb", y = "Year", fill = "Count", title = "") +
  scale_fill_fish(option = "Gramma_loreto", direction = -1)+#, breaks = c(1,3,5,7))+
  #scale_fill_viridis_c(option = "A", direction = -1, breaks = c(6.8, 8.889), labels = c("Low", "High")) +
  #scale_fill_gradient(low="blue", high = "red")+
  #scale_fill_gradient(colours=viridis(4))+
  coord_flip() +
  my_theme +
  theme(legend.position = "right", plot.title = element_text(hjust = 0.5, vjust = 1.5, face = "plain", size = 14), 
        axis.text.y = element_text(face = "italic"),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) 
#theme(legend.title = element_text(hjust = 0.5, vjust = 1.8))
foodweb_heatmap

png(file=file.path(output_directory, Supplementary, "foodweb_heatmap.png"), height = 1500, width = 3000, res = 350)
foodweb_heatmap
dev.off()