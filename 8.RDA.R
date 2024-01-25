########################################################################################
############################# fish benthos relationship ################################
########################################################################################

# objective: Redundancy analysis 
# Author: Jack Johnson (jackvjohnson@hotmail.com)
# Date created: December 2023
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


df2 <- coral_raw_df %>%
  group_by(Year, Site_Name, Coral_genus) %>%
  summarise(total_abundance = n())

df2 <- na.omit(df2)

df2 <- df2 %>%
  filter(Coral_genus != "" & Coral_genus !="#N/A")

# remove low abundant species 

tapply(df2$total_abundance, df2$Coral_genus, sum)
# drop mussa, oculina, scolymia, manicina 

# Assuming 'your_dataframe' is the name of your dataframe
df2 <- df2 %>%
  filter(!(Coral_genus %in% c("Mussa", "Oculina", "Scolymia", "Manicina")))



df3 <- df2 %>%
  pivot_wider(names_from = Coral_genus, values_from = total_abundance)

df3[is.na(df3)] <- 0

##########################################################################################
############################# Environmental dataframe ####################################

# in this case, fish community 

fish_env <- fish_df %>% 
  group_by(Year, Site_Name, foodweb) %>%
  summarise(mean_biomass=mean(biomass))

fish_wide <- fish_env %>%
  pivot_wider(names_from = foodweb, values_from = mean_biomass)



# unequeal number of survey years and number of surveys per year 

match_df <- left_join(df3, fish_wide, by =c("Site_Name", "Year"))
match_df <- na.omit(match_df)

coral_mat <- match_df[,3:21]
coral_hellinger <- decostand(coral_mat, method = "hellinger")

my_rda <- rda(coral_hellinger ~ Carnivore + Corallivore + Herbivore + Omnivore, data = match_df)
summary(my_rda)

plot(my_rda, type = "n", scaling = "none")
orditorp(my_rda, display='sp', cex=1.2, scaling="none", col='blue')
text(my_rda, display='bp', col='red', cex=.8, lwd = .5)


plot(my_rda, display = c("sp"))

RsquareAdj(my_rda)
anova.cca(my_rda,permutations = 999)
anova.cca(my_rda, by="axis")
anova.cca(my_rda, by="terms")

plot_datagenus1 <- data.frame(
  x = scores(my_rda, display = "sp", scaling = "none")[, 1],
  y = scores(my_rda, display = "sp", scaling = "none")[, 2],
  species = rownames(scores(my_rda, display = "sp"))
)

plot_datagenus2 <- data.frame(
  predictor1 = scores(my_rda, display = "bp")[, 1],
  predictor2 = scores(my_rda, display = "bp")[, 2],
  guild = c("Carnivore", "Corallivore", "Herbivore", "Omnivore")  # Add your trophic guilds here
)

plot_datagenus2 <- do.call(rbind, c(list(plot_datagenus2), lapply(1:15, function(x) setNames(rep(NA, ncol(plot_datagenus2)), names(plot_datagenus2)))))

plot_genus <- cbind(plot_datagenus1, plot_datagenus2)

# Create a ggplot scatter plot
p1 <- ggplot(plot_genus, aes(x, y)) +
  #geom_point(aes(color = species)) +
  theme_minimal() +
  labs(title = "", x="RDA 1", y="RDA 2") +
  theme(legend.position = "none") +
  theme_classic() +
  my_theme

library(ggrepel)

# Add arrows for predictor loadings with labels
p1 <- p1 + geom_segment(
  aes(x = 0, y = 0, xend = predictor1, yend = predictor2),
  arrow = arrow(length = unit(0.2, "cm")),
  color = "red4", size = 1
) +
  #geom_text_repel(aes(label = species), vjust = -0.5, hjust = -0.5, size = 5, color = "navy", box.padding = 0.5, point.padding = 0.1, min.segment.length=1, max.overlaps = 10) +
  geom_text(aes(label = species), vjust = -.3, hjust = .1, size = 5, color = "navy") +
  geom_label(aes(x = predictor1, y = predictor2, label = guild), size = 3, color = "red4", fontface = "bold", hjust =1.2, vjust=1)

p1

##########################################################################################
########################### same for coral growth forms ##################################

# using growth forms by Kramer et al 2023 and Darling et al 2013

# Competitive: Acropora, Millepora

# Stress tolerant: Colpophyllia, Pseudodiploria, Meandrina, Montastrea, Orbicella, Siderastrea, Dichocoenia, diploria, Eusmilia, favia, stephanocoenia

# weedy: Agaricia, Leptoseris, Madracis, Porites,  Isophyllia, Manicina


# Assuming your dataframe is named 'coral_data' and you have a column named 'Genus'
growth_df <- mutate(coral_raw_df, Growth_Form = case_when(
  Coral_genus %in% c("Acropora", "Millepora") ~ "Competitive",
  Coral_genus %in% c("Colpophyllia", "Pseudodiploria", "Meandrina", "Montastraea", "Orbicella", "Siderastrea", "Dichocoenia", "Diploria", "Eusmilia", "favia", "stephanocoenia") ~ "Stress Tolerant",
  Coral_genus %in% c("Agaricia", "Leptoseris", "Madracis", "Porites", "Isophyllia", "Manicina") ~ "Weedy",
  TRUE ~ NA_character_  # If none of the conditions match, set to NA
))

growth_dfsum <- growth_df %>%
  group_by(Site_Name, Year, Growth_Form) %>%
  summarise(total_abundance = n())

growth_dfsum <- na.omit(growth_dfsum)
growth_wide <- growth_dfsum %>%
  pivot_wider(names_from = Growth_Form, values_from = total_abundance)
growth_wide[is.na(growth_wide)] <- 0

# match up with fish df

growthmatch_df <- left_join(growth_wide, fish_wide, by =c("Site_Name", "Year"))
growthmatch_df <- na.omit(growthmatch_df)

growth_mat <- growthmatch_df[,3:5]
growth_hellinger <- decostand(growth_mat, method = "hellinger")

growth_rda <- rda(growth_hellinger ~ Carnivore + Corallivore + Herbivore + Omnivore, data = growthmatch_df)
summary(growth_rda)

# check plot
plot(growth_rda, type = "n", scaling = "none")
orditorp(growth_rda, display='sp', cex=1.2, scaling="none")
text(growth_rda, display='bp',  cex=.8, lwd = .5)

RsquareAdj(growth_rda)
anova.cca(growth_rda,permutations = 999)
anova.cca(growth_rda, by="axis")
anova.cca(growth_rda, by="terms")


# Create a dataframe for ggplot
plot_data2 <- data.frame(
  x = scores(growth_rda, display = "sp", scaling = "none")[, 1],
  y = scores(growth_rda, display = "sp", scaling = "none")[, 2],
  species = rownames(scores(growth_rda, display = "sp"))
)

plot_data3 <- data.frame(
  predictor1 = scores(my_rda, display = "bp")[, 1],
  predictor2 = scores(my_rda, display = "bp")[, 2],
  guild = c("Carnivore", "Corallivore", "Herbivore", "Omnivore")  # Add your trophic guilds here
)

plot_data2 <- rbind(plot_data2, rep(NA, ncol(plot_data2)))

df99 <- cbind(plot_data2, plot_data3)

df99$species <- gsub("Stress Tolerant", "Tolerant", df99$species)

# Create a ggplot scatter plot
p2 <- ggplot(df99, aes(x, y)) +
  #geom_point(aes(color = species)) +
  theme_minimal() +
  labs(title = "", x="RDA 1", y="RDA 2") +
  theme(legend.position = "none") +
  theme_classic() +
  my_theme

p2


# Add arrows for predictor loadings with labels
p2 <- p2 + geom_segment(
  aes(x = 0, y = 0, xend = predictor1, yend = predictor2),
  arrow = arrow(length = unit(0.2, "cm")),
  color = "red4", size = 1
) +
  geom_text(aes(label = species), size = 8, color = "navy", vjust = 0.5, hjust = 0.2) +
  geom_label(aes(x = predictor1, y = predictor2, label = guild), size = 3, color = "red4", fontface = "bold", hjust =1.1, vjust=.5)
p2

png(file=file.path(output_directory, "RDA_genera_LH_FIGURE.png"), height = 2500, width = 5500, res = 350)
p1 + p2 + plot_annotation(tag_levels = "A")  &
  theme(plot.tag = element_text(size = 30, face = "bold"))
dev.off()
