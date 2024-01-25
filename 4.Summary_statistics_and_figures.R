########################################################################################
################################# AGGRA explore data  ##################################
########################################################################################

# objective: data exploration  
# Author: Jack Johnson (jackvjohnson@hotmail.com)
# Date created: July 2023
# Last edited: 

# clear workspace
rm(list=ls())

# libraries

library(tidyverse)
library(vegan)
library(patchwork)
library(mgcv)
library(performance)

# working directories 

tidy_wd <- "C:/Users/jackv/OneDrive - Central Caribbean Marine Institute, Inc/Projects/AGGRA/Data_tidy/TIDY_COMPLETE"

output_directory <-  "C:/Users/jackv/OneDrive - Central Caribbean Marine Institute, Inc/Projects/AGGRA/Fish benthos relationship/Output_directory"

########################################################################################
################################### read in files ######################################

coral_cover_df <- read.csv(file=file.path(tidy_wd, "CORALCOVER_TIDY.csv"))
coral_cover_df$Year <- as.factor(coral_cover_df$Year)

algae_df <- read.csv(file=file.path(tidy_wd, "ALGAE_TIDY.csv"))
algae_df$Year <- as.factor(algae_df$Year)
algae_df <- subset(algae_df, !(Year == 2012 & Site_Name == "Sailfin Miniwall"))
algae_df <- subset(algae_df, !(Year == 2009))

recruits_df <- read.csv(file=file.path(tidy_wd, "RECRUITS_TIDY.csv"))
recruits_df$Year <- as.factor(recruits_df$Year)


# divide recruits by 4 for 2019/2020 because some **** used a 1x1m quadrat rather than 25cmx25cm 
recruits_df <- recruits_df %>%
  mutate(total_recruits = ifelse(Year == 2020 | Year == 2019, total_recruits / 4, total_recruits))



########################################################################################

coral_df <- read.csv(file=file.path(tidy_wd, "CORALRAW_TIDY.csv"))
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

fish_df <- read.csv(file=file.path(tidy_wd, "FISH_TIDY.csv"))
summary(fish_df)
fish_df$biomass <- as.numeric(fish_df$biomass)
fish_df$density <- as.numeric(fish_df$density)
fish_df$count <- as.numeric(fish_df$count)
fish_df$year <- as.factor(fish_df$year)

########################################################################################
################################## theme for figures ###################################

my_theme <- theme_classic() +
  theme(axis.title.x = element_text(size = 20, color = "black"), axis.title.y = element_text(size = 20, color = "black"), text=element_text(size=16)) +
  theme(axis.text.y = element_text(size=16, color="black"), axis.text.x = element_text(size=16, color="black", angle = 0, vjust = .0)) +
  theme(legend.text = element_text(size = 16),
        legend.title = element_text(size = 18, face = "bold")) 

########################################################################################
################################## benthic figures #####################################

cover2 <- coral_cover_df %>%
  group_by(Year, Site_Name) %>%
  summarise(Perc_cover = mean(Perc_cover))

p1 <- ggplot(cover2, aes(Year, Perc_cover)) +
  geom_boxplot() +
  labs(x="Year", y="Coral cover (%)") +
  geom_smooth(method="loess", color = "#952ea0", size = 1.5, aes(group=1)) +
  my_theme +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank()) +
  theme(plot.title = element_text(size = 26, face = "bold", hjust = .5)) +
  ggtitle("Benthic metrics")
  #theme(axis.text.x = element_text(angle = 45, hjust = .5, vjust=.5))
p1

algae2 <- algae_df %>%
  group_by(Year, Site_Name) %>%
  summarise(prop_macro = mean(prop_macro))

p2 <- ggplot(algae2, aes(Year, prop_macro*100)) +
  geom_boxplot() +
  geom_smooth(method="loess", color = "#952ea0", size = 1.5, aes(group=1)) +
  labs(x="Year", y="Macroalgae cover (%)") +
  my_theme +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank())
 #theme(axis.text.x = element_text(angle = 45, hjust = .5, vjust=.5))
p2

recruits2 <- recruits_df %>%
  group_by(Year, Site_Name) %>%
  summarise(total=sum(total_recruits))

recruits_df$Year <- as.factor(recruits_df$Year)
p3 <- ggplot(recruits2, aes(Year, total)) +
  geom_boxplot() +
  geom_smooth(method="loess", color = "#952ea0", size = 1.5, aes(group=1)) +
  labs(x="Year", y="Coral recruits (n)") +
  my_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = .5, vjust=.5))
  #theme(axis.text.x = element_text(angle = 45, hjust = .5, vjust=.5))
p3


######################################################################################
################################## coral figures #####################################

# surface area
# height 
# shannon diversity 

coral_df2 <- coral_df %>%
  group_by(Year, Site) %>%
  summarise(mean_SA = mean(coral_SA), 
            mean_height = mean(height),
            sp_richness = n_distinct(Coral_spp),
            shan_diversity = diversity(table(Coral_spp), index = "shannon"),
            genus_richness = n_distinct(Coral_genus),
            shan_diversity_genus = diversity(table(Coral_genus), index = "shannon"))

#remove_outliers <- function(x, coef = 1.5) {
#  q1 <- quantile(x, 0.25)
#  q3 <- quantile(x, 0.75)
#  iqr <- q3 - q1
#  lower_bound <- q1 - coef * iqr
#  upper_bound <- q3 + coef * iqr
#  return(x[which(x >= lower_bound & x <= upper_bound)])
#}

hist(coral_df2$mean_SA)
hist(log1p(coral_df2$mean_SA))
hist(coral_df2$mean_height)

coral_df2 <- subset(coral_df2, mean_SA != 0)
coral_df2 <- coral_df2[!is.na(coral_df2$mean_SA),]

#coral_df2cleaned <-coral_df2 %>% 
#  filter(!mean_SA%in% remove_outliers(coral_df2$mean_SA))


p4 <- ggplot(coral_df2, aes(Year, (mean_height))) +
  geom_boxplot()
p4

p5 <- ggplot(coral_df2, aes(Year, log1p(mean_SA))) +
  geom_boxplot() +
  geom_smooth(method="loess", color = "#952ea0", size = 1.5, aes(group=1)) +
  ylab(expression(paste("Mean surface area (cm²)"))) +
  my_theme +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank()) + 
  theme(plot.title = element_text(size = 26, face = "bold", hjust = .5)) +
  ggtitle("Coral metrics")
p5

p6 <- ggplot(coral_df2, aes(Year, genus_richness)) +
  geom_boxplot() +
  geom_smooth(method="loess", color = "#952ea0", size = 1.5, aes(group=1)) +
  ylab("Genus richness (n)") +
  my_theme +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank())
  #theme(axis.text.x = element_text(angle = 45, hjust = .5, vjust=.5))
p6

p7 <- ggplot(coral_df2, aes(Year, shan_diversity_genus)) +
  geom_boxplot() +
  geom_smooth(method="loess", color = "#952ea0", size = 1.5, aes(group=1)) +
  ylab("Coral genus diversity (Shannon)") +
  my_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = .5, vjust=.5))
p7


fish_df2 <- fish_df %>%
  group_by(year, site, observer, mpa) %>%
  summarise(mean_biomass = mean(biomass),
            mean_density = mean(density), 
            total_abundance = sum(count),
            fish_richness = n_distinct(fish_spp),
            shan_diversity = diversity(table(fish_spp), index = "shannon"))

ggplot(fish_df2, aes(mean_biomass, mean_density)) +
  geom_point()

cor.test(fish_df2$mean_biomass, fish_df2$mean_density, method = "spearman")

ggplot(fish_df2, aes(year, mean_density)) +
  geom_boxplot() +
  ylab("Mean density") +
  geom_smooth(method="loess", color = "#952ea0", size = 1.5, aes(group=1)) +
  my_theme +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank()) +
  theme(plot.title = element_text(size = 26, face = "bold", hjust = .5)) 

p8 <- ggplot(fish_df2, aes(year, mean_biomass/1000)) +
  geom_boxplot() +
  ylab("Mean biomass (kg)") +
  geom_smooth(method="loess", color = "#952ea0", size = 1.5, aes(group=1)) +
  my_theme +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank()) +
  theme(plot.title = element_text(size = 26, face = "bold", hjust = .5)) +
  ggtitle("Fish metrics")
p8

p9 <- ggplot(fish_df2, aes(year, fish_richness)) +
  geom_boxplot() +
  geom_smooth(method="loess", color = "#952ea0", size = 1.5, aes(group=1)) +
  ylab("Fish species richness (n)") +
  my_theme +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank())
p9

p10 <- ggplot(fish_df2, aes(year, shan_diversity)) +
  geom_boxplot() +
  geom_smooth(method="loess", color = "#952ea0", size = 1.5, aes(group=1)) +
  ylab("Fish species diversity (Shannon)") +
  my_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = .5, vjust=.5))
p10

# export figure 
png(file=file.path(output_directory, "summary_boxplots_FIGURE_2.png"), height = 5000, width = 6500, res = 350)
(p1/p2/p3 | p5/p6/p7 | p8/p9/p10) + plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(size = 30, face = "bold"))
dev.off()

hist(coral_cover_df$Perc_cover)
m1 <- gam(round(Perc_cover) ~ s(as.numeric(Year)), data = cover2, family = poisson())
summary(m1)
check_model(m1)
m1sum <- summary(m1)

hist(algae_df$prop_macro)
m2 <- gam(round(prop_macro*100) ~ s(as.numeric(Year)), data = algae2, family = poisson())
summary(m2)
check_model(m2)
m2sum <- summary(m2)

hist(recruits_df$total)
m3 <- gam(round(total) ~ s(as.numeric(Year)), data = recruits2, family = poisson())
summary(m3)
check_model(m3)
m3sum <- summary(m3)

hist(coral_df2$mean_SA)
hist(log1p(coral_df2$mean_SA))
m4 <- gam(log1p(mean_SA) ~s(as.numeric(Year)), data = coral_df2, family = gaussian())
summary(m4)
check_model(m4)
m4sum <- summary(m4)

hist(coral_df2$genus_richness)
shapiro.test(coral_df2$genus_richness)
m5 <- gam(genus_richness ~ s(as.numeric(Year)), data = coral_df2, family = poisson())
summary(m5)
check_model(m5)
m5sum <- summary(m5)

hist(coral_df2$shan_diversity_genus)
m6 <- gam(round(shan_diversity_genus*10) ~ s(as.numeric(Year)), data = coral_df2, family = poisson())
summary(m6)
check_model(m6)
m6sum <- summary(m6)

hist(fish_df2$mean_biomass)
hist(log1p(fish_df2$mean_biomass))
shapiro.test(log1p(fish_df2$mean_biomass))
m7 <- gam(mean_biomass ~ s(as.numeric(year)), data = fish_df2, family = gaussian())
summary(m7)
check_model(m7)
m7sum <- summary(m7)

hist(fish_df2$fish_richness)
m8 <- gam(fish_richness ~ s(as.numeric(year)), data = fish_df2, family = poisson())
summary(m8)
check_model(m8)
m8sum <- summary(m8)

hist(fish_df2$shan_diversity)
m9 <- gam(round(shan_diversity*10) ~ s(as.numeric(year)), data = fish_df2, family = poisson())
summary(m9)
check_model(m9)
m9sum <-summary(m9)


msumbind <- rbind(m1sum$s.table, m2sum$s.table, m3sum$s.table, m4sum$s.table, m5sum$s.table, m6sum$s.table, m7sum$s.table, m8sum$s.table, m9sum$s.table)
msumbind <- data.frame(msumbind)

write.csv(msumbind, file=file.path(output_directory, "S_table.csv"))
