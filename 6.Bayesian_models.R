########################################################################################
############################# fish benthos relationship ################################
########################################################################################

# objective: Perform Bayesian models 
# Author: Jack Johnson (jackvjohnson@hotmail.com)
# Date created: July 2023
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

coral_cover_df <- read.csv(file=file.path(tidy_wd, "CORALCOVER_TIDY.csv"))
coral_raw_df <- read.csv(file=file.path(tidy_wd, "CORALRAW_TIDY.csv"))
algae_df <- read.csv(file=file.path(tidy_wd, "ALGAE_TIDY.csv"))
recruits_df <- read.csv(file=file.path(tidy_wd, "RECRUITS_TIDY.csv"))
fish_df <- read.csv(file=file.path(tidy_wd, "FISH_TIDY.csv"))

coral_cover_df <- coral_cover_df[,-c(1,2,4)]
coral_raw_df <- coral_raw_df[,-c(1,2,6)]
algae_df <- algae_df[,-c(1,2,4)]
recruits_df <- recruits_df[,-c(1,2,4)]
fish_df <- fish_df[,-c(1,2,7)]

table(fish_df$fish_spp)

# divide recruits by 4 for 2020 because some **** used a 1x1m quadrat 
recruits_df <- recruits_df %>%
  mutate(total_recruits = ifelse(Year == 2020 | Year == 2019, total_recruits / 4, total_recruits))


recruits_df$total_recruits <- round(recruits_df$total_recruits)

#########################################################################################
################################ fish and coral cover ###################################
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

coral_cover_df <- coral_cover_df[!is.na(coral_cover_df$Perc_cover),]

df2 <- fish_df %>%
  group_by(fish_spp, Site_Name, Year) %>%
  summarise(total_abundance = sum(count))

df3 <- df2 %>%
  pivot_wider(names_from = fish_spp, values_from = total_abundance)
df3[is.na(df3)] <- 0

mat1 <- as.matrix(df3[,-c(1:2)])
test <- diversity(mat1)

fish_sum_df <- fish_df %>%
  group_by(Year,Site_Name) %>%
  summarise(mean_biomass = mean(biomass), 
            sum_biomass = sum(biomass),
            richness = n_distinct(fish_spp),
            sum_abundance = sum(count),
            mean_abundance = mean(count))

fish_sum_df$shannon_div <- test

coral_cover_sum_df <- coral_cover_df %>%
  group_by(Year, Site_Name) %>%
  summarise(mean_coral_cover = mean(Perc_cover))

master_df <- left_join(fish_sum_df, coral_cover_sum_df, by=c("Site_Name", "Year"))

#########################################################################################
##################################### add algae #########################################

algae_sum_df <- algae_df %>%
  group_by(Year, Site_Name) %>%
  summarise(mean_macroalgae_cover = mean(prop_macro))

master_df <- left_join(master_df, algae_sum_df, by=c("Site_Name", "Year"))

#########################################################################################
################################### add recruits ########################################

recruits_sum_df <- recruits_df %>%
  group_by(Year, Site_Name) %>%
  summarise(total_recruits = sum(total_recruits))

master_df <- left_join(master_df,recruits_sum_df, by=c("Site_Name", "Year"))

#########################################################################################
################################ coral surface area #####################################

coral_area_df <- coral_raw_df %>%
  group_by(Year, Site_Name) %>%
  summarise(mean_height = mean(height),
            mean_SA = mean(coral_SA))

master_df <- left_join(master_df,coral_area_df, by=c("Site_Name", "Year"))

# not for one second to i believe sailfin mini wall had 0% macroalgae. Have to exclude this 
master_df <- subset(master_df, !(Year == 2012 & Site_Name == "Sailfin Miniwall"))



########################################################################################
################################## check collinearity ##################################
names(master_df)

cor_matrix <- subset(master_df, select=-c(Year, Site_Name))
cor_matrix <- na.omit(cor_matrix)
cor_matrix <- cor(cor_matrix, method = "spearman")

Supplementary <- "Supplementary"

png(file=file.path(output_directory, Supplementary, "Collinearity.png"), height = 3000, width = 3000, res = 350)
ggcorrplot(cor_matrix, hc.order = TRUE,
           type = "lower", lab = TRUE) +
  theme_classic() +
  my_theme +
  labs(x="", y="") +
  theme(axis.text.x = element_text(angle =45, hjust =1, vjust = 1))
dev.off()

#######################################################################################

df_m1 <- master_df
df_m1 <- subset(df_m1, select=-c(total_recruits))
df_m1 <- na.omit(df_m1)

########################################################################################
########################### does coral cover predict fish ##############################
options(mc.cores=parallel::detectCores())

hist(df_m1$mean_biomass)
hist(round(df_m1$shannon_div*100))
hist(df_m1$richness)

hist(df_m1$mean_coral_cover)
hist(df_m1$mean_macroalgae_cover*100)
hist(scale(df_m1$mean_SA))

m_fishfromcoral <- brm(mvbind(round(mean_biomass), round(shannon_div*100), richness) ~ scale(mean_coral_cover) + scale(mean_macroalgae_cover*100) + scale(mean_SA) + (1|Site_Name),
                   data = df_m1,
                   family = negbinomial(),
                   warmup = 1500,
                   iter = 3000, 
                   chains = 4,
                   cores = 4,
                   control = list(adapt_delta = 0.99))

summary(m_fishfromcoral)

mcmc_plot(m_fishfromcoral, type = "trace")
mcmc_plot(m_fishfromcoral, var=c("b_roundmeanbiomass_scalemean_coral_cover", "b_roundmeanbiomass_scalemean_macroalgae_coverMU100", "b_roundmeanbiomass_scalemean_SA", "b_roundshannondiv100_scalemean_coral_cover", "b_roundshannondiv100_scalemean_macroalgae_coverMU100", "b_roundshannondiv100_scalemean_SA", "b_richness_scalemean_coral_cover", "b_richness_scalemean_macroalgae_coverMU100","b_richness_scalemean_SA"))

pp_check(m_fishfromcoral, resp='roundmeanbiomass')
pp_check(m_fishfromcoral, resp='roundshannondiv100')
pp_check(m_fishfromcoral, resp='richness')

bayes_R2(m_fishfromcoral)

########################################################################################
########################### Fish predicting benthic ####################################

# coral

coral_fish_df <- master_df[,c(1:9)]
coral_fish_df <- coral_fish_df[!is.na(coral_fish_df$mean_coral_cover),]
hist(coral_fish_df$mean_coral_cover)

prior_CoralFish <- get_prior(mean_coral_cover/100 ~ scale(richness) + scale(mean_biomass) + scale(shannon_div) + (1|Site_Name), data = coral_fish_df, family = "beta")

m_CoralFish <- brm(mean_coral_cover/100 ~ scale(richness) + scale(mean_biomass) + scale(shannon_div) + (1|Site_Name),
                   data = coral_fish_df,
                   family = "beta",
                   warmup = 1000,
                   iter = 2000, 
                   chains = 4,
                   cores = 4,
                   prior = prior_CoralFish,
                   control = list(adapt_delta = 0.99))

summary(m_CoralFish)

mcmc_plot(m_CoralFish, type = "trace")
mcmc_plot(m_CoralFish, var = c("b_scalerichness", "b_scalemean_biomass", "b_scaleshannon_div"))
pp_check(m_CoralFish)

########################################################################################

# macroalgae

algae_fish_df <- master_df[,c(1:8,10)]
algae_fish_df <- algae_fish_df[!is.na(algae_fish_df$mean_macroalgae_cover),]
hist(algae_fish_df$mean_macroalgae_cover)

prior_algaeFish <- get_prior(mean_macroalgae_cover ~ scale(richness) + scale(mean_biomass) + scale(shannon_div) + (1|Site_Name), data = algae_fish_df, family = "beta")

m_algaeFish <- brm(mean_macroalgae_cover ~ scale(richness) + scale(mean_biomass) + scale(shannon_div) + (1|Site_Name),
                   data = algae_fish_df,
                   family = "beta",
                   warmup = 1000,
                   iter = 2000, 
                   chains = 4,
                   cores = 4,
                   prior = prior_algaeFish,
                   control = list(adapt_delta = 0.99))

summary(m_algaeFish)

mcmc_plot(m_algaeFish, type = "trace")
mcmc_plot(m_algaeFish, var = c("b_scalerichness", "b_scalemean_biomass", "b_scaleshannon_div"))
pp_check(m_algaeFish)

########################################################################################

# coral surface area

coral_SA_fish_df <- master_df[,c(1:8,13)]
coral_SA_fish_df <- coral_SA_fish_df[!is.na(coral_SA_fish_df$mean_SA),]
hist(coral_SA_fish_df$mean_SA)
hist(log1p(coral_SA_fish_df$mean_SA))

prior_coral_SAFish <- get_prior(log1p(mean_SA) ~ scale(richness) + scale(mean_biomass) + scale(shannon_div) + (1|Site_Name), data = coral_SA_fish_df, family = gaussian())

m_coral_SAFish <- brm(log1p(mean_SA) ~ scale(richness) + scale(mean_biomass) + scale(shannon_div) + (1|Site_Name),
                   data = coral_SA_fish_df,
                   family = gaussian(),
                   warmup = 1000,
                   iter = 2000, 
                   chains = 4,
                   cores = 4,
                   prior = prior_coral_SAFish,
                   control = list(adapt_delta = 0.99))

summary(m_coral_SAFish)

mcmc_plot(m_coral_SAFish, type = "trace")
mcmc_plot(m_coral_SAFish, var = c("b_scalerichness", "b_scalemean_biomass", "b_scaleshannon_div"))
pp_check(m_coral_SAFish)

# coral recruits 

recruits_fish_df <- master_df[,c(1:8,11)]
recruits_fish_df <- recruits_fish_df[!is.na(recruits_fish_df$total_recruits),]
hist(recruits_fish_df$total_recruits)

prior_recruitsFish <- get_prior(total_recruits ~ scale(richness) + scale(mean_biomass) + scale(shannon_div) + (1|Site_Name), data = recruits_fish_df, family = negbinomial())

m_recruitsFish <- brm(total_recruits ~ scale(richness) + scale(mean_biomass) + scale(shannon_div) + (1|Site_Name),
                      data = recruits_fish_df,
                      family = negbinomial(),
                      warmup = 1000,
                      iter = 2000, 
                      chains = 4,
                      cores = 4,
                      prior = prior_recruitsFish,
                      control = list(adapt_delta = 0.99))

summary(m_recruitsFish)

mcmc_plot(m_recruitsFish, type = "trace")
mcmc_plot(m_recruitsFish, var = c("b_scalerichness", "b_scalemean_biomass", "b_scaleshannon_div"))
pp_check(m_recruitsFish)

#####################################################################################
########################### Influence of herbivores  ################################

herb_df <-subset(fish_df, foodweb == "Herbivore")
herb_sum_df <- herb_df %>%
  group_by(Year,Site_Name) %>%
  summarise(herb_biomass = mean(biomass), 
            herb_richness = n_distinct(fish_spp))


herb_df <- herb_df[!is.na(herb_df$fish_spp),]
herb_df <- herb_df[!is.na(herb_df$count),]

herb2 <- herb_df %>%
  group_by(fish_spp, Site_Name, Year) %>%
  summarise(total_abundance = sum(count))

herb3 <- herb2 %>%
  pivot_wider(names_from = fish_spp, values_from = total_abundance)
herb3[is.na(herb3)] <- 0

herbmat <- as.matrix(herb3[,-c(1:2)])

herb_shannon <- diversity(herbmat)

herb_sum_df$herb_shannon <- herb_shannon

master_df <- left_join(master_df, herb_sum_df, by=c("Site_Name", "Year"))

# herbivore with coral 

herb_coral_df <- master_df[,c(1,2,9,14:16)]
herb_coral_df <- na.omit(herb_coral_df)
hist(herb_coral_df$mean_coral_cover)

prior_herbcoral <- get_prior(mean_coral_cover/100 ~ scale(herb_richness) + scale(herb_biomass) + scale(herb_shannon) + (1|Site_Name), data = herb_coral_df, family = "beta")

m_herbcoral <- brm(mean_coral_cover/100 ~ 
                     scale(herb_richness) + scale(herb_biomass) + scale(herb_shannon) + 
                     (1|Site_Name), 
                      data = herb_coral_df, 
                      family = "beta",
                      warmup = 1000,
                      iter = 2000, 
                      chains = 4,
                      cores = 4,
                      prior = prior_herbcoral,
                      control = list(adapt_delta = 0.99))

summary(m_herbcoral)

mcmc_plot(m_herbcoral, type = "trace")
mcmc_plot(m_herbcoral, var = c("b_scaleherb_richness", "b_scaleherb_biomass", "b_scaleherb_shannon"))
pp_check(m_herbcoral)

# herbivore with macroalgae 

herb_algae_df <- master_df[,c(1,2,10,14:16)]
herb_algae_df <- na.omit(herb_algae_df)
hist(herb_algae_df$mean_macroalgae_cover)

prior_herbalgae <- get_prior(mean_macroalgae_cover ~ scale(herb_richness) + scale(herb_biomass) + scale(herb_shannon) + (1|Site_Name), data = herb_algae_df, family = "beta")

m_herbalgae <- brm(mean_macroalgae_cover ~ 
                     scale(herb_richness) + scale(herb_biomass) + scale(herb_shannon) + 
                     (1|Site_Name), 
                   data = herb_algae_df, 
                   family = "beta",
                   warmup = 1000,
                   iter = 2000, 
                   chains = 4,
                   cores = 4,
                   prior = prior_herbalgae,
                   control = list(adapt_delta = 0.99))

summary(m_herbalgae)

mcmc_plot(m_herbalgae, type = "trace")
mcmc_plot(m_herbalgae, var = c("b_scaleherb_richness", "b_scaleherb_biomass", "b_scaleherb_shannon"))
pp_check(m_herbalgae)

# coral recruits

herb_recruits_df <- master_df[,c(1,2,11,14:16)]
herb_recruits_df <- na.omit(herb_recruits_df)
hist(herb_recruits_df$total_recruits)

prior_herbrecruits <- get_prior(total_recruits ~ scale(herb_richness) + scale(herb_biomass) + scale(herb_shannon) + (1|Site_Name), data = herb_recruits_df, family = negbinomial())

m_herbrecruits <- brm(total_recruits ~ 
                     scale(herb_richness) + scale(herb_biomass) + scale(herb_shannon) + 
                     (1|Site_Name), 
                   data = herb_recruits_df, 
                   family = negbinomial(),
                   warmup = 1000,
                   iter = 2000, 
                   chains = 4,
                   cores = 4,
                   prior = prior_herbrecruits,
                   control = list(adapt_delta = 0.99))

summary(m_herbrecruits)

mcmc_plot(m_herbrecruits, type = "trace")
mcmc_plot(m_herbrecruits, var = c("b_scaleherb_richness", "b_scaleherb_biomass", "b_scaleherb_shannon"))
pp_check(m_herbrecruits)

conditional_effects(m_herbrecruits)

# coral surface area

herb_SA_df <- master_df[,c(1,2,13,14:16)]
herb_SA_df <- na.omit(herb_SA_df)
hist(herb_SA_df$mean_SA)
hist(log1p(herb_SA_df$mean_SA))

prior_herbSA <- get_prior(log1p(mean_SA) ~ scale(herb_richness) + scale(herb_biomass) + scale(herb_shannon) + (1|Site_Name), data = herb_SA_df, family = gaussian())

m_herbSA <- brm(log1p(mean_SA) ~ 
                        scale(herb_richness) + scale(herb_biomass) + scale(herb_shannon) + 
                        (1|Site_Name), 
                      data = herb_SA_df, 
                      family = gaussian(),
                      warmup = 1000,
                      iter = 2000, 
                      chains = 4,
                      cores = 4,
                      prior = prior_herbSA,
                      control = list(adapt_delta = 0.99))

summary(m_herbSA)

mcmc_plot(m_herbSA, type = "trace")
mcmc_plot(m_herbSA, var = c("b_scaleherb_richness", "b_scaleherb_biomass", "b_scaleherb_shannon"))
pp_check(m_herbSA)

conditional_effects(m_herbSA)

#####################################################################################
############################### Parrotfish only #####################################

parrot_df <-subset(fish_df, common_family == "Parrotfish")
parrot_sum_df <- parrot_df %>%
  group_by(Year,Site_Name) %>%
  summarise(parrot_biomass = mean(biomass), 
            parrot_richness = n_distinct(fish_spp))


parrot_df <- parrot_df[!is.na(parrot_df$fish_spp),]
parrot_df <- parrot_df[!is.na(parrot_df$count),]

parrot2 <- parrot_df %>%
  group_by(fish_spp, Site_Name, Year) %>%
  summarise(total_abundance = sum(count))

parrot3 <- parrot2 %>%
  pivot_wider(names_from = fish_spp, values_from = total_abundance)
parrot3[is.na(parrot3)] <- 0

parrotmat <- as.matrix(parrot3[,-c(1:2)])

parrot_shannon <- diversity(parrotmat)

parrot_sum_df$parrot_shannon <- parrot_shannon

master_df <- left_join(master_df, parrot_sum_df, by=c("Site_Name", "Year"))


# parrotfish with coral 

parrot_coral_df <- master_df[,c(1,2,9,17:19)]
parrot_coral_df <- na.omit(parrot_coral_df)
hist(parrot_coral_df$mean_coral_cover)

prior_parrotcoral <- get_prior(mean_coral_cover/100 ~ scale(parrot_richness) + scale(parrot_biomass) + scale(parrot_shannon) + (1|Site_Name), data = parrot_coral_df, family = "beta")

m_parrotcoral <- brm(mean_coral_cover/100 ~ 
                     scale(parrot_richness) + scale(parrot_biomass) + scale(parrot_shannon) + 
                     (1|Site_Name), 
                   data = parrot_coral_df, 
                   family = "beta",
                   warmup = 1000,
                   iter = 2000, 
                   chains = 4,
                   cores = 4,
                   prior = prior_parrotcoral,
                   control = list(adapt_delta = 0.99))

summary(m_parrotcoral)

mcmc_plot(m_parrotcoral, type = "trace")
mcmc_plot(m_parrotcoral, var = c("b_scaleparrot_richness", "b_scaleparrot_biomass", "b_scaleparrot_shannon"))
pp_check(m_parrotcoral)

# parrot with algae

parrot_algae_df <- master_df[,c(1,2,10,17:19)]
parrot_algae_df <- na.omit(parrot_algae_df)
hist(parrot_algae_df$mean_macroalgae_cover)

prior_parrotalgae <- get_prior(mean_macroalgae_cover ~ scale(parrot_richness) + scale(parrot_biomass) + scale(parrot_shannon) + (1|Site_Name), data = parrot_algae_df, family = "beta")

m_parrotalgae <- brm(mean_macroalgae_cover ~ 
                     scale(parrot_richness) + scale(parrot_biomass) + scale(parrot_shannon) + 
                     (1|Site_Name), 
                   data = parrot_algae_df, 
                   family = "beta",
                   warmup = 1000,
                   iter = 2000, 
                   chains = 4,
                   cores = 4,
                   prior = prior_parrotalgae,
                   control = list(adapt_delta = 0.99))

summary(m_parrotalgae)

mcmc_plot(m_parrotalgae, type = "trace")
mcmc_plot(m_parrotalgae, var = c("b_scaleparrot_richness", "b_scaleparrot_biomass", "b_scaleparrot_shannon"))
pp_check(m_parrotalgae)

# parrot with recruits 

parrot_recruits_df <- master_df[,c(1,2,11,17:19)]
parrot_recruits_df <- na.omit(parrot_recruits_df)
hist(parrot_recruits_df$total_recruits)

prior_parrotrecruits <- get_prior(total_recruits ~ scale(parrot_richness) + scale(parrot_biomass) + scale(parrot_shannon) + (1|Site_Name), data = parrot_recruits_df, family = negbinomial())

m_parrotrecruits <- brm(total_recruits ~ 
                        scale(parrot_richness) + scale(parrot_biomass) + scale(parrot_shannon) + 
                        (1|Site_Name), 
                      data = parrot_recruits_df, 
                      family = negbinomial(),
                      warmup = 1000,
                      iter = 2000, 
                      chains = 4,
                      cores = 4,
                      prior = prior_parrotrecruits,
                      control = list(adapt_delta = 0.99))

summary(m_parrotrecruits)

mcmc_plot(m_parrotrecruits, type = "trace")
mcmc_plot(m_parrotrecruits, var = c("b_scaleparrot_richness", "b_scaleparrot_biomass", "b_scaleparrot_shannon"))
pp_check(m_parrotrecruits)

conditional_effects(m_parrotrecruits)

# parrot with coral SA

parrot_SA_df <- master_df[,c(1,2,13,17:19)]
parrot_SA_df <- na.omit(parrot_SA_df)
hist(parrot_SA_df$mean_SA)
hist(log1p(parrot_SA_df$mean_SA))

prior_parrotSA <- get_prior(log1p(mean_SA) ~ scale(parrot_richness) + scale(parrot_biomass) + scale(parrot_shannon) + (1|Site_Name), data = parrot_SA_df, family = gaussian())

m_parrotSA <- brm(log1p(mean_SA) ~ 
                  scale(parrot_richness) + scale(parrot_biomass) + scale(parrot_shannon) + 
                  (1|Site_Name), 
                data = parrot_SA_df, 
                family = gaussian(),
                warmup = 1000,
                iter = 2000, 
                chains = 4,
                cores = 4,
                prior = prior_parrotSA,
                control = list(adapt_delta = 0.99))

summary(m_parrotSA)

mcmc_plot(m_parrotSA, type = "trace")
mcmc_plot(m_parrotSA, var = c("b_scaleparrot_richness", "b_scaleparrot_biomass", "b_scaleparrot_shannon"))
pp_check(m_parrotSA)

bayes_R2(m_parrotSA)



#####################################################################################
################################# Corallivores ######################################

corallivore_df <-subset(fish_df, foodweb == "Corallivore")
corallivore_sum_df <- corallivore_df %>%
  group_by(Year,Site_Name) %>%
  summarise(corallivore_biomass = mean(biomass), 
            corallivore_richness = n_distinct(fish_spp))


corallivore_df <- corallivore_df[!is.na(corallivore_df$fish_spp),]
corallivore_df <- corallivore_df[!is.na(corallivore_df$count),]

corallivore2 <- corallivore_df %>%
  group_by(fish_spp, Site_Name, Year) %>%
  summarise(total_abundance = sum(count))

corallivore3 <- corallivore2 %>%
  pivot_wider(names_from = fish_spp, values_from = total_abundance)
corallivore3[is.na(corallivore3)] <- 0

corallivoremat <- as.matrix(corallivore3[,-c(1:2)])

corallivore_shannon <- diversity(corallivoremat)

corallivore_sum_df$corallivore_shannon <- corallivore_shannon

master_df <- left_join(master_df, corallivore_sum_df, by=c("Site_Name", "Year"))


# corallivorefish with coral 

corallivore_coral_df <- master_df[,c(1,2,9,20:22)]
corallivore_coral_df <- na.omit(corallivore_coral_df)
hist(corallivore_coral_df$mean_coral_cover)

prior_corallivorecoral <- get_prior(mean_coral_cover/100 ~ scale(corallivore_richness) + scale(corallivore_biomass) + scale(corallivore_shannon) + (1|Site_Name), data = corallivore_coral_df, family = "beta")

m_corallivorecoral <- brm(mean_coral_cover/100 ~ 
                       scale(corallivore_richness) + scale(corallivore_biomass) + scale(corallivore_shannon) + 
                       (1|Site_Name), 
                     data = corallivore_coral_df, 
                     family = "beta",
                     warmup = 1000,
                     iter = 2000, 
                     chains = 4,
                     cores = 4,
                     prior = prior_corallivorecoral,
                     control = list(adapt_delta = 0.99))

summary(m_corallivorecoral)

mcmc_plot(m_corallivorecoral, type = "trace")
mcmc_plot(m_corallivorecoral, var = c("b_scalecorallivore_richness", "b_scalecorallivore_biomass", "b_scalecorallivore_shannon"))
pp_check(m_corallivorecoral)

# corallivore with algae

corallivore_algae_df <- master_df[,c(1,2,10,20:22)]
corallivore_algae_df <- na.omit(corallivore_algae_df)
hist(corallivore_algae_df$mean_macroalgae_cover)

prior_corallivorealgae <- get_prior(mean_macroalgae_cover ~ scale(corallivore_richness) + scale(corallivore_biomass) + scale(corallivore_shannon) + (1|Site_Name), data = corallivore_algae_df, family = "beta")

m_corallivorealgae <- brm(mean_macroalgae_cover ~ 
                       scale(corallivore_richness) + scale(corallivore_biomass) + scale(corallivore_shannon) + 
                       (1|Site_Name), 
                     data = corallivore_algae_df, 
                     family = "beta",
                     warmup = 1000,
                     iter = 2000, 
                     chains = 4,
                     cores = 4,
                     prior = prior_corallivorealgae,
                     control = list(adapt_delta = 0.99))

summary(m_corallivorealgae)

mcmc_plot(m_corallivorealgae, type = "trace")
mcmc_plot(m_corallivorealgae, var = c("b_scalecorallivore_richness", "b_scalecorallivore_biomass", "b_scalecorallivore_shannon"))
pp_check(m_corallivorealgae)

# corallivore with recruits 

corallivore_recruits_df <- master_df[,c(1,2,11,20:22)]
corallivore_recruits_df <- na.omit(corallivore_recruits_df)
hist(corallivore_recruits_df$total_recruits)

prior_corallivorerecruits <- get_prior(total_recruits ~ scale(corallivore_richness) + scale(corallivore_biomass) + scale(corallivore_shannon) + (1|Site_Name), data = corallivore_recruits_df, family = negbinomial())

m_corallivorerecruits <- brm(total_recruits ~ 
                          scale(corallivore_richness) + scale(corallivore_biomass) + scale(corallivore_shannon) + 
                          (1|Site_Name), 
                        data = corallivore_recruits_df, 
                        family = negbinomial(),
                        warmup = 1000,
                        iter = 2000, 
                        chains = 4,
                        cores = 4,
                        prior = prior_corallivorerecruits,
                        control = list(adapt_delta = 0.99))

summary(m_corallivorerecruits)

mcmc_plot(m_corallivorerecruits, type = "trace")
mcmc_plot(m_corallivorerecruits, var = c("b_scalecorallivore_richness", "b_scalecorallivore_biomass", "b_scalecorallivore_shannon"))
pp_check(m_corallivorerecruits)

conditional_effects(m_corallivorerecruits)

# corallivore with coral SA

corallivore_SA_df <- master_df[,c(1,2,13,20:22)]
corallivore_SA_df <- na.omit(corallivore_SA_df)
hist(corallivore_SA_df$mean_SA)
hist(log1p(corallivore_SA_df$mean_SA))

prior_corallivoreSA <- get_prior(log1p(mean_SA) ~ scale(corallivore_richness) + scale(corallivore_biomass) + scale(corallivore_shannon) + (1|Site_Name), data = corallivore_SA_df, family = gaussian())

m_corallivoreSA <- brm(log1p(mean_SA) ~ 
                    scale(corallivore_richness) + scale(corallivore_biomass) + scale(corallivore_shannon) + 
                    (1|Site_Name), 
                  data = corallivore_SA_df, 
                  family = gaussian(),
                  warmup = 1000,
                  iter = 2000, 
                  chains = 4,
                  cores = 4,
                  prior = prior_corallivoreSA,
                  control = list(adapt_delta = 0.99))

summary(m_corallivoreSA)

mcmc_plot(m_corallivoreSA, type = "trace")
mcmc_plot(m_corallivoreSA, var = c("b_scalecorallivore_richness", "b_scalecorallivore_biomass", "b_scalecorallivore_shannon"))
pp_check(m_corallivoreSA)

bayes_R2(m_corallivoreSA)


#########################################################################################
##################################### export models #####################################

models <- "Models"

# benthic predicting fish
saveRDS(file = file.path(output_directory, models, 'Benthic_predict_fish.rds'), m_fishfromcoral)

# all fish predicting benthic 
saveRDS(file = file.path(output_directory, models, 'Fish_p_algae.rds'), m_algaeFish)
saveRDS(file = file.path(output_directory, models, 'Fish_p_coralSA.rds'), m_coral_SAFish)
saveRDS(file = file.path(output_directory, models, 'Fish_p_coralCover.rds'), m_CoralFish)
saveRDS(file = file.path(output_directory, models, 'Fish_p_recruits.rds'), m_recruitsFish)

# herbivores predicting benthic

saveRDS(file = file.path(output_directory, models, 'HerbAlgae.rds'), m_herbalgae)
saveRDS(file = file.path(output_directory, models, 'HerbCoral.rds'), m_herbcoral)
saveRDS(file = file.path(output_directory, models, 'HerbRecruits.rds'), m_herbrecruits)
saveRDS(file = file.path(output_directory, models, 'HerbSA.rds'), m_herbSA)

# corallivores predicting benthic 

saveRDS(file = file.path(output_directory, models, 'corallivoreAlgae.rds'), m_corallivorealgae)
saveRDS(file = file.path(output_directory, models, 'corallivoreCoral.rds'), m_corallivorecoral)
saveRDS(file = file.path(output_directory, models, 'corallivoreRecruits.rds'), m_corallivorerecruits)
saveRDS(file = file.path(output_directory, models, 'corallivoreSA.rds'), m_corallivoreSA)

# parrotfish predicting benthic 

saveRDS(file = file.path(output_directory, models, 'parrotAlgae.rds'), m_parrotalgae)
saveRDS(file = file.path(output_directory, models, 'parrotCoral.rds'), m_parrotcoral)
saveRDS(file = file.path(output_directory, models, 'parrotRecruits.rds'), m_parrotrecruits)
saveRDS(file = file.path(output_directory, models, 'parrotSA.rds'), m_parrotSA)


