########################################################################################
######################## Coefficient plots from bayes models ###########################
########################################################################################

# objective: Figures 3 and 4 for ecology letters paper - coefficient plots 
# Author: Jack Johnson (jackvjohnson@hotmail.com)
# Date created: November 2023
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



# benthic predicting fish
m1 <- readRDS(file = file.path(output_directory, "Models", 'Benthic_predict_fish.rds'))

m1_coeffs <- posterior_summary(m1, digits = 0.6, probs=c(0.95, 0.05,0.8, 0.2,0.5))
m1_coeffs <- as.data.frame(m1_coeffs[4:12,])
m1_coeffs <- rownames_to_column(m1_coeffs)

m1_coeffs <- mutate(m1_coeffs,
                         predictor = case_when(
                           row_number() %in% c(1,4,7) ~ "Coral Cover",
                           row_number() %in% c(2,5,8) ~ "Macroalgae cover",
                           row_number() %in% c(3,6,9) ~ "Coral Surface Area"
                         )
)

m1_coeffs <- mutate(m1_coeffs,
                    group = case_when(
                      row_number() %in% 1:3 ~ "Biomass",
                      row_number() %in% 4:6 ~ "Diversity",
                      row_number() %in% 7:9 ~ "Richness"
                    )
)


p1 <- ggplot(m1_coeffs, aes(Estimate, predictor, fill = group, color = group)) +
  geom_vline(aes(xintercept = 0), size = .25, linetype = "dashed") +         
  geom_errorbarh(aes(xmax = Q95, xmin =Q5), linewidth = 1.3, height = 0, color = "light grey", position = position_dodge(width = 0.5)) +
  geom_errorbarh(aes(xmax = Q80, xmin =Q20), linewidth = 2, height = 0, color = "Dark grey", position = position_dodge(width = 0.5)) +
  geom_point(size = 3, position = position_dodge(width=0.5)) +
  scale_color_fish_d(option = "Gramma_loreto") +
  labs(color = "Response", fill = "Response", x = "Model coefficient", y = "") +
  theme_classic() +
  my_theme
p1

# export figure 
png(file=file.path(output_directory, "Benthic_predict_fish_FIGURE.png"), height = 2500, width = 3500, res = 350)
p1 
dev.off()

bayes_R2(m1)


# all fish predicting benthic 
m_algae_all <- readRDS(file = file.path(output_directory, "Models", 'Fish_p_algae.rds'))
m_coralSA_all <- readRDS(file = file.path(output_directory, "Models", 'Fish_p_coralSA.rds'))
m_coralCov_all <- readRDS(file = file.path(output_directory, "Models", 'Fish_p_coralCover.rds'))
m_recruits_all <- readRDS(file = file.path(output_directory, "Models", 'Fish_p_recruits.rds'))

# herbivores predicting benthic

m_algae_herb <- readRDS(file = file.path(output_directory, "Models", 'HerbAlgae.rds'))
m_coralSA_herb <- readRDS(file = file.path(output_directory, "Models", 'HerbCoral.rds'))
m_coralCov_herb <- readRDS(file = file.path(output_directory, "Models", 'HerbRecruits.rds'))
m_recruits_herb <- readRDS(file = file.path(output_directory, "Models", 'HerbSA.rds'))

# corallivores predicting benthic 

m_algae_corallivore <- readRDS(file = file.path(output_directory, "Models", 'corallivoreAlgae.rds'))
m_coralSA_corallivore <- readRDS(file = file.path(output_directory, "Models", 'corallivoreCoral.rds'))
m_coralCov_corallivore <- readRDS(file = file.path(output_directory, "Models", 'corallivoreRecruits.rds'))
m_recruits_corallivore <- readRDS(file = file.path(output_directory, "Models", 'corallivoreSA.rds'))

# parrotfish predicting benthic 

m_algae_parrot <- readRDS(file = file.path(output_directory, "Models", 'parrotAlgae.rds'))
m_coralSA_parrot <- readRDS(file = file.path(output_directory, "Models", 'parrotCoral.rds'))
m_coralCov_parrot <- readRDS(file = file.path(output_directory, "Models", 'parrotRecruits.rds'))
m_recruits_parrot <- readRDS(file = file.path(output_directory, "Models", 'parrotSA.rds'))


################################################################################################
################################## extract model coefficients ##################################

# algae

coeffs_algae_all  <- posterior_summary(m_algae_all, digits = 0.6, probs=c(0.95, 0.05,0.8, 0.2,0.5))
coeffs_algae_all <- as.data.frame(coeffs_algae_all[2:4,])
coeffs_algae_all <- rownames_to_column(coeffs_algae_all)
coeffs_algae_all$group <- "All fish"

coeffs_algae_herb  <- posterior_summary(m_algae_herb, digits = 0.6, probs=c(0.95, 0.05,0.8, 0.2,0.5))
coeffs_algae_herb <- as.data.frame(coeffs_algae_herb[2:4,])
coeffs_algae_herb <- rownames_to_column(coeffs_algae_herb)
coeffs_algae_herb$group <- "Herbivores"

coeffs_algae_corallivore  <- posterior_summary(m_algae_corallivore, digits = 0.6, probs=c(0.95, 0.05,0.8, 0.2,0.5))
coeffs_algae_corallivore <- as.data.frame(coeffs_algae_corallivore[2:4,])
coeffs_algae_corallivore <- rownames_to_column(coeffs_algae_corallivore)
coeffs_algae_corallivore$group <- "Corallivores"

coeffs_algae_parrot  <- posterior_summary(m_algae_parrot, digits = 0.6, probs=c(0.95, 0.05,0.8, 0.2,0.5))
coeffs_algae_parrot <- as.data.frame(coeffs_algae_parrot[2:4,])
coeffs_algae_parrot <- rownames_to_column(coeffs_algae_parrot)
coeffs_algae_parrot$group <- "Parrotfish"

coeffs_algae <- rbind(coeffs_algae_all, coeffs_algae_corallivore, coeffs_algae_herb, coeffs_algae_parrot)
predictor_sequence <- rep(c("Richness", "Biomass", "Diversity"), length.out = nrow(coeffs_algae))
coeffs_algae$predictor <- predictor_sequence

p_algae <- ggplot(subset(coeffs_algae, predictor %in% c("Richness", "Biomass", "Diversity")), aes(Estimate, predictor, fill = group, color = group)) +
  geom_vline(aes(xintercept = 0), size = .25, linetype = "dashed") +         
  geom_errorbarh(aes(xmax = Q95, xmin =Q5), linewidth = 1.3, height = 0, color = "light grey", position = position_dodge(width = 0.5)) +
  geom_errorbarh(aes(xmax = Q80, xmin =Q20), linewidth = 2, height = 0, color = "Dark grey", position = position_dodge(width = 0.5)) +
  geom_point(size = 3, position = position_dodge(width=0.5)) +
  scale_color_fish_d(option = "Gramma_loreto") +
  labs(color = "Group", fill = "Group", x = "Model coefficient", y = "", title = "Algae Cover") +
  theme_classic() +
  my_theme

p_algae


# coral 

coeffs_coralCov_all  <- posterior_summary(m_coralCov_all, digits = 0.6, probs=c(0.95, 0.05,0.8, 0.2,0.5))
coeffs_coralCov_all <- as.data.frame(coeffs_coralCov_all[2:4,])
coeffs_coralCov_all <- rownames_to_column(coeffs_coralCov_all)
coeffs_coralCov_all$group <- "All fish"

coeffs_coralCov_herb  <- posterior_summary(m_coralCov_herb, digits = 0.6, probs=c(0.95, 0.05,0.8, 0.2,0.5))
coeffs_coralCov_herb <- as.data.frame(coeffs_coralCov_herb[2:4,])
coeffs_coralCov_herb <- rownames_to_column(coeffs_coralCov_herb)
coeffs_coralCov_herb$group <- "Herbivores"

coeffs_coralCov_corallivore  <- posterior_summary(m_coralCov_corallivore, digits = 0.6, probs=c(0.95, 0.05,0.8, 0.2,0.5))
coeffs_coralCov_corallivore <- as.data.frame(coeffs_coralCov_corallivore[2:4,])
coeffs_coralCov_corallivore <- rownames_to_column(coeffs_coralCov_corallivore)
coeffs_coralCov_corallivore$group <- "Corallivores"

coeffs_coralCov_parrot  <- posterior_summary(m_coralCov_parrot, digits = 0.6, probs=c(0.95, 0.05,0.8, 0.2,0.5))
coeffs_coralCov_parrot <- as.data.frame(coeffs_coralCov_parrot[2:4,])
coeffs_coralCov_parrot <- rownames_to_column(coeffs_coralCov_parrot)
coeffs_coralCov_parrot$group <- "Parrotfish"

coeffs_coralCov <- rbind(coeffs_coralCov_all, coeffs_coralCov_corallivore, coeffs_coralCov_herb, coeffs_coralCov_parrot)
predictor_sequence <- rep(c("Richness", "Biomass", "Diversity"), length.out = nrow(coeffs_coralCov))
coeffs_coralCov$predictor <- predictor_sequence

p_coralCov <- ggplot(coeffs_coralCov, aes(Estimate, predictor, fill = group, color = group)) +
  geom_vline(aes(xintercept = 0), size = .25, linetype = "dashed") +         
  geom_errorbarh(aes(xmax = Q95, xmin =Q5), linewidth = 1.3, height = 0, color = "light grey", position = position_dodge(width = 0.5)) +
  geom_errorbarh(aes(xmax = Q80, xmin =Q20), linewidth = 2, height = 0, color = "Dark grey", position = position_dodge(width = 0.5)) +
  geom_point(size = 3, position = position_dodge(width=0.5)) +
  scale_color_fish_d(option = "Gramma_loreto") +
  labs(color = "Group", fill = "Group", x = "Model coefficient", y = "", title = "Coral Cover") +
  theme_classic() +
  my_theme

p_coralCov

# recruits

coeffs_recruits_all  <- posterior_summary(m_recruits_all, digits = 0.6, probs=c(0.95, 0.05,0.8, 0.2,0.5))
coeffs_recruits_all <- as.data.frame(coeffs_recruits_all[2:4,])
coeffs_recruits_all <- rownames_to_column(coeffs_recruits_all)
coeffs_recruits_all$group <- "All fish"

coeffs_recruits_herb  <- posterior_summary(m_recruits_herb, digits = 0.6, probs=c(0.95, 0.05,0.8, 0.2,0.5))
coeffs_recruits_herb <- as.data.frame(coeffs_recruits_herb[2:4,])
coeffs_recruits_herb <- rownames_to_column(coeffs_recruits_herb)
coeffs_recruits_herb$group <- "Herbivores"

coeffs_recruits_corallivore  <- posterior_summary(m_recruits_corallivore, digits = 0.6, probs=c(0.95, 0.05,0.8, 0.2,0.5))
coeffs_recruits_corallivore <- as.data.frame(coeffs_recruits_corallivore[2:4,])
coeffs_recruits_corallivore <- rownames_to_column(coeffs_recruits_corallivore)
coeffs_recruits_corallivore$group <- "Corallivores"

coeffs_recruits_parrot  <- posterior_summary(m_recruits_parrot, digits = 0.6, probs=c(0.95, 0.05,0.8, 0.2,0.5))
coeffs_recruits_parrot <- as.data.frame(coeffs_recruits_parrot[2:4,])
coeffs_recruits_parrot <- rownames_to_column(coeffs_recruits_parrot)
coeffs_recruits_parrot$group <- "Parrotfish"

coeffs_recruits <- rbind(coeffs_recruits_all, coeffs_recruits_corallivore, coeffs_recruits_herb, coeffs_recruits_parrot)
predictor_sequence <- rep(c("Richness", "Biomass", "Diversity"), length.out = nrow(coeffs_recruits))
coeffs_recruits$predictor <- predictor_sequence

p_recruits <- ggplot(coeffs_recruits, aes(Estimate, predictor, fill = group, color = group)) +
  geom_vline(aes(xintercept = 0), size = .25, linetype = "dashed") +         
  geom_errorbarh(aes(xmax = Q95, xmin =Q5), linewidth = 1.3, height = 0, color = "light grey", position = position_dodge(width = 0.5)) +
  geom_errorbarh(aes(xmax = Q80, xmin =Q20), linewidth = 2, height = 0, color = "Dark grey", position = position_dodge(width = 0.5)) +
  geom_point(size = 3, position = position_dodge(width=0.5)) +
  scale_color_fish_d(option = "Gramma_loreto") +
  labs(color = "Group", fill = "Group", x = "Model coefficient", y = "", title = "Total Recruits") +
  theme_classic() +
  my_theme

p_recruits

# coral SA

coeffs_coralSA_all  <- posterior_summary(m_coralSA_all, digits = 0.6, probs=c(0.95, 0.05,0.8, 0.2,0.5))
coeffs_coralSA_all <- as.data.frame(coeffs_coralSA_all[2:4,])
coeffs_coralSA_all <- rownames_to_column(coeffs_coralSA_all)
coeffs_coralSA_all$group <- "All fish"

coeffs_coralSA_herb  <- posterior_summary(m_coralSA_herb, digits = 0.6, probs=c(0.95, 0.05,0.8, 0.2,0.5))
coeffs_coralSA_herb <- as.data.frame(coeffs_coralSA_herb[2:4,])
coeffs_coralSA_herb <- rownames_to_column(coeffs_coralSA_herb)
coeffs_coralSA_herb$group <- "Herbivores"

coeffs_coralSA_corallivore  <- posterior_summary(m_coralSA_corallivore, digits = 0.6, probs=c(0.95, 0.05,0.8, 0.2,0.5))
coeffs_coralSA_corallivore <- as.data.frame(coeffs_coralSA_corallivore[2:4,])
coeffs_coralSA_corallivore <- rownames_to_column(coeffs_coralSA_corallivore)
coeffs_coralSA_corallivore$group <- "Corallivores"

coeffs_coralSA_parrot  <- posterior_summary(m_coralSA_parrot, digits = 0.6, probs=c(0.95, 0.05,0.8, 0.2,0.5))
coeffs_coralSA_parrot <- as.data.frame(coeffs_coralSA_parrot[2:4,])
coeffs_coralSA_parrot <- rownames_to_column(coeffs_coralSA_parrot)
coeffs_coralSA_parrot$group <- "Parrotfish"

coeffs_coralSA <- rbind(coeffs_coralSA_all, coeffs_coralSA_corallivore, coeffs_coralSA_herb, coeffs_coralSA_parrot)
predictor_sequence <- rep(c("Richness", "Biomass", "Diversity"), length.out = nrow(coeffs_coralSA))
coeffs_coralSA$predictor <- predictor_sequence

p_coralSA <- ggplot(coeffs_coralSA, aes(Estimate, predictor, fill = group, color = group)) +
  geom_vline(aes(xintercept = 0), size = .25, linetype = "dashed") +         
  geom_errorbarh(aes(xmax = Q95, xmin =Q5), linewidth = 1.3, height = 0, color = "light grey", position = position_dodge(width = 0.5)) +
  geom_errorbarh(aes(xmax = Q80, xmin =Q20), linewidth = 2, height = 0, color = "Dark grey", position = position_dodge(width = 0.5)) +
  geom_point(size = 3, position = position_dodge(width=0.5)) +
  scale_color_fish_d(option = "Gramma_loreto") +
  labs(color = "Group", fill = "Group", x = "Model coefficient", y = "", title = "Coral Surface Area") +
  theme_classic() +
  my_theme

p_coralSA

# export plot

png(file=file.path(output_directory, "Fish_predict_benthic.png"), height = 4000, width = 5000, res = 400)
(p_coralCov + p_coralSA)/(p_algae + p_recruits) + plot_layout(guides="collect") + plot_annotation(tag_levels = "A")  &
  theme(plot.tag = element_text(size = 30, face = "bold"))
dev.off()

###########################################################################################
############################## plot random effects ########################################

mcmc_plot(m_algae_all, var = c("sd_Site_Name__Intercept", "sd_Year__Intercept"), type = "hist")
mcmc_plot(m_algae_all)
get_variables(m_algae_all)
summary(m_algae_all)
ranef(m_algae_all)

# Extract posterior samples of sd_Site_Name__Intercept
random_algae_all <- m_algae_all %>%
  spread_draws(sd_Site_Name__Intercept, sd_Year__Intercept) %>%
  select(sd_Site_Name__Intercept, sd_Year__Intercept) %>%
  pivot_longer(cols = everything(), 
               names_to = "Variable", 
               values_to = "Value") %>%
  mutate(group = "All fish") 

random_algae_herb <- m_algae_herb %>%
  spread_draws(sd_Site_Name__Intercept, sd_Year__Intercept) %>%
  select(sd_Site_Name__Intercept, sd_Year__Intercept) %>%
  pivot_longer(cols = everything(), 
               names_to = "Variable", 
               values_to = "Value") %>%
  mutate(group = "Herbivores") 

random_algae_corallivore <- m_algae_corallivore %>%
  spread_draws(sd_Site_Name__Intercept, sd_Year__Intercept) %>%
  select(sd_Site_Name__Intercept, sd_Year__Intercept) %>%
  pivot_longer(cols = everything(), 
               names_to = "Variable", 
               values_to = "Value") %>%
  mutate(group = "Corallivores")

random_algae_parrot <- m_algae_parrot %>%
  spread_draws(sd_Site_Name__Intercept, sd_Year__Intercept) %>%
  select(sd_Site_Name__Intercept, sd_Year__Intercept) %>%
  pivot_longer(cols = everything(), 
               names_to = "Variable", 
               values_to = "Value") %>%
  mutate(group = "Parrotfish") 

random_plot_algae <- rbind(random_algae_all, random_algae_corallivore, random_algae_herb, random_algae_parrot)
random_plot_algae$Variable <- gsub("sd_Site_Name__Intercept", "Site", random_plot_algae$Variable)
random_plot_algae$Variable <- gsub("sd_Year__Intercept", "Year", random_plot_algae$Variable)
table(random_plot_algae$group)
table(random_plot_algae$Variable)

library(ggridges)

p_sigma_algae <- ggplot(random_plot_algae, aes(x = Value, y = group, fill = Variable)) +
  geom_density_ridges(alpha = 0.6, scale = 1) +
  geom_vline(aes(xintercept = 0), size = .25, linetype = "dashed") +
  labs(fill = "Variable", x = expression(paste("Intercept Estimate (", sigma, ")")), y = "Group", title = "Algae Cover") + 
  scale_fill_fish_d(option = "Gramma_loreto") +
  theme_classic() +
  xlim(c(-0.01, 1.5)) +
  my_theme

p_sigma_algae
 
# coral cover



summary(m_coralCov_all)
# Extract posterior samples of sd_Site_Name__Intercept
random_coralCov_all <- m_coralCov_all %>%
  spread_draws(sd_Site_Name__Intercept, sd_Year__Intercept) %>%
  select(sd_Site_Name__Intercept, sd_Year__Intercept) %>%
  pivot_longer(cols = everything(), 
               names_to = "Variable", 
               values_to = "Value") %>%
  mutate(group = "All fish") 

random_coralCov_herb <- m_coralCov_herb %>%
  spread_draws(sd_Site_Name__Intercept, sd_Year__Intercept) %>%
  select(sd_Site_Name__Intercept, sd_Year__Intercept) %>%
  pivot_longer(cols = everything(), 
               names_to = "Variable", 
               values_to = "Value") %>%
  mutate(group = "Herbivores") 

random_coralCov_corallivore <- m_coralCov_corallivore %>%
  spread_draws(sd_Site_Name__Intercept, sd_Year__Intercept) %>%
  select(sd_Site_Name__Intercept, sd_Year__Intercept) %>%
  pivot_longer(cols = everything(), 
               names_to = "Variable", 
               values_to = "Value") %>%
  mutate(group = "Corallivores")

random_coralCov_parrot <- m_coralCov_parrot %>%
  spread_draws(sd_Site_Name__Intercept, sd_Year__Intercept) %>%
  select(sd_Site_Name__Intercept, sd_Year__Intercept) %>%
  pivot_longer(cols = everything(), 
               names_to = "Variable", 
               values_to = "Value") %>%
  mutate(group = "Parrotfish") 

random_plot_coralCov <- rbind(random_coralCov_all, random_coralCov_corallivore, random_coralCov_herb, random_coralCov_parrot)
random_plot_coralCov$Variable <- gsub("sd_Site_Name__Intercept", "Site", random_plot_coralCov$Variable)
random_plot_coralCov$Variable <- gsub("sd_Year__Intercept", "Year", random_plot_coralCov$Variable)
table(random_plot_coralCov$group)
table(random_plot_coralCov$Variable)


p_sigma_coralCov <- ggplot(random_plot_coralCov, aes(x = Value, y = group, fill = Variable)) +
  geom_density_ridges(alpha = 0.6, scale = 1) +
  geom_vline(aes(xintercept = 0), size = .25, linetype = "dashed") +
  labs(fill = "Variable", x = expression(paste("Intercept Estimate (", sigma, ")")), y = "Group", title = "Coral Cover") + 
  scale_fill_fish_d(option = "Gramma_loreto") +
  theme_classic() +
  xlim(c(-0.01, 1.5)) +
  my_theme
p_sigma_coralCov
# recruits

# Extract posterior samples of sd_Site_Name__Intercept
random_recruits_all <- m_recruits_all %>%
  spread_draws(sd_Site_Name__Intercept, sd_Year__Intercept) %>%
  select(sd_Site_Name__Intercept, sd_Year__Intercept) %>%
  pivot_longer(cols = everything(), 
               names_to = "Variable", 
               values_to = "Value") %>%
  mutate(group = "All fish") 

random_recruits_herb <- m_recruits_herb %>%
  spread_draws(sd_Site_Name__Intercept, sd_Year__Intercept) %>%
  select(sd_Site_Name__Intercept, sd_Year__Intercept) %>%
  pivot_longer(cols = everything(), 
               names_to = "Variable", 
               values_to = "Value") %>%
  mutate(group = "Herbivores") 

random_recruits_corallivore <- m_recruits_corallivore %>%
  spread_draws(sd_Site_Name__Intercept, sd_Year__Intercept) %>%
  select(sd_Site_Name__Intercept, sd_Year__Intercept) %>%
  pivot_longer(cols = everything(), 
               names_to = "Variable", 
               values_to = "Value") %>%
  mutate(group = "Corallivores")

random_recruits_parrot <- m_recruits_parrot %>%
  spread_draws(sd_Site_Name__Intercept, sd_Year__Intercept) %>%
  select(sd_Site_Name__Intercept, sd_Year__Intercept) %>%
  pivot_longer(cols = everything(), 
               names_to = "Variable", 
               values_to = "Value") %>%
  mutate(group = "Parrotfish") 

random_plot_recruits <- rbind(random_recruits_all, random_recruits_corallivore, random_recruits_herb, random_recruits_parrot)
random_plot_recruits$Variable <- gsub("sd_Site_Name__Intercept", "Site", random_plot_recruits$Variable)
random_plot_recruits$Variable <- gsub("sd_Year__Intercept", "Year", random_plot_recruits$Variable)
table(random_plot_recruits$group)
table(random_plot_recruits$Variable)

p_sigma_recruits <- ggplot(random_plot_recruits, aes(x = Value, y = group, fill = Variable)) +
  geom_density_ridges(alpha = 0.6, scale = 1) +
  geom_vline(aes(xintercept = 0), size = .25, linetype = "dashed") +
  labs(fill = "Variable", x = expression(paste("Intercept Estimate (", sigma, ")")), y = "Group", title = "Total Recruits") + 
  scale_fill_fish_d(option = "Gramma_loreto") +
  theme_classic() +
  xlim(c(-0.01, 1.5)) +
  my_theme
p_sigma_recruits

# coral surface area


# Extract posterior samples of sd_Site_Name__Intercept
random_coralSA_all <- m_coralSA_all %>%
  spread_draws(sd_Site_Name__Intercept, sd_Year__Intercept) %>%
  select(sd_Site_Name__Intercept, sd_Year__Intercept) %>%
  pivot_longer(cols = everything(), 
               names_to = "Variable", 
               values_to = "Value") %>%
  mutate(group = "All fish") 

random_coralSA_herb <- m_coralSA_herb %>%
  spread_draws(sd_Site_Name__Intercept, sd_Year__Intercept) %>%
  select(sd_Site_Name__Intercept, sd_Year__Intercept) %>%
  pivot_longer(cols = everything(), 
               names_to = "Variable", 
               values_to = "Value") %>%
  mutate(group = "Herbivores") 

random_coralSA_corallivore <- m_coralSA_corallivore %>%
  spread_draws(sd_Site_Name__Intercept, sd_Year__Intercept) %>%
  select(sd_Site_Name__Intercept, sd_Year__Intercept) %>%
  pivot_longer(cols = everything(), 
               names_to = "Variable", 
               values_to = "Value") %>%
  mutate(group = "Corallivores")

random_coralSA_parrot <- m_coralSA_parrot %>%
  spread_draws(sd_Site_Name__Intercept, sd_Year__Intercept) %>%
  select(sd_Site_Name__Intercept, sd_Year__Intercept) %>%
  pivot_longer(cols = everything(), 
               names_to = "Variable", 
               values_to = "Value") %>%
  mutate(group = "Parrotfish") 

random_plot_coralSA <- rbind(random_coralSA_all, random_coralSA_corallivore, random_coralSA_herb, random_coralSA_parrot)
random_plot_coralSA$Variable <- gsub("sd_Site_Name__Intercept", "Site", random_plot_coralSA$Variable)
random_plot_coralSA$Variable <- gsub("sd_Year__Intercept", "Year", random_plot_coralSA$Variable)
table(random_plot_coralSA$group)
table(random_plot_coralSA$Variable)


p_sigma_coralSA <- ggplot(random_plot_coralSA, aes(x = Value, y = group, fill = Variable)) +
  geom_density_ridges(alpha = 0.6, scale = 1) +
  geom_vline(aes(xintercept = 0), size = .25, linetype = "dashed") +
  labs(fill = "Variable", x = expression(paste("Intercept Estimate (", sigma, ")")), y = "Group", title = "Coral Surface Area") + 
  scale_fill_fish_d(option = "Gramma_loreto") +
  theme_classic() +
  xlim(c(-0.01, 1.5)) +
  my_theme
p_sigma_coralSA

png(file=file.path(output_directory, "Sigma_random_effects.png"), height = 4000, width = 5000, res = 400)
(p_sigma_coralCov + p_sigma_coralSA)/(p_sigma_algae + p_sigma_recruits) + plot_layout(guides="collect") + plot_annotation(tag_levels = "A")  &
  theme(plot.tag = element_text(size = 30, face = "bold"))
dev.off()



############################################################################################
################################ PPCs and traceplots #######################################

# benthic predicting fish community

a <- mcmc_plot(m1, type = "trace")

b <- pp_check(m1, resp='roundmeanbiomass')
c <- pp_check(m1, resp='roundshannondiv100')
d <- pp_check(m1, resp='richness')
summary(m1)

Supplementary <- "Supplementary"

png(file=file.path(output_directory, Supplementary, "brms_out_coral_predict_fish.png"), height = 3000, width = 5500, res = 350)
(a)/(b+c+d) + plot_layout(heights=c(3,1)) + plot_annotation(
  title = 'Coral predicting fish model', tag_levels = "A") & theme(plot.title = element_text(size = 20, face = "bold"), plot.tag = element_text(size = 30, face = "bold"))
dev.off()

# all fish predicting coral 

a <- mcmc_plot(m_coralCov_all, type = "trace")
b <- pp_check(m_coralCov_all)

png(file=file.path(output_directory, Supplementary, "brms_out_Allfish_coralcov.png"), height = 2000, width = 4500, res = 350)
a+b +  plot_annotation(
  title = 'All fish predicting coral cover', tag_levels = "A") & theme(plot.title = element_text(size = 20, face = "bold"), plot.tag = element_text(size = 30, face = "bold"))
dev.off()

# all fish macroalgae

a <- mcmc_plot(m_algae_all, type = "trace")
b <- pp_check(m_algae_all)

png(file=file.path(output_directory, Supplementary, "brms_out_Allfish_algae.png"), height = 2000, width = 4500, res = 350)
a+b +  plot_annotation(
  title = 'All fish predicting algae cover', tag_levels = "A") & theme(plot.title = element_text(size = 20, face = "bold"), plot.tag = element_text(size = 30, face = "bold"))
dev.off()

# all fish recruits

a <- mcmc_plot(m_recruits_all, type = "trace")
b <- pp_check(m_recruits_all)

png(file=file.path(output_directory, Supplementary, "brms_out_Allfish_recruits.png"), height = 2000, width = 4500, res = 350)
a+b +  plot_annotation(
  title = 'All fish predicting recruits', tag_levels = "A") & theme(plot.title = element_text(size = 20, face = "bold"), plot.tag = element_text(size = 30, face = "bold"))
dev.off()

# all fish coral SA

a <- mcmc_plot(m_coralSA_all, type = "trace")
b <- pp_check(m_coralSA_all)

png(file=file.path(output_directory, Supplementary, "brms_out_Allfish_coralSA.png"), height = 2000, width = 4500, res = 350)
a+b +  plot_annotation(
  title = 'All fish predicting coral SA', tag_levels = "A") & theme(plot.title = element_text(size = 20, face = "bold"), plot.tag = element_text(size = 30, face = "bold"))
dev.off()

# tidy work space
rm(m1, m_coralCov_all, m_algae_all, m_recruits_all, m_coralSA_all)

# herbivores 


# herb fish predicting coral 

a <- mcmc_plot(m_coralCov_herb, type = "trace")
b <- pp_check(m_coralCov_herb)

png(file=file.path(output_directory, Supplementary, "brms_out_herbfish_coralcov.png"), height = 2000, width = 4500, res = 350)
a+b +  plot_annotation(
  title = 'Herbivores predicting coral cover', tag_levels = "A") & theme(plot.title = element_text(size = 20, face = "bold"), plot.tag = element_text(size = 30, face = "bold"))
dev.off()

# herb fish macroalgae

a <- mcmc_plot(m_algae_herb, type = "trace")
b <- pp_check(m_algae_herb)

png(file=file.path(output_directory, Supplementary, "brms_out_herbfish_algae.png"), height = 2000, width = 4500, res = 350)
a+b +  plot_annotation(
  title = 'Herbivores predicting algae cover', tag_levels = "A") & theme(plot.title = element_text(size = 20, face = "bold"), plot.tag = element_text(size = 30, face = "bold"))
dev.off()

# herb fish recruits

a <- mcmc_plot(m_recruits_herb, type = "trace")
b <- pp_check(m_recruits_herb)

png(file=file.path(output_directory, Supplementary, "brms_out_herbfish_recruits.png"), height = 2000, width = 4500, res = 350)
a+b +  plot_annotation(
  title = 'Herbivores predicting recruits', tag_levels = "A") & theme(plot.title = element_text(size = 20, face = "bold"), plot.tag = element_text(size = 30, face = "bold"))
dev.off()

# herb fish coral SA

a <- mcmc_plot(m_coralSA_herb, type = "trace")
b <- pp_check(m_coralSA_herb)

png(file=file.path(output_directory, Supplementary, "brms_out_herbfish_coralSA.png"), height = 2000, width = 4500, res = 350)
a+b +  plot_annotation(
  title = 'Herbivores predicting coral SA', tag_levels = "A") & theme(plot.title = element_text(size = 20, face = "bold"), plot.tag = element_text(size = 30, face = "bold"))
dev.off()

rm(m_coralCov_herb, m_algae_herb, m_recruits_herb, m_coralSA_herb)

# parrotfish


# parrot fish predicting coral 

a <- mcmc_plot(m_coralCov_parrot, type = "trace")
b <- pp_check(m_coralCov_parrot)

png(file=file.path(output_directory, Supplementary, "brms_out_parrotfish_coralcov.png"), height = 2000, width = 4500, res = 350)
a+b +  plot_annotation(
  title = 'Parrotfish predicting coral cover', tag_levels = "A") & theme(plot.title = element_text(size = 20, face = "bold"), plot.tag = element_text(size = 30, face = "bold"))
dev.off()

# parrot fish macroalgae

a <- mcmc_plot(m_algae_parrot, type = "trace")
b <- pp_check(m_algae_parrot)

png(file=file.path(output_directory, Supplementary, "brms_out_parrotfish_algae.png"), height = 2000, width = 4500, res = 350)
a+b +  plot_annotation(
  title = 'Parrotfish predicting algae cover', tag_levels = "A") & theme(plot.title = element_text(size = 20, face = "bold"), plot.tag = element_text(size = 30, face = "bold"))
dev.off()

# parrot fish recruits

a <- mcmc_plot(m_recruits_parrot, type = "trace")
b <- pp_check(m_recruits_parrot)

png(file=file.path(output_directory, Supplementary, "brms_out_parrotfish_recruits.png"), height = 2000, width = 4500, res = 350)
a+b +  plot_annotation(
  title = 'Parrotfish predicting recruits', tag_levels = "A") & theme(plot.title = element_text(size = 20, face = "bold"), plot.tag = element_text(size = 30, face = "bold"))
dev.off()

# parrot fish coral SA

a <- mcmc_plot(m_coralSA_parrot, type = "trace")
b <- pp_check(m_coralSA_parrot)

png(file=file.path(output_directory, Supplementary, "brms_out_parrotfish_coralSA.png"), height = 2000, width = 4500, res = 350)
a+b +  plot_annotation(
  title = 'Parrotfish predicting coral SA', tag_levels = "A") & theme(plot.title = element_text(size = 20, face = "bold"), plot.tag = element_text(size = 30, face = "bold"))
dev.off()

rm(m_coralCov_parrot, m_algae_parrot, m_recruits_parrot, m_coralSA_parrot)

# corallivores


# corallivore fish predicting coral 

a <- mcmc_plot(m_coralCov_corallivore, type = "trace")
b <- pp_check(m_coralCov_corallivore)

png(file=file.path(output_directory, Supplementary, "brms_out_corallivorefish_coralcov.png"), height = 2000, width = 4500, res = 350)
a+b +  plot_annotation(
  title = 'Corallivorespredicting coral cover', tag_levels = "A") & theme(plot.title = element_text(size = 20, face = "bold"), plot.tag = element_text(size = 30, face = "bold"))
dev.off()

# corallivore fish macroalgae

a <- mcmc_plot(m_algae_corallivore, type = "trace")
b <- pp_check(m_algae_corallivore)

png(file=file.path(output_directory, Supplementary, "brms_out_corallivorefish_algae.png"), height = 2000, width = 4500, res = 350)
a+b +  plot_annotation(
  title = 'Corallivores predicting algae cover', tag_levels = "A") & theme(plot.title = element_text(size = 20, face = "bold"), plot.tag = element_text(size = 30, face = "bold"))
dev.off()

# corallivore fish recruits

a <- mcmc_plot(m_recruits_corallivore, type = "trace")
b <- pp_check(m_recruits_corallivore)

png(file=file.path(output_directory, Supplementary, "brms_out_corallivorefish_recruits.png"), height = 2000, width = 4500, res = 350)
a+b +  plot_annotation(
  title = 'Corallivores predicting recruits', tag_levels = "A") & theme(plot.title = element_text(size = 20, face = "bold"), plot.tag = element_text(size = 30, face = "bold"))
dev.off()

# corallivore fish coral SA

a <- mcmc_plot(m_coralSA_corallivore, type = "trace")
b <- pp_check(m_coralSA_corallivore)

png(file=file.path(output_directory, Supplementary, "brms_out_corallivorefish_coralSA.png"), height = 2000, width = 4500, res = 350)
a+b +  plot_annotation(
  title = 'Corallivores predicting coral SA', tag_levels = "A") & theme(plot.title = element_text(size = 20, face = "bold"), plot.tag = element_text(size = 30, face = "bold"))
dev.off()