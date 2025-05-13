library(readr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(rethinking)

###############################################################################################
# Load file
df_wheel_expe1 <- read_csv("data/experiment/df_wheel_experiment1.csv", col_names = TRUE)

df_wheel_expe1 <- df_wheel_expe1%>%
  filter(Treatment != 'ABSMAP')%>%
  mutate(Treatment = factor(Treatment, levels = c("ABS", "CON")))%>%
  select(ParticipantID, ChainID, Treatment, Generation, Trial, TrialChain,
         wT, wR, wB, wL, comX, comY, Inertia)
         
treatment_names <- c("ABS"="Causal Reasoning", 
                     "CON"="Technical Reasoning"
)

# Exploration variables 
# calculations of distance from generation mean
# inertia: one dim, com: two dim, config: four dim
gen_means_explo <- df_wheel_expe1  %>% 
  group_by(Treatment, Generation)  %>% 
  summarise(gen_mean_inertia = mean(Inertia),
            gen_mean_comX = mean(comX),
            gen_mean_comY = mean(comY), 
            gen_mean_wT = mean(wT),
            gen_mean_wR = mean(wR),
            gen_mean_wB = mean(wB),
            gen_mean_wL = mean(wL), .groups = "drop")

df_expe1_explo <- df_wheel_expe1%>% 
  left_join(gen_means_explo, by = c("Treatment", "Generation")) %>%
  mutate(dist_gen_inertia_sq = (Inertia - gen_mean_inertia)^2,
         dist_gen_comX_sq = (comX - gen_mean_comX)^2,
         dist_gen_comY_sq = (comY - gen_mean_comY)^2,
         dist_gen_wT_sq = (wT - gen_mean_wT)^2,
         dist_gen_wR_sq = (wR - gen_mean_wR)^2,
         dist_gen_wB_sq = (wB - gen_mean_wB)^2,
         dist_gen_wL_sq = (wL - gen_mean_wL)^2) %>%
  mutate(dist_gen_inertia = sqrt(dist_gen_inertia_sq),
         dist_gen_com = sqrt(dist_gen_comX_sq + dist_gen_comY_sq),
         dist_gen_config = sqrt(dist_gen_wT_sq + dist_gen_wR_sq + dist_gen_wB_sq + dist_gen_wL_sq))  %>% 
  select(#-starts_with("gen_mean"),
    -ends_with("_sq"))

df_expe1_explo_models <- df_expe1_explo%>%
  filter(Generation == 1)%>%
  mutate(ParticipantID = as.integer(factor(ParticipantID)),  # Converts IDs to sequential numbers
         Treatment = as.integer(factor(Treatment))-1)
  

############################ 
####### Inertia
############################ 

df_expe1_explo%>%
  filter(Generation == 1)%>%
  ggplot(aes(x = Treatment, y = dist_gen_inertia, fill = Treatment)) +
  geom_violin(trim = FALSE, alpha = 0.7, color = NA) +
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
  labs(title = " ",
       y = "Exploration (Inertia/Compactness)", x = "Treatment") +
  
  scale_fill_manual(values = c(
    "CON" = "#174A7E" ,
    "ABS" = "#6A9FCC"
  ))+
  scale_x_discrete(labels = treatment_names) +
  theme_minimal() +
  theme(legend.position = "none")  

ggsave("figures/exploration_experiment1.png", width = 5, height = 4, dpi = 300)

#Model 
model_explo_inertia <- ulam(
  alist(
    dist_gen_inertia ~ dnorm(mu, sigma),
    mu <- a + a_Participant[ParticipantID] + b_Treatment*Treatment,
    
    # Intercept and fixed effects
    a ~ dnorm(5, 5),  
    b_Treatment ~ dnorm(0, 5), 
    
    # Random effects
    a_Participant[ParticipantID] ~ dnorm(0, sigma_Participant),
    
    sigma_Participant ~ dexp(1),  
    sigma ~ dexp(1)  
  ),
  data = df_expe1_explo_models, 
  warmup = 2000,  
  iter = 4000,    
  chains = 4, 
  control = list(adapt_delta = 0.999, max_treedepth = 15),
  cores = parallel::detectCores() - 1
)

# Output
precis(model_explo_inertia,depth=1, prob = 0.95)


############################ 
####### CoM
############################ 
df_expe1_explo%>%
  filter(Generation == 1)%>%
  ggplot(aes(x = Treatment, y = dist_gen_com, fill = Treatment)) +
  geom_violin(trim = FALSE, alpha = 0.7, color = NA) +
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
  labs(title = " ",
       y = "Exploration (CoM/Centroid)", x = "Treatment") +
  
  scale_fill_manual(values = c(
    "CON" = "#174A7E" ,
    "ABS" = "#6A9FCC"
  )) +
  scale_x_discrete(labels = treatment_names) +
  theme_minimal() +
  theme(legend.position = "none")  

#Model
model_explo_com <- ulam(
  alist(
    dist_gen_com ~ dnorm(mu, sigma),
    mu <- a + a_Participant[ParticipantID] + b_Treatment*Treatment,
    
    # Intercept and fixed effects
    a ~ dnorm(1, 5),  
    b_Treatment ~ dnorm(0, 5),  
    
    # random effects
    a_Participant[ParticipantID] ~ dnorm(0, sigma_Participant),
    
    sigma_Participant ~ dexp(1),  
    sigma ~ dexp(1)  
  ),
  data = df_expe1_explo_models, 
  warmup = 2000,  
  iter = 4000,    
  chains = 4, 
  control = list(adapt_delta = 0.999, max_treedepth = 15),
  cores = parallel::detectCores() - 1
)

# Output
precis(model_explo_com,depth=1, prob = 0.95)


############################ 
####### Configurations
############################ 
df_expe1_explo%>%
  filter(Generation == 1)%>%
  ggplot(aes(x = Treatment, y = dist_gen_config, fill = Treatment)) +
  geom_violin(trim = FALSE, alpha = 0.7, color = NA) +
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
  labs(title = " ",
       y = "Exploration (Configurations) ", x = "Treatment") +
  
  scale_fill_manual(values = c(
    "CON" = "#174A7E" ,
    "ABS" = "#6A9FCC"
  )) +
  scale_x_discrete(labels = treatment_names) +
  theme_minimal() +
  theme(legend.position = "none")  

#Model 
model_explo_config <- ulam(
  alist(
    dist_gen_config ~ dnorm(mu, sigma),
    mu <- a + a_Participant[ParticipantID] + b_Treatment*Treatment,
    
    # Intercept and fixed effects
    a ~ dnorm(5, 5),  
    b_Treatment ~ dnorm(0, 5),  
    
    # random effects
    a_Participant[ParticipantID] ~ dnorm(0, sigma_Participant),
    
    sigma_Participant ~ dexp(1),  
    sigma ~ dexp(1)  
  ),
  data = df_expe1_explo_models, 
  warmup = 2000,  
  iter = 4000,    
  chains = 4, 
  control = list(adapt_delta = 0.999, max_treedepth = 15),
  cores = parallel::detectCores() - 1
)

# Output
precis(model_explo_config,depth=1, prob = 0.95)



##############
precis(model_explo_inertia,depth=1, prob = 0.95)
precis(model_explo_com,depth=1, prob = 0.95)
precis(model_explo_config,depth=1, prob = 0.95)


