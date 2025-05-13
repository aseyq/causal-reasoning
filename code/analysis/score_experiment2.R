library(readr)
library(ggplot2)
library(tidyverse)
library(here)
library(rethinking)
library(systemfonts)
library(showtext)
library(dplyr)

###############################################################################################
# Load file
df_landscape2 <- read_csv("data/fitness_landscape/df_landscape_experiment2.csv", col_names = TRUE)

df_landscape2_summary <- df_landscape2 %>%
  summarize(
    mean_Payoff = mean(Payoff),  # Mean of all Payoff values
    prop_Payoff_positive = mean(Payoff > 0),  # Proportion of Payoff > 0
    mean_Payoff_positive = mean(Payoff[Payoff > 0]),  # Mean Payoff for values > 0
    mean_Payoff_balanced = mean(Payoff[wT == wR & wR == wB & wB == wL]),  # Mean Payoff when wT = wR = wB = wL
    max_Payoff_balanced = max(Payoff[wT == wR & wR == wB & wB == wL]),
    n_config = nrow(df_landscape2),
    n_config_zero = length(Payoff[Payoff == 0])
  )
###############################################################################################

df_wheel_expe2 <- read_csv("data/experiment/df_wheel_experiment2.csv", col_names = TRUE)

df_wheel_expe2 <- df_wheel_expe2%>%
  mutate(Treatment = factor(Treatment, levels = c("ABSMAP", "ABS", "CON")),
         Payoff = Payoff*100)%>%
  select(ParticipantID, ChainID, Treatment, Generation, Payoff, Trial, TrialChain,
         Configuration, wT, wR, wB, wL, comX, comY, Inertia)

treatment_names <- c("ABS"="Abstract information / Causal structure", 
                     "CON"="Mechanical information / Causal structure", 
                     "ABSMAP" = "Abstract information / No Causal structure"
)

treatment_colors <- c("CON"= "#174A7E",
                      "ABS"= "#6A9FCC", 
                      "ABSMAP" = "#D9D9D9"
)

annotations_df <- data.frame(
  x = c(2.5, 7.5, 12.5, 17.5, 22.5),
  label = c("Generation 1", "Generation 2", "Generation 3", "Generation 4", "Generation 5")
)

## Figure 1

# Define the trials to highlight (transmitted configurations)
highlight_trials <- c(4, 5, 9, 10, 14, 15, 19, 20, 24, 25)

df_wheel_expe2 %>%
  group_by(TrialChain, Treatment) %>%
  summarize(
    Payoff_mean = mean(Payoff),
    Payoff_sd = sd(Payoff),
    Payoff_se = sd(Payoff) / sqrt(n())
  ) %>%
  mutate(Segment = cut(TrialChain, breaks = c(0.5, 5.5, 10.5, 15.5, 20.5, 25.5), labels = FALSE),
         LineWidth = ifelse(TrialChain %% 5 == 4, 1, 0.2),
         PointSize = ifelse(TrialChain %% 5 == 4 | TrialChain %% 5 == 0, 5, 2)) %>%
  
  ggplot(aes(y = Payoff_mean, x = TrialChain, color = Treatment, group = interaction(Treatment, Segment), linewidth = 0.3)) +
  
  geom_vline(xintercept = 5.5, linetype = "dashed") +
  geom_vline(xintercept = 10.5, linetype = "dashed") +
  geom_vline(xintercept = 15.5, linetype = "dashed") +
  geom_vline(xintercept = 20.5, linetype = "dashed") +
  
  geom_hline(yintercept = df_landscape2_summary$mean_Payoff, linetype = "dotted") +
  
  geom_point(aes(size = PointSize)) +
  geom_linerange(aes(ymin = Payoff_mean - Payoff_se, ymax = Payoff_mean + Payoff_se, linewidth = 0.1)) +
  
  geom_line(aes(linewidth = LineWidth)) +  
  
  scale_x_continuous(limits = c(1, 25.5), breaks = 1:25) +
  ylim(0, 17) +
  
  scale_color_manual(labels = treatment_names, values = treatment_colors) +
  
  annotate(geom = "text", x = annotations_df$x + 0.5, y = rep(17, nrow(annotations_df)), 
           label = annotations_df$label, vjust = -0.5, size = 5) +
  
  labs(x = "Trial", y = "Payoff (Pence)") +
  guides(color = guide_legend(title = NULL), linewidth = "none") +
  
  theme_classic() +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 12)) +
  theme(legend.position = "top") +
  scale_linewidth(range = c(0.2, 1))+
  scale_size_identity()

ggsave("figures/payoff_across_generations_experiment2.png", width = 11, height = 8, dpi = 300)

##################################################################
# Models 
##################################################################

df_models_expe2 <- df_wheel_expe2 %>%
  select(ParticipantID, ChainID, Treatment, Generation, Payoff, Trial)  

######################################
## Data ABSMAP
######################################

df_models_expe2_ABSMAP <- df_models_expe2%>%
  filter(Treatment == 'ABSMAP')%>%
  mutate(ParticipantID = as.integer(factor(ParticipantID)),  
         ChainID = as.integer(factor(ChainID)))

## 
model_ABSMAP <- ulam(
  alist(
    Payoff ~ dnorm(mu, sigma),
    mu <- a + a_Participant[ParticipantID]*sigma_Participant + a_Chain[ChainID]*sigma_Chain + b_Generation*Generation,
    
    # Intercept and fixed effects
    a ~ dnorm(4, 10),  
    b_Generation ~ dnorm(0, 10),  
    
    #  random effects
    a_Participant[ParticipantID] ~ dnorm(0, 1),
    a_Chain[ChainID] ~ dnorm(0, 1),
    
    sigma_Participant ~ dexp(1),  
    sigma_Chain ~ dexp(1),  
    sigma ~ dexp(1)  
  ),
  data = df_models_expe2_ABSMAP, 
  warmup = 2000,  
  iter = 4000,    
  chains = 4, 
  control = list(adapt_delta = 0.999, max_treedepth = 15),
  cores = parallel::detectCores() - 1
)

# Output
precis(model_ABSMAP,depth=1, prob = 0.95)

######################################
## Data ABS
######################################

df_models_expe2_ABS <- df_models_expe2%>%
  filter(Treatment == 'ABS')%>%
  mutate(ParticipantID = as.integer(factor(ParticipantID)),  
         ChainID = as.integer(factor(ChainID)))

model_ABS <- ulam(
  alist(
    Payoff ~ dnorm(mu, sigma),
    mu <- a + a_Participant[ParticipantID]*sigma_Participant + a_Chain[ChainID]*sigma_Chain + b_Generation*Generation,
    
    # Intercept and fixed effects
    a ~ dnorm(4, 10),  
    b_Generation ~ dnorm(0, 10),  
    
    #  random effects
    a_Participant[ParticipantID] ~ dnorm(0, 1),
    a_Chain[ChainID] ~ dnorm(0, 1),
    
    sigma_Participant ~ dexp(1),  
    sigma_Chain ~ dexp(1),  
    sigma ~ dexp(1)  
  ),
  data = df_models_expe2_ABS, 
  warmup = 2000,  
  iter = 4000,    
  chains = 4, 
  control = list(adapt_delta = 0.999, max_treedepth = 15),
  cores = parallel::detectCores() - 1
)

# Output
precis(model_ABS,depth=1, prob = 0.95)

######################################
## Data CON
######################################

df_models_expe2_CON <- df_models_expe2%>%
  filter(Treatment == 'CON')%>%
  mutate(ParticipantID = as.integer(factor(ParticipantID)),  
         ChainID = as.integer(factor(ChainID)))

#Model
model_CON <- ulam(
  alist(
    Payoff ~ dnorm(mu, sigma),
    mu <- a + a_Participant[ParticipantID]*sigma_Participant + a_Chain[ChainID]*sigma_Chain + b_Generation*Generation,
    
    # Intercept and fixed effects
    a ~ dnorm(4, 10),  
    b_Generation ~ dnorm(0, 10),  
    
    #  random effects
    a_Participant[ParticipantID] ~ dnorm(0, 1),
    a_Chain[ChainID] ~ dnorm(0, 1),
    
    sigma_Participant ~ dexp(1),  
    sigma_Chain ~ dexp(1),  
    sigma ~ dexp(1)  
  ),
  data = df_models_expe2_CON, 
  warmup = 2000,  
  iter = 4000,    
  chains = 4, 
  control = list(adapt_delta = 0.999, max_treedepth = 15),
  cores = parallel::detectCores() - 1
)

# Output
precis(model_CON,depth=1, prob = 0.95)

######################################
## Data ABS vs ABSMAP
######################################

df_models_expe2_ABS_ABSMAP <- df_models_expe2%>%
  filter(Treatment != 'CON')%>%
  mutate(ParticipantID = as.integer(factor(ParticipantID)),  
         ChainID = as.integer(factor(ChainID)),
         Treatment = as.integer(factor(Treatment))-1)

#Model
model_ABS_ABSMAP <- ulam(
  alist(
    Payoff ~ dnorm(mu, sigma),
    mu <- a + a_Participant[ParticipantID] + a_Chain[ChainID] + b_Generation*Generation +
      b_Treatment*Treatment + b_Treatment_Generation*Treatment*Generation,
    
    # Intercept and fixed effects
    a ~ dnorm(4, 10),  
    b_Generation ~ dnorm(0, 10),  
    
    b_Treatment ~ dnorm(0, 10),  
    b_Treatment_Generation ~ dnorm(0, 10),  
    
    #  random effects
    a_Participant[ParticipantID] ~ dnorm(0, 1),
    a_Chain[ChainID] ~ dnorm(0, 1),
    
    sigma_Participant ~ dexp(1),  
    sigma_Chain ~ dexp(1),  
    sigma ~ dexp(1)  
  ),
  data = df_models_expe2_ABS_ABSMAP, 
  warmup = 2000,  
  iter = 4000,    
  chains = 4, 
  control = list(adapt_delta = 0.999, max_treedepth = 15),
  cores = parallel::detectCores() - 1
)

# Output
precis(model_ABS_ABSMAP,depth=1, prob = 0.95)

######################################
## Data ABS vs CON
######################################

df_models_expe2_ABS_CON <- df_models_expe2%>%
  filter(Treatment != 'ABSMAP')%>%
  mutate(ParticipantID = as.integer(factor(ParticipantID)),  
         ChainID = as.integer(factor(ChainID)),
         Treatment = as.integer(factor(Treatment))-1)

#Model
model_ABS_CON <- ulam(
  alist(
    Payoff ~ dnorm(mu, sigma),
    mu <- a + a_Participant[ParticipantID] + a_Chain[ChainID] + b_Generation*Generation +
      b_Treatment*Treatment + b_Treatment_Generation*Treatment*Generation,
    
    # Intercept and fixed effects
    a ~ dnorm(4, 10),  
    b_Generation ~ dnorm(0, 10),  
    
    b_Treatment ~ dnorm(0, 10),  
    b_Treatment_Generation ~ dnorm(0, 10),  
    
    #  random effects
    a_Participant[ParticipantID] ~ dnorm(0, 1),
    a_Chain[ChainID] ~ dnorm(0, 1),
    
    sigma_Participant ~ dexp(1),  
    sigma_Chain ~ dexp(1),  
    sigma ~ dexp(1)  
  ),
  data = df_models_expe2_ABS_CON, 
  warmup = 2000,  
  iter = 4000,    
  chains = 4, 
  control = list(adapt_delta = 0.999, max_treedepth = 15),
  cores = parallel::detectCores() - 1
)

# Output
precis(model_ABS_CON,depth=1, prob = 0.95)

################################################################################

precis(model_ABSMAP,depth=1, prob = 0.95)
precis(model_ABS,depth=1, prob = 0.95)
precis(model_CON,depth=1, prob = 0.95)

precis(model_ABS_ABSMAP,depth=1, prob = 0.95)
precis(model_ABS_CON,depth=1, prob = 0.95)
###############################################################################






