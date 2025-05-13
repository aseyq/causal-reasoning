library(readr)
library(ggplot2)
library(tidyverse)
library(patchwork)

df_wheel_expe1 <- read_csv("data/experiment/df_wheel_experiment1.csv", col_names = TRUE)
df_wheel_expe2 <- read_csv("data/experiment/df_wheel_experiment2.csv", col_names = TRUE)

##
df_wheel_expe1 <- df_wheel_expe1%>%
  mutate(Treatment = factor(Treatment, levels = c("CON", "ABS", "ABSMAP")),
         Payoff = Payoff*100, 
         Quadrant = case_when(
           comX == 0 & comY == 0  ~ "Center",
           comX >= 0 & comY >= 0   ~ "Top Right",
           comX >= 0 & comY < 0   ~ "Bottom Right",
           comX <= 0 & comY < 0   ~ "Bottom Left",
           comX <= 0 & comY >= 0    ~ "Top Left"
         ))%>%
  select(ParticipantID, ChainID, Treatment, Generation, Payoff, Trial, TrialChain,
         Configuration, wT, wR, wB, wL, comX, comY, Inertia, Quadrant)

##
df_wheel_expe2 <- df_wheel_expe2%>%
  mutate(Treatment = factor(Treatment, levels = c("CON", "ABS", "ABSMAP")),
         Payoff = Payoff*100, 
         Quadrant = case_when(
           comX == 0 & comY == 0  ~ "Center",
           comX >= 0 & comY >= 0   ~ "Top Right",
           comX >= 0 & comY < 0   ~ "Bottom Right",
           comX <= 0 & comY < 0   ~ "Bottom Left",
           comX <= 0 & comY >= 0    ~ "Top Left"
         ))%>%
  select(ParticipantID, ChainID, Treatment, Generation, Payoff, Trial, TrialChain,
         Configuration, wT, wR, wB, wL, comX, comY, Inertia, Quadrant)

##

# Define precise start and end angles for each quadrant (in degrees)
angle_mapping <- tibble(
  Quadrant = c("Top Right", "Bottom Right", "Bottom Left", "Top Left"),
  Start_Angle = c(0, 90, 180, 270),
  End_Angle = c(90, 180, 270, 360)  # Each quadrant covers 90°
)

treatment_names <- c("ABS"="Abstract information / Causal structure", 
                     "CON"="Mechanical information / Causal structure", 
                     "ABSMAP" = "Abstract information / No Causal structure"
)

####################################  
# Expe1 Trial1
####################################  
df_expe1_trial1 <- df_wheel_expe1%>%
  filter(Generation == 1)%>%
  filter(Trial ==1)
  
df_expe1_trial1_summary <- df_expe1_trial1%>%
  group_by(Quadrant, Treatment) %>%
  summarize(Count = n(), .groups = "drop") %>%
  left_join(angle_mapping, by = "Quadrant") %>%
  mutate(Radius = Count)  # Radius represents the count of wheels in each quadrant

# Create a separate data frame for the center circles
df_expe1_trial1_center_circle <- df_expe1_trial1_summary %>%
  filter(is.na(Start_Angle)) %>%
  select(Treatment) %>%
  distinct() %>%
  crossing(x = seq(0, 360, length.out = 100)) %>%
  left_join(df_expe1_trial1_summary %>% filter(is.na(Start_Angle)), by = "Treatment")


plot_expe1_trial1 <- ggplot() +
  # Draw four quadrants using geom_rect()
  geom_rect(data = df_expe1_trial1_summary %>% filter(!is.na(Start_Angle)), 
            aes(xmin = Start_Angle, xmax = End_Angle, ymin = 0, ymax = Radius, fill = Treatment)) +
  
  # Draw the center circle dynamically for each Treatment
  geom_path(data = df_expe1_trial1_center_circle, 
            aes(x = x, y = Count, group = Treatment),
            color = "black", linetype = "dashed", size = 0.5) +
  
  # Draw the center circle dynamically for each Treatment
  geom_vline(xintercept = c(0, 90, 180, 270), linewidth = 0.2)+
  
  # Convert to a circular layout
  coord_polar() +
  scale_x_continuous(limits = c(0, 360), breaks = c(0, 90, 180, 270), labels = NULL) +
  facet_wrap(~Treatment) +
  
  scale_fill_manual(labels = treatment_names, values = c(
    "CON" = "#174A7E" ,
    "ABS" = "#6A9FCC",
    "ABSMAP" = "#D9D9D9"
  )) +
  
  labs(title = "A) Experiment 1 / Trial 1",
       x = NULL, y = NULL) +
  ylim(0, 22) +
  theme_minimal()+
  theme(panel.grid.minor.x = element_blank(),
        axis.text.y = element_text(size = 8),
        strip.text = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(size = 10),
        plot.margin = margin(0, 0, 0, 0))


####################################  
# Expe1 Trial25
#################################### 
df_expe1_trial25 <- df_wheel_expe1%>%
  filter(Generation == 5)%>%
  filter(Trial ==5)

df_expe1_trial25_summary <- df_expe1_trial25%>%
  group_by(Quadrant, Treatment) %>%
  summarize(Count = n(), .groups = "drop") %>%
  left_join(angle_mapping, by = "Quadrant") %>%
  mutate(Radius = Count)  # Radius represents the count of wheels in each quadrant

# Create a separate data frame for the center circles
df_expe1_trial25_center_circle <- df_expe1_trial25_summary %>%
  filter(is.na(Start_Angle)) %>%
  select(Treatment) %>%
  crossing(x = seq(0, 360, length.out = 100)) %>%
  distinct() %>%
  left_join(df_expe1_trial25_summary %>% filter(is.na(Start_Angle)), by = "Treatment")

plot_expe1_trial25 <- ggplot() +
  # Draw four quadrants using geom_rect()
  geom_rect(data = df_expe1_trial25_summary %>% filter(!is.na(Start_Angle)), 
            aes(xmin = Start_Angle, xmax = End_Angle, ymin = 0, ymax = Radius, fill = Treatment)) +
  
  # Draw the center circle dynamically for each Treatment
  geom_path(data = df_expe1_trial25_center_circle, 
            aes(x = x, y = Count, group = Treatment),
            color = "black", linetype = "dashed", size = 0.5) +
  
  # Draw the center circle dynamically for each Treatment
  geom_vline(xintercept = c(0, 90, 180, 270), linewidth = 0.2)+
  
  # Convert to a circular layout
  coord_polar() +
  scale_x_continuous(limits = c(0, 360), breaks = c(0, 90, 180, 270), labels = NULL) +
  facet_wrap(~Treatment) +
  
  scale_fill_manual(labels = treatment_names, values = c(
    "CON" = "#174A7E" ,
    "ABS" = "#6A9FCC",
    "ABSMAP" = "#D9D9D9"
  )) +
  
  labs(title = "C) Experiment 1 / Trial 25",
       x = NULL, y = NULL) +
  ylim(0, 22) +
  theme_minimal()+
  theme(panel.grid.minor.x = element_blank(),
        axis.text.y = element_text(size = 8),
        legend.position = "none",
        strip.text = element_blank(),
        plot.title = element_text(size = 10),
        plot.margin = margin(0, 0, 0, 0))

####################################  
# Expe2 Trial1
#################################### 
df_expe2_trial1 <- df_wheel_expe2%>%
  filter(Generation == 1)%>%
  filter(Trial ==1)

df_expe2_trial1_summary <- df_expe2_trial1%>%
  group_by(Quadrant, Treatment) %>%
  summarize(Count = n(), .groups = "drop") %>%
  left_join(angle_mapping, by = "Quadrant") %>%
  mutate(Radius = Count)  # Radius represents the count of wheels in each quadrant

# Create a separate data frame for the center circles
df_expe2_trial1_center_circle <- df_expe2_trial1_summary %>%
  filter(is.na(Start_Angle)) %>%
  select(Treatment) %>%
  crossing(x = seq(0, 360, length.out = 100)) %>%
  distinct() %>%
  left_join(df_expe2_trial1_summary %>% filter(is.na(Start_Angle)), by = "Treatment")

plot_expe2_trial1 <- ggplot() +
  # Draw four quadrants using geom_rect()
  geom_rect(data = df_expe2_trial1_summary %>% filter(!is.na(Start_Angle)), 
            aes(xmin = Start_Angle, xmax = End_Angle, ymin = 0, ymax = Radius, fill = Treatment)) +
  
  # Draw the center circle dynamically for each Treatment
  geom_path(data = df_expe2_trial1_center_circle, 
            aes(x = x, y = Count, group = Treatment),
            color = "black", linetype = "dashed", size = 0.5) +
  
  # Draw the center circle dynamically for each Treatment
  geom_vline(xintercept = c(0, 90, 180, 270), linewidth = 0.2)+
  
  # Convert to a circular layout
  coord_polar() +
  scale_x_continuous(limits = c(0, 360), breaks = c(0, 90, 180, 270), labels = NULL) +
  facet_wrap(~Treatment) +
  
  scale_fill_manual(labels = treatment_names, values = c(
    "CON" = "#174A7E" ,
    "ABS" = "#6A9FCC",
    "ABSMAP" = "#D9D9D9"
  )) +
  
  labs(title = "B) Experiment 2 / Trial 1",
       x = NULL, y = NULL) +
  ylim(0, 22) +
  theme_minimal()+
  theme(panel.grid.minor.x = element_blank(),
        axis.text.y = element_text(size = 8),
        legend.position = "none",
        strip.text = element_blank(),
        plot.title = element_text(size = 10),
        plot.margin = margin(0, 0, 0, 0))


####################################  
# Expe2 Trial25
#################################### 
df_expe2_trial25 <- df_wheel_expe2%>%
  filter(Generation == 5)%>%
  filter(Trial ==5)

df_expe2_trial25_summary <- df_expe2_trial25%>%
  group_by(Quadrant, Treatment) %>%
  summarize(Count = n(), .groups = "drop") %>%
  left_join(angle_mapping, by = "Quadrant") %>%
  mutate(Radius = Count)  # Radius represents the count of wheels in each quadrant

# Create a separate data frame for the center circles
df_expe2_trial25_center_circle <- df_expe2_trial25_summary %>%
  filter(is.na(Start_Angle)) %>%
  select(Treatment) %>%
  crossing(x = seq(0, 360, length.out = 100)) %>%
  distinct() %>%
  left_join(df_expe2_trial25_summary %>% filter(is.na(Start_Angle)), by = "Treatment")

plot_expe2_trial25 <- ggplot() +
  # Draw four quadrants using geom_rect()
  geom_rect(data = df_expe2_trial25_summary %>% filter(!is.na(Start_Angle)), 
            aes(xmin = Start_Angle, xmax = End_Angle, ymin = 0, ymax = Radius, fill = Treatment)) +
  
  # Draw the center circle dynamically for each Treatment
  geom_path(data = df_expe2_trial25_center_circle, 
            aes(x = x, y = Count, group = Treatment),
            color = "black", linetype = "dashed", size = 0.5) +
  
  # Draw the center circle dynamically for each Treatment
  geom_vline(xintercept = c(0, 90, 180, 270), linewidth = 0.2)+
  
  # Convert to a circular layout
  coord_polar() +
  scale_x_continuous(limits = c(0, 360), breaks = c(0, 90, 180, 270), labels = NULL) +
  facet_wrap(~Treatment) +
  
  scale_fill_manual(labels = treatment_names, values = c(
    "CON" = "#174A7E" ,
    "ABS" = "#6A9FCC",
    "ABSMAP" = "#D9D9D9"
  )) +
  
  labs(title = "D) Experiment 2 / Trial 25",
       x = NULL, y = NULL) +
  ylim(0, 22) +
  theme_minimal()+
  theme(panel.grid.minor.x = element_blank(),
        axis.text.y = element_text(size = 8),
        legend.position = "none",
        strip.text = element_blank(),
        plot.title = element_text(size = 10),
        plot.margin = margin(0, 0, 0, 0))


#########################################################
# Figure 5
wrap_plots(plot_expe1_trial1, plot_expe2_trial1, plot_expe1_trial25, plot_expe2_trial25, ncol = 2, guides = "collect") +
  plot_annotation(title = " ") &
  guides(fill = guide_legend(title = NULL), color = guide_legend(title = NULL))&
  theme(
    legend.position = "bottom",
    panel.spacing = unit(0, "lines"),        # remove spacing between panels
  )

ggsave("figures/exploration_patterns.png", width = 8, height = 6, dpi = 300)
  
  