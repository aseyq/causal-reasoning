library(readr)
library(ggplot2)
library(tidyverse)
library(scales)


treatment_names <- c("ABS"="Abstract information / Causal structure", 
                     "CON"="Mechanical information / Causal structure", 
                     "ABSMAP" = "Abstract information / No Causal structure"
)

treatment_colors <- c("CON"= "#174A7E",
                      "ABS"= "#6A9FCC", 
                      "ABSMAP" = "#D9D9D9"
)


df_wheel_expe1 <- read_csv("data/experiment/df_wheel_experiment1.csv", col_names = TRUE)
df_wheel_expe2 <- read_csv("data/experiment/df_wheel_experiment2.csv", col_names = TRUE)


df_wheel_expe1 <- read_csv(file1, col_names = TRUE)
df_wheel_expe2 <- read_csv(file2, col_names = TRUE)

##########################################
######Supplementary Figure 2
##########################################

# Expe 1 / Tech Reason/ Gen 5
df_expe1_CON_gen5 <- df_wheel_expe1%>%
  filter(Generation == 5)%>%
  filter(Treatment == 'CON')

highlight_points_expe1_CON_gen5 <- tibble(
  wBm = df_expe1_CON_gen5$wB,  # Bottom weight values
  wTm = df_expe1_CON_gen5$wT,   # Top weight values
  wRm = df_expe1_CON_gen5$wR,   # Right weight values
  wLm = df_expe1_CON_gen5$wL   # Left weight values
)

# Expe 2 / Tech Reason/ Gen 5
df_expe2_CON_gen5 <- df_wheel_expe2%>%
  filter(Generation == 5)%>%
  filter(Treatment == 'CON')

highlight_points_expe2_CON_gen5 <- tibble(
  wBm = df_expe2_CON_gen5$wB,  # Bottom weight values
  wTm = df_expe2_CON_gen5$wT,   # Top weight values
  wRm = df_expe2_CON_gen5$wR,   # Right weight values
  wLm = df_expe2_CON_gen5$wL   # Left weight values
)

df_landscape_expe1 <- read_csv("data/fitness_landscape/df_landscape_experiment1.csv", col_names = TRUE)
df_landscape_expe2 <- read_csv("data/fitness_landscape/df_landscape_experiment2.csv", col_names = TRUE)

plot_payoff <- function(df) {
  df %>%
    # Reverse factor levels for 'wBm' and 'wLm'
    mutate(wBm = factor(wBm, levels = 1:12),
           wLm = factor(wLm, levels = rev(1:12))) %>%
    mutate(Top=wTm, Right=wRm, Bottom=wBm, Left=wLm) %>%
    ggplot(aes(x = Right, y = Top, fill = Payoff)) +
    geom_tile() +
    # Assuming scale_custom_gradient is predefined elsewhere
    scale_fill_gradientn("Payoff",
                         #                         colors = c("black", "darkblue", "#0000FF",  "#FFFF00", "red"),
                         colors = c("black", "darkblue", "royalblue",  "gold",  "firebrick"),
                         values = c(0, 0.01, seq(0.02, 1, length.out = 4))
    ) +
    
    theme_minimal() +
    facet_grid(Bottom ~ Left, labeller = label_both) +
    scale_x_continuous(breaks = 1:12) +
    scale_y_continuous(breaks = 1:12) +
    theme(
      strip.text = element_text(face = "bold", size = 12, color = "black"),  # Customize facet labels
      axis.text.x = element_text(size = 5),  # Customize axis text
      axis.text.y = element_text(size = 5),
      panel.grid.major = element_blank(),  # Remove grid lines
      panel.grid.minor = element_blank(),
      panel.spacing = unit(0.1, "lines")
    )
}

#### -------------------------

plot_landscape_expe1 <- df_landscape_expe1 %>%
  mutate(wTm = wT, wRm = wR, wBm = wB, wLm = wL) %>%
  plot_payoff() + geom_point(data = highlight_points_expe1_CON_gen5 %>%
                               mutate(Bottom = factor(wBm, levels = 1:12),
                                      Left = factor(wLm, levels = rev(1:12))),
                             aes(x = wRm, y = wTm),
                             color = "red", size = 1, shape = 21, fill = "white") 

plot_landscape_expe2 <- df_landscape_expe2 %>%
  mutate(wTm = wT, wRm = wR, wBm = wB, wLm = wL) %>%
  plot_payoff() + geom_point(data = highlight_points_expe2_CON_gen5 %>%
                               mutate(Bottom = factor(wBm, levels = 1:12),
                                      Left = factor(wLm, levels = rev(1:12))),
                             aes(x = wRm, y = wTm),
                             color = "red", size = 1, shape = 21, fill = "white") 

plot_landscape_expe1
ggsave("figures/landscape_experiment1.png", plot = plot_landscape_expe1, width = 12, height = 12, dpi = 300)
plot_landscape_expe2
ggsave("figures/landscape_experiment2.png", plot = plot_landscape_expe2, width = 12, height = 12, dpi = 300)


##########################################
######Supplementary Figure 3
##########################################


df_expe1_viable <- df_wheel_expe1%>%
  filter(Payoff > 0)%>%
  filter(Treatment != 'ABSMAP')

df_expe1_unviable <- df_landscape_expe1%>%
  filter(Payoff == 0)

df_expe2_viable <- df_wheel_expe2%>%
  filter(Payoff > 0)%>%
  filter(Treatment != 'ABSMAP')

df_expe2_unviable <- df_landscape_expe2%>%
  filter(Payoff == 0)

###################################################################################

## Expe 1

# Function to find the closest configuration 
find_closest_config_expe1 <- function(row) {
  # Compute Euclidean distance for all configurations 
  distances <- df_expe1_unviable %>%
    mutate(distance = abs(wT - row$wT) + abs(wR - row$wR) + abs(wB - row$wB) + abs(wL - row$wL)) %>%
    arrange(distance)  # Sort by closest distance
  
  # Return the closest non-zero payoff configuration along with distance
  return(distances[1, ])  # Selects the row with the smallest distance
}

# Apply the function to all rows 
closest_matches_expe1 <- df_expe1_viable %>%
  rowwise() %>%
  mutate(closest_config = list(find_closest_config_expe1(pick(everything())))) %>%  
  unnest(closest_config, names_sep = "_")%>% 
  select(ParticipantID, ChainID, Treatment, Generation, Configuration, TrialChain, Trial, wT,wR,wB,wL, Payoff, closest_config_Configuration, closest_config_Payoff, closest_config_distance)

###################################################################################
## Expe 2

# Function to find the closest configuration 
find_closest_config_expe2 <- function(row) {
  # Compute Euclidean distance for all configurations in df2_unviable
  distances <- df_expe2_unviable %>%
    mutate(distance = abs(wT - row$wT) + abs(wR - row$wR) + abs(wB - row$wB) + abs(wL - row$wL)) %>%
    arrange(distance)  # Sort by closest distance
  
  # Return the closest non-zero payoff configuration along with distance
  return(distances[1, ])  # Selects the row with the smallest distance
}

# Apply the function to all rows 
closest_matches_expe2 <- df_expe2_viable %>%
  rowwise() %>%
  mutate(closest_config = list(find_closest_config_expe2(pick(everything())))) %>%  
  unnest(closest_config, names_sep = "_")%>%  
  select(ParticipantID, ChainID, Treatment, Generation, Configuration, TrialChain, Trial, wT,wR,wB,wL, Payoff, closest_config_Configuration, closest_config_Payoff, closest_config_distance)


closest_matches_expe1$Experiment <- "Experiment 1"
closest_matches_expe2$Experiment <- "Experiment 2"

combined_closest_matches <- bind_rows(closest_matches_expe1, closest_matches_expe2)


# 
annotations_df <- data.frame(
  x = c(2.5, 7.5, 12.5, 17.5, 22.5),
  label = c("Generation 1", "Generation 2", "Generation 3", "Generation 4", "Generation 5")
)

combined_closest_matches %>%
  group_by(TrialChain, Treatment, Experiment) %>%
  summarize(Distance_mean = mean(closest_config_distance),
            Distance_se = sd(closest_config_distance) / sqrt(n()), .groups = "drop") %>%
  mutate(Segment = cut(TrialChain, breaks = c(0.5, 5.5, 10.5, 15.5, 20.5, 25.5), labels = FALSE))%>%
  ggplot(aes(x = TrialChain, y = Distance_mean, color = Treatment, group = interaction(Treatment, Segment, Experiment)))+
  geom_point() +
  geom_linerange(aes(ymin = Distance_mean - Distance_se, ymax = Distance_mean + Distance_se), linewidth = 0.2) +
  geom_line(aes(linetype = Experiment)) +
  geom_vline(xintercept = 5.5, linetype = "dashed") +
  geom_vline(xintercept = 10.5, linetype = "dashed") +
  geom_vline(xintercept = 15.5, linetype = "dashed") +
  geom_vline(xintercept = 20.5, linetype = "dashed") +
  scale_x_continuous(limits = c(0.5, 25.5), breaks = 1:25) +
  scale_color_manual(labels = treatment_names, values = treatment_colors) +
  scale_linetype_manual(values = c("Experiment 1" = "solid", "Experiment 2" = "dotted")) +
  annotate(geom = "text", x = annotations_df$x + 0.5, y = rep(3, nrow(annotations_df)), 
           label = annotations_df$label, vjust = -0.5, size = 5) +
  ylim(1, 3.1) +
  labs(x = "Trial", y = "Distance to closest non-viable configuration") +
  guides(color = guide_legend(title = NULL), linetype = guide_legend(title = NULL)) +
  theme_minimal() +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 12)) +
  theme(legend.position = "top")

ggsave("figures/distance_closest_nonviable.png", width = 12, height = 8, dpi = 300)

##
closest_matches_expe2%>%
  filter(closest_config_distance >4)
#
closest_matches_expe1%>%
  filter(closest_config_distance >4)

nrow(closest_matches_expe1)
nrow(closest_matches_expe2)
