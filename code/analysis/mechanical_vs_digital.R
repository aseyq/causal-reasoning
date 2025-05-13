library(tibble)
library(ggplot2)
library(tidyverse)
library(lme4)
library(lmerTest)
library(texreg)
library(sjPlot)
library(rethinking)
library(kableExtra)
library(openxlsx)


df_wheel_digital <- read_csv("data/experiment/df_wheel_experiment1.csv", col_names = TRUE) |>
  filter(Treatment == "CON") %>%
  mutate(Treatment = "Digital Wheel") %>%
  select(Treatment, ChainID, ParticipantID, Generation, Trial, TrialChain, Speed, wT, wR, wB, wL)

glimpse(df_wheel_digital)

df_wheel_physical <- read.xlsx("data/physical_wheel/wheel_derex_etal_2019.xlsx") %>%
  as_tibble() %>%
  filter(Treatment == "Configurations") %>%
  mutate(Treatment = "Physical Wheel") %>%
  select(Treatment, ChainID = Chain, ParticipantID = Participant, Generation, Trial, TrialChain = OverallTrial, Speed, wT= Weight1, wR = Weight2, wB = Weight3 , wL = Weight4) %>%
  mutate(ChainID = as.character(ChainID), ParticipantID = as.character(ParticipantID))

glimpse(df_wheel_physical)

df <- bind_rows(df_wheel_digital, df_wheel_physical) %>%
  mutate(Configuration = paste(wT, wR, wB, wL, sep = "-")) 

treatment_colors <- c("Digital Wheel"="#174A7E", 
                      "Physical Wheel"= "#c00000"
)

annotations_df <- data.frame(
  x = c(2.5, 7.5, 12.5, 17.5, 22.5),
  label = c("Generation 1", "Generation 2", "Generation 3", "Generation 4", "Generation 5")
)


## Supp Figure 4
df %>%
  group_by(TrialChain, Generation, Treatment) %>%
  summarize(Speed_mean = mean(Speed), Speed_sd = sd(Speed), Speed_se = sd(Speed) / sqrt(n())) %>%
  mutate(Segment = cut(TrialChain, breaks = c(0.5, 5.5, 10.5, 15.5, 20.5, 25.5), labels = FALSE),
         LineWidth = ifelse(TrialChain %% 5 == 4, 1, 0.2)) %>%
  ggplot(aes(y = Speed_mean, x = TrialChain, color = Treatment)) +
  
  geom_vline(xintercept = 5.5, linetype = "dashed") +
  geom_vline(xintercept = 10.5, linetype = "dashed") +
  geom_vline(xintercept = 15.5, linetype = "dashed") +
  geom_vline(xintercept = 20.5, linetype = "dashed") +
  
  geom_point() +
  geom_linerange(aes(ymin = Speed_mean - Speed_se, ymax = Speed_mean + Speed_se), linewidth = 0.1) +
  geom_line() +  
  
  scale_x_continuous(limits = c(1, 25.5), breaks = 1:25) +
  ylim(70, 160) +
  
  scale_color_manual(values = treatment_colors) +
  
  annotate(geom = "text", x = annotations_df$x + 0.5, y = rep(160, nrow(annotations_df)), 
           label = annotations_df$label, vjust = -0.5, size = 5) +
  
  labs(x = "Trial", y = "Speed (m/h)") +
  guides(color = guide_legend(title = NULL), linewidth = "none") +
  
  theme_classic() +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 12)) +
  theme(legend.position = "top") +
  scale_linewidth(range = c(0.2, 1))+
  scale_size_identity()


## Supp Figure 4
df_speeds_physcal <- df %>%
  filter(Treatment == "Physical Wheel") %>%
  select(Configuration, Speed) %>%
  group_by(Configuration) %>%
  summarize(SpeedPhysical = mean(Speed))

df_speeds_physcal

df_speeds_digital <- df %>%
  filter(Treatment == "Digital Wheel") %>%
  select(Configuration, Speed) %>%
  group_by(Configuration) %>%
  summarize(SpeedDigital = mean(Speed))

df_speeds_digital

speeds_comparison <- df %>%
  group_by(Treatment, Configuration) %>%
  summarise(n = n()) %>%
  pivot_wider(names_from = Treatment, values_from = n) %>%
  # remove configurations that are not present in both treatments
  filter(!is.na(`Digital Wheel`), !is.na(`Physical Wheel`)) %>%
  # get speed from df_wheel_physical 
  left_join(df_speeds_physcal, by = "Configuration") %>%
  # get speed from df_wheel_digital
  left_join(df_speeds_digital, by = "Configuration") %>%
  mutate(difference = SpeedDigital - SpeedPhysical)


speeds_comparison %>%
  filter(SpeedPhysical != 0, SpeedDigital != 0) %>%
  ggplot(aes(x = SpeedPhysical, y = SpeedDigital)) +
  geom_point(size=0.5) +
  # 45 degree line
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color="darkgray") +
  scale_x_continuous(breaks = seq(70, 160, 10)) +
  scale_y_continuous(breaks = seq(70, 160, 10)) +
  geom_text(aes(label = Configuration), nudge_x = 0.5, nudge_y = 0.5, color="black", size=3, alpha=0.3)+
  labs(x = "Speed of the physical wheel (m/h)", y = "Speed of the digital wheel (m/h)")+
  theme_classic() +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14))
  
  ggsave("figures/speed_comparison.png", width = 9, height = 7, dpi = 300)
