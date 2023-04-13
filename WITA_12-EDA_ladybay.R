# ~WRANGLING IN THE ANTIPODES~            
# 12: Acoustic complexity as an indicator of tidal activity                   
# Tristan Louth-Robins, December 2022     
########################################################################################
# Post can found here:                                                                 #
# https://wranglingintheantipodes.wordpress.com/2022/12/19/acoustic-complexity-as-an-indicator-of-tidal-activity/
########################################################################################

# Required dependencies ----------------------------------------------------------------
library(tidyverse)
library(rlang)
library(gghighlight)
library(ggplot2)
library(ggrepel)
library(patchwork)
# https://patchwork.data-imaginist.com/articles/guides/layout.html

# Set WD and import dataset --------------------------------------------------------------------------------------------------------------------
setwd("/Users/tristanlouth-robins/Documents/Documents - MacBook Pro/R data and projects/acoustic_ecology_tests/Wrangling_In_The-Antipodes/data")
dataset <- "WITA_12-ACI_tidy_ladybay_22100809.csv"
data <- read_csv(dataset)

# Note: This above dataset has been derived from prior acoustic complexity analysis and tidying using this R script:
# https://github.com/TristanLouthRobins/Wrangling_In_The-Antipodes/blob/main/WITA_G-audiomoth_compute_indices_and_tidy_data

# Define general factors for time of day and season (season only to be used later on for longer term analysis) ---------------------------------
data$period <- ordered(data$period, levels = c("pre-dawn", "dawn", "morning", "midday", "afternoon", "dusk", "night"))
data$season <- ordered(data$season, levels = c("Summer", "Autumn", "Winter", "Spring"))
data$index <- as.factor(data$index)

# Removing the observation after retrieval at 10:10 9/10/22
n <- dim(data)[1]
data <- data[1:(n-1),]

# Optional: removing first two observations (heavy rain) to remove lm-smooth skew in plot.
data <- slice(data, -(1:2))

# Create variables from observations specific to unique events over period of overnight deployment --------------------------------------------- 
deploy <- data %>% filter(hour == 12 & mins == 00)
retrieve <- data %>% filter(hour == 10 & mins == 00)
hightide_081022 <- data %>% filter(hour == 17 & mins == 20)
hightide_091022 <- data %>% filter(hour == 5 & mins == 40)
lowtide_081022 <- data %>% filter(hour == 23 & mins == 30)
lowtide_091022 <- data %>% filter(hour == 10 & mins == 00)
raining1 <- data %>% filter(hour == 12 & mins == 00)
raining2 <- data %>% filter(hour == 12 & mins == 10)

# Visualise ACI over period of overnight deployment -----------------------------------------------
theme_wita <- function(){
  theme(
    text = element_text(family = "Avenir", colour = "black"),
    plot.margin = margin(5,2,5,2, "mm"),
    plot.background = element_rect(fill = "#ffffff"),
    panel.background = element_rect(fill = "#2e4a45"),
    panel.grid.major = element_line(colour = "#21403a"),
    panel.grid.minor = element_blank(),
    legend.title = element_text(family = "Courier", size = 9),
    legend.text = element_text(family = "Courier", size = 8),
    legend.background = element_rect(fill = "#ffffff")
  )
}

label_alpha <- 0.5
points <- "#eaf043"

d_1 <- data %>% 
  filter(date == "2022-10-08") %>% 
  ggplot(aes(x=time, y=value)) +
  geom_point(colour=points) +
  geom_smooth(colour="#ffffff") +
  geom_vline(data = deploy, aes(xintercept=time), colour = "green", alpha = 0.8, linetype="dashed") +
  geom_point(data = raining1, aes(x=time, y=value), colour = points, shape=8, size=4, alpha=0.5) +
  geom_point(data = raining2, aes(x=time, y=value), colour = points, shape=8, size=4, alpha=0.5) +
  geom_label(data = raining1, label="heavy rain", y=2600, alpha=label_alpha, nudge_x=3500) +
  geom_point(data = hightide_081022, aes(x=time, y=value), colour = "red", shape=24, size=4, stroke=2) +
  geom_label(data = hightide_081022, label="High: 1.40m", y=2450, alpha=label_alpha) +
  geom_point(data = lowtide_081022, aes(x=time, y=value), colour = "#03fcd3", shape=25, size=4, stroke=2, alpha=0.6) +
  geom_label(data = lowtide_081022, label="Low: 0.29m", y=1750, alpha=label_alpha, nudge_x=-3000) +
  theme_wita() +
  theme(axis.title.x=element_blank()) +
  ylim(1700, 2600)

d_2 <- data %>% 
  filter(date == "2022-10-09") %>% 
  ggplot(aes(x=time, y=value)) +
  geom_point(colour=points) +
  geom_smooth(colour="#ffffff") +
  geom_vline(data = retrieve, aes(xintercept=time), colour = "red", alpha=0.8, linetype="dashed") +
  geom_point(data = hightide_091022, aes(x=time, y=value), colour = "red", shape=24, size=4, stroke=2) +
  geom_label(data = hightide_091022, label = "High: 1.40m", y=2450, alpha=label_alpha) +
  geom_point(data = lowtide_091022, aes(x=time, y=value), colour = "#03fcd3", shape=25, size=4, stroke=2, alpha=0.6) +
  geom_label(data = lowtide_091022, label = "Low: 0.25m", y =1750, alpha=label_alpha, nudge_x=-3000) +
  theme_wita() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  ylim(1700, 2600)


viz <- d_1 +
  labs(title = "Lady Bay Reef: AudioMoth deployment\n8/10 (12:00-23:59) to 9/10 (00:00-10:00)",
       subtitle = "Acoustic Complexity as an indicator of tidal flows, peaks & lows",
       y = "ACI value") +
  d_2 +
  labs(caption = "Wrangling In The Antipodes: 'Acoustic complexity as an indicator of tidal activity'\nTristan Louth-Robins, 2022. Github: /TristanLouthRobins")

viz
