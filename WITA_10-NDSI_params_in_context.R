###########################################
# ~WRANGLING IN THE ANTIPODES~            #
# 10: Exploration of NDSI parameters      #
# Tristan Louth-Robins, April 2022        #
########################################################################################
# Post can found here:                                                                 #
# https://wranglingintheantipodes.wordpress.com/2022/04/09/ndsi-parameters-in-context/ #
########################################################################################

# Required packages:
library(hms)
library(lubridate)
library(tidyverse)
library(rlang)
library(gghighlight)
library(ggplot2)
library(ggtext)
library(RColorBrewer)
library(reshape2)

# Check/set your working directory:
getwd()

# Import the dataset from your directory:
data <- read_csv("WITA10_data.csv")

# Re-coerce factor variables in the dataset:
data$period <- ordered(data$period, levels = c("pre-dawn", "dawn", "morning", "midday", "afternoon", "dusk", "night"))
data$season <- ordered(data$season, levels = c("Summer", "Autumn", "Winter", "Spring"))

#################################################
# FIGURE 1                                      #
# OBSERVE ALL 1kHz NDSI READINGS OVER PERIOD    #
#################################################

na.data <- data %>% na.omit() 

mean(na.data$ndsi_1000)

ggplot(data, aes(x=date, y=ndsi_1000)) +
  geom_point(size = 1) +
  ylim(-1, 1) +
  geom_hline(yintercept = 0, colour = "black", linetype = "dashed", alpha = 0.4) +
  geom_hline(yintercept = 0.608774, colour = "red", linetype = "dashed", alpha = 1) +
  labs(title = ,
       x = "Anthropopophony Range \nLower bound (Hz)",
       y = "NDSI index (-1 Anthropophony < 0 = neutral < 1 = biophony)",
       caption = "") +
  theme_classic() 

############################################
# FIGURE 3                                 #
# COMP. NDSI FOR ONE OBSERVATION           #
############################################

figure_date <- "14th December 2021" # <-- enter date here for figure
observation <- "7:00:00 to 7:00:55" # <-- enter observation for figure

summary <- data %>% 
  filter(date == "2021-12-14",
         hour == 7, mins == 00) %>% 
  select(contains('ndsi')) %>% 
  pivot_longer(cols = contains('ndsi'), names_to = 'ndsi', values_to = 'value')

summary <- summary %>% 
  mutate(soundscape = ifelse(value < -0.75, "Anthropophony: strong", 
                             ifelse(value > -0.74 & value < -0.3, "Anthropophony: moderate", 
                                    ifelse(value > -0.3 & value < 0.3, "Neutral", 
                                           ifelse(value > 0.3 & value < 0.75, "Biophony: moderate", 
                                                  ifelse(value > 0.75, "Biophony: strong", "x")))))) 

summary$soundscape <- ordered(summary$soundscape, levels = c("Biophony: strong","Biophony: moderate","Neutral","Anthropophony: moderate","Anthropophony: strong"))
summary$ndsi <- ordered(summary$ndsi, levels = c("ndsi_1000", "ndsi_500", "ndsi_250", "ndsi_200Hz_to_1.5kHz_1.5kHz_to_9kHz","ndsi_200", "ndsi_100", "ndsi_50"))

summary <- summary %>% filter(ndsi != "ndsi_200Hz_to_1.5kHz_1.5kHz_to_9kHz")

# Simplified axis x tick labels
ndsi_types <- c("1kHz", "500Hz", "250Hz", "200Hz", "100Hz", "50Hz")

fig_title <- paste("NDSI Values", "\n", 
                   observation, "(", figure_date, ")")

compare_ndsi <- 
  ggplot(summary, aes(x=ndsi, y= value, colour=soundscape)) +
  geom_point(size = 3) +
  geom_label(
    label= round(summary$value, 2), 
    color = "black",
    nudge_x = 0.3, nudge_y = 0, 
    check_overlap = T
  ) +
  ylim(-1, 1) +
  geom_hline(yintercept = 0, colour = "black", linetype = "dashed", alpha = 0.4) +
  labs(title = fig_title,
       x = "Anthropopophony Range \nLower bound (Hz)",
       y = "NDSI index (-1 Anthropophony < 0 = neutral < 1 = biophony)",
       caption = "") +
  scale_x_discrete(labels = ndsi_types) + 
  theme_dark() +
  theme(legend.title = element_text(colour = "black", size = 12),
        legend.text = element_text(),
        legend.background = element_rect(colour = "steelblue"),
        legend.position = "bottom") 

compare_ndsi + scale_colour_manual(values=c("Biophony: strong" = "#33cc33", 
                                            "Biophony: moderate" = "#ccff66", 
                                            "Neutral" = "#ffff00", 
                                            "Anthropophony: moderate" = "#ffcc00", 
                                            "Anthropophony: strong" = "#ff6600"))



######################################################
# FIGURE 4                                           #
# OBSERVE 1kHz and 50Hz NDSI READINGS OVER PERIOD    #
######################################################

na.data <- data %>% na.omit() 

ndsi50vs1000 <- na.data %>% 
  pivot_longer(cols = contains('ndsi'), names_to = 'ndsi', values_to = 'value') %>% 
  filter(ndsi %in% c('ndsi_50', 'ndsi_1000')) 

ndsi50vs1000$ndsi <- ordered(ndsi50vs1000$ndsi, levels = c("ndsi_50", "ndsi_1000"))
ndsi_labels <- as_labeller(c('ndsi_50' = "NDSI (50Hz to 2kHz)", 'ndsi_1000' = "NDSI (1kHz to 2kHz)"))

means <- ndsi50vs1000 %>% group_by(ndsi) %>% summarise(mean_ndsi = mean(value),
                                                       min_ndsi = min(value),
                                                       max_ndsi = max(value),
                                                       sd_ndsi = sd(value))

ggplot(ndsi50vs1000, aes(x=date, y=value)) +
  geom_point(size = 1) +
  ylim(-1, 1) +
  geom_hline(yintercept = 0, colour = "black", linetype = "dashed", alpha = 0.6) +
  geom_hline(data = means, aes(yintercept = mean_ndsi, colour = "red"), linetype = "solid", alpha = 0.9, size = 1) +
  labs(title = ,
       x = "Anthropopophony Range \nLower bound (Hz)",
       y = "NDSI index (-1 Anthropophony < 0 = neutral < 1 = biophony)",
       caption = "") +
  facet_wrap(~ndsi, labeller = ndsi_labels) +
  theme_classic() +
  theme(legend.position="none") 

############################################
# FIGURE 5:                                #
# OBSERVE ALL NDSI READINGS OVER PERIOD    #
############################################

na.data <- data %>% na.omit() %>% select(-ndsi_200Hz_to_1.5kHz_1.5kHz_to_9kHz)

ndsi_all <- na.data %>% 
  pivot_longer(cols = contains('ndsi'), names_to = 'ndsi', values_to = 'value') 

ndsi_all$ndsi <- ordered(ndsi_all$ndsi, levels = c("ndsi_50", "ndsi_100", "ndsi_200", "ndsi_250", "ndsi_500", "ndsi_1000"))
ndsi_labels <- as_labeller(c('ndsi_50' = "NDSI (50Hz to 2kHz)", 
                             'ndsi_100' = "NDSI (100Hz to 2kHz)",
                             'ndsi_200' = "NDSI (200Hz to 2kHz)",
                             'ndsi_250' = "NDSI (250Hz to 2kHz)",
                             'ndsi_500' = "NDSI (500Hz to 2kHz)",
                             'ndsi_1000' = "NDSI (1kHz to 2kHz)"))

means_all <- ndsi_all %>% group_by(ndsi) %>% summarise(mean_ndsi = mean(value),
                                                       median_ndsi = median(value),
                                                       min_ndsi = min(value),
                                                       max_ndsi = max(value),
                                                       sd_ndsi = sd(value))

# PLOT THE DATA:

ggplot(ndsi_all, aes(x=date, y=value)) +
  geom_point(size = 1) +
  ylim(-1, 1) +
  geom_hline(yintercept = 0, colour = "black", linetype = "dashed", alpha = 0.6) +
  geom_hline(data = means_all, aes(yintercept = mean_ndsi, colour = "red"), linetype = "solid", alpha = 0.9, size = 1) +
  labs(title = ,
       x = "Anthropopophony Range \nLower bound (Hz)",
       y = "NDSI index (-1 Anthropophony < 0 = neutral < 1 = biophony)",
       caption = "") +
  facet_wrap(~ndsi, labeller = ndsi_labels) +
  theme_classic() +
  theme(legend.position="none") 

############################################
# FIGURE 6:                                #
# PLOT THE DAILY AVERAGE NDSI:             #
############################################

daily_ndsi <- ndsi_all %>% group_by(date, ndsi) %>% summarise(mean_daily_ndsi = mean(value))

ggplot(daily_ndsi, aes(x=date, y=mean_daily_ndsi)) +
  geom_point(size = 1) +
  geom_line() +
  ylim(-1, 1) +
  geom_hline(yintercept = 0, colour = "black", linetype = "dashed", alpha = 0.6) +
  geom_hline(data = means_all, aes(yintercept = mean_ndsi, colour = "red"), linetype = "solid", alpha = 0.9, size = 1) +
  labs(title = ,
       x = "Anthropopophony Range \nLower bound (Hz)",
       y = "NDSI index (-1 Anthropophony < 0 = neutral < 1 = biophony)",
       caption = "") +
  facet_wrap(~ndsi, labeller = ndsi_labels) +
  theme_classic() +
  theme(legend.position="none") 

#########################
# FIGURE 7:             #
# BOXPLOT OF ALL DATA   #
#########################

ndsi_types <- c("50Hz", "100Hz", "200Hz", "250Hz", "500Hz", "1kHz")

ggplot(ndsi_all, aes(x=ndsi, y=value)) +
  geom_boxplot() +
  geom_point(data = means_all, aes(y = mean_ndsi, colour = "red"), alpha = 0.9, size = 3) +
  ylim(-1, 1) +
  geom_hline(yintercept = 0, colour = "black", linetype = "dashed", alpha = 0.6) +
  labs(title = ,
       x = "Anthropopophony Range \nLower bound (Hz)",
       y = "NDSI index (-1 Anthropophony < 0 = neutral < 1 = biophony)",
       caption = "") +
  scale_x_discrete(labels = ndsi_types) + 
  theme_classic() +
  theme(legend.position="none") 

###############################################
# FIGURE 9:                                   #
# EXAMINE 200Hz range and the tweaked range   #
# with scatter plot                           #
###############################################

na.data <- data %>% na.omit()

ndsi200vstweaked <- na.data %>% 
  pivot_longer(cols = contains('ndsi'), names_to = 'ndsi', values_to = 'value') %>% 
  filter(ndsi %in% c('ndsi_200', 'ndsi_200Hz_to_1.5kHz_1.5kHz_to_9kHz')) 

ndsi200vstweaked$ndsi <- ordered(ndsi200vstweaked$ndsi, levels = c("ndsi_200", "ndsi_200Hz_to_1.5kHz_1.5kHz_to_9kHz"))
ndsi_labels <- as_labeller(c('ndsi_200' = "NDSI (200Hz to 2kHz, 2kHz to 8kHz)", 'ndsi_200Hz_to_1.5kHz_1.5kHz_to_9kHz' = "NDSI (200Hz to 1.5kHz, 1.5kHz to 9kHz)"))

means <- ndsi200vstweaked %>% group_by(ndsi) %>% summarise(mean_ndsi = mean(value),
                                                           median_ndsi = median(value),
                                                           min_ndsi = min(value),
                                                           max_ndsi = max(value),
                                                           sd_ndsi = sd(value))


ggplot(ndsi200vstweaked, aes(x=date, y=value)) +
  geom_point(size = 1) +
  ylim(-1, 1) +
  geom_hline(yintercept = 0, colour = "black", linetype = "dashed", alpha = 0.6) +
  geom_hline(data = means, aes(yintercept = mean_ndsi, colour = "red"), linetype = "solid", alpha = 1, size = 1) +
  labs(title = ,
       x = "Anthropopophony Range \nLower bound (Hz)",
       y = "NDSI index (-1 Anthropophony < 0 = neutral < 1 = biophony)",
       caption = "") +
  facet_wrap(~ndsi, labeller = ndsi_labels) +
  theme_classic() +
  theme(legend.position="none") 

###############################################
# FIGURE 10:                                  #
# EXAMINE 200Hz range and the tweaked range   #
# with boxplot                                #
###############################################

na.data <- data %>% na.omit() 

ndsi_all <- na.data %>% 
  pivot_longer(cols = contains('ndsi'), names_to = 'ndsi', values_to = 'value') 

ndsi_all$ndsi <- ordered(ndsi_all$ndsi, levels = c("ndsi_50", "ndsi_100", "ndsi_200", "ndsi_200Hz_to_1.5kHz_1.5kHz_to_9kHz", "ndsi_250", "ndsi_500", "ndsi_1000"))
ndsi_labels <- as_labeller(c('ndsi_50' = "NDSI (50Hz to 2kHz)", 
                             'ndsi_100' = "NDSI (100Hz to 2kHz)",
                             'ndsi_200' = "NDSI (200Hz to 2kHz)",
                             'ndsi_200Hz_to_1.5kHz_1.5kHz_to_9kHz' = "NDSI (tweaked)",
                             'ndsi_250' = "NDSI (250Hz to 2kHz)",
                             'ndsi_500' = "NDSI (500Hz to 2kHz)",
                             'ndsi_1000' = "NDSI (1kHz to 2kHz)"))

means_all <- ndsi_all %>% group_by(ndsi) %>% summarise(mean_ndsi = mean(value),
                                                       median_ndsi = median(value),
                                                       min_ndsi = min(value),
                                                       max_ndsi = max(value),
                                                       sd_ndsi = sd(value))

na.data.all <- na.data %>% 
  pivot_longer(cols = contains('ndsi'), names_to = 'ndsi', values_to = 'value')

na.data.all$ndsi <- ordered(na.data.all$ndsi, levels = c("ndsi_50", "ndsi_100", "ndsi_200", "ndsi_200Hz_to_1.5kHz_1.5kHz_to_9kHz", "ndsi_250", "ndsi_500", "ndsi_1000"))

ndsi_types <- c("A: 50Hz-2kHz\nB: 2kHz-8kHz", 
                "A: 100Hz-2kHz\nB: 2kHz-8kHz", 
                "A: 200Hz-2kHz\nB: 2kHz-8kHz", 
                "A: 200Hz-1.5kHz\nB: 1.5kHz-9kHz", 
                "A: 250Hz-2kHz\nB: 2kHz-8kHz", 
                "A: 500Hz-2kHz\nB: 2kHz-8kHz", 
                "A: 1kHz-2kHz\nB: 2kHz-8kHz")

ggplot(na.data.all, aes(x=ndsi, y=value)) +
  geom_boxplot() +
  geom_point(data = means_all, aes(y = mean_ndsi, colour = "red"), alpha = 0.9, size = 3) +
  ylim(-1, 1) +
  geom_hline(yintercept = 0, colour = "black", linetype = "dashed", alpha = 0.6) +
  labs(title = ,
       x = "\nNDSI Range: Anthropophony (A), Biophony (B)",
       y = "NDSI index (-1 Anthropophony < 0 = neutral < 1 = biophony)",
       caption = "") +
  scale_x_discrete(labels = ndsi_types) + 
  theme_classic() +
  theme(legend.position="none") 
