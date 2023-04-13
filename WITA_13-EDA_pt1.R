# ~ WRANGLING IN THE ANTIPODES ~            
# 13: Further explorations of ACI on Lady Bay Reef
# Part 1: Seagrass meadow area and comparison with area from WITA_12
# Tristan Louth-Robins, April 2023
# Blog post can found here: https:// #

# Required libraries ----------------------------------------------------------------
library(tidyverse)
library(rlang)
library(gghighlight)
library(ggplot2)
library(ggrepel)
library(patchwork)
library(plotly)
library(showtext)

# 1. First of all, let's generate the viz from the previous dataset and overlay some summary 
# data to establish baselines for comparison with the new dataset.

setwd("/Users/tristanlouth-robins/Documents/Documents - MacBook Pro/R data and projects/acoustic_ecology_tests/Wrangling_In_The-Antipodes/data")
dataset <- "WITA_12-ACI_tidy_ladybay_22100809.csv"
data <- read_csv(dataset)

# Note: This above dataset has been derived from prior acoustic complexity analysis and tidying using this R script:
# https://github.com/TristanLouthRobins/Wrangling_In_The-Antipodes/blob/main/WITA_G-audiomoth_compute_indices_and_tidy_data

# Define general factors for time of day and season (season only to be used later on for longer term analysis) ---------------------------------
data$period <- ordered(data$period, levels = c("pre-dawn", "dawn", "morning", "midday", "afternoon", "dusk", "night"))
data$index <- as.factor(data$index)

# Removing the observation after retrieval at 10:10 9/10/22
n <- dim(data)[1]
data <- data[1:(n-1),]

# Create variables from observations specific to unique events over period of overnight deployment --------------------------------------------- 
deploy <- data %>% filter(hour == 12 & mins == 00)
retrieve <- data %>% filter(hour == 10 & mins == 00)
hightide_081022 <- data %>% filter(hour == 17 & mins == 20)
hightide_091022 <- data %>% filter(hour == 5 & mins == 40)
lowtide_081022 <- data %>% filter(hour == 23 & mins == 30)
lowtide_091022 <- data %>% filter(hour == 10 & mins == 00)
raining <- data %>% filter(hour == 12 & mins %in% c(00, 10))

# define a function to generate summary stats (either overall or grouped by date) -- 

summary.stats <- function(data, condition) {
  if(condition == "all") {
    result <- 
      data %>% 
      summarise(min = min(value),
                max = max(value),
                mean = round(mean(value),0),
                med = median(value),
                std.dev = sd(value)) %>% 
      ungroup()
  } else {
    result <- 
      data %>% 
      group_by(date) %>% 
      summarise(min = min(value),
                max = max(value),
                mean = round(mean(value),0),
                med = median(value),
                std.dev = sd(value)) %>% 
      ungroup()
  }
  return(result)
}

# Store the summary stats as unique tibbles for these observations
(wita12_overall.stats <- summary.stats(data, "all"))
(wita12_daily.stats <- summary.stats(data, "daily"))

# visualisation ----------------------------------------------------------------

# aesthetics --
plot.bg <- "#f4f5f2"
panel.bg <- "#f6f7f5"
grid.minor <- "#e8ebe6"
grid.major <- "#e8ebe6"
leg.bg <- plot.bg
h.tide <- "#f511e6"
l.tide <- "#17e0ff"
points <- "#245757"
lab1.text <- "#320c85"
lab1.fill <- "#ffffff"
lab2.text <- "#000000"
lab2.fill <- "#afa9ba"
lab.alpha <- 0.5
titles.font <- "Helvetica"
cap.font <- "Avenir Next Condensed Regular"
lab.font <- "Avenir Next Condensed Regular"

# Scatter plot theme
theme_wita <- function(){
  theme(
    text = element_text(family = titles.font, colour = "black"),
    plot.margin = margin(5,2,5,2, "mm"),
    plot.background = element_rect(fill = plot.bg, colour = plot.bg),
    panel.background = element_rect(fill = panel.bg),
    panel.grid.major = element_line(colour = grid.major, size = 0.5),
    panel.grid.minor = element_line(colour = grid.major, size = 0.5),
    legend.title = element_text(family = titles.font, size = 9),
    legend.text = element_text(family = titles.font, size = 8),
    legend.background = element_rect(fill = leg.bg),
    legend.key = element_rect(fill = panel.bg),
    legend.position = "right",
  )
}

day1 <- "2022-10-08"
day2 <- "2022-10-09"

wita12.day1 <- 
data %>% 
  filter(date == day1) %>% 
  ggplot(aes(x=time, y=value)) +
  geom_point(colour = points) +
  geom_smooth(colour = "#ffd445", alpha = 0.5) +
  # deployment start
  geom_vline(data = deploy, aes(xintercept=time), colour = "#a9f70c", alpha = 0.8, linetype="dashed", size =1, alpha = 0.5) +
  # mean value for the period of observation
  geom_hline(data = wita12_overall.stats, aes(yintercept=mean), colour = "#0073ff", alpha = 0.5, linetype="solid", size = 1) +
  # events: raining 
  geom_point(data = raining, aes(x=time, y=value), colour = points, shape=8, size=4, alpha=0.5) +
  # events: low and high tide marks
  geom_point(data = lowtide_081022, aes(x=time, y=value), colour = l.tide, shape=25, size=4, stroke=2, alpha=0.6) +
  geom_point(data = hightide_081022, aes(x=time, y=value), colour = h.tide, shape=24, size=4, stroke=2) +
  # labels for events
  geom_label_repel(data = deploy, aes(family = lab.font, label="Deployment start", x=time, y=1700),colour = lab1.text,fill = lab1.fill,nudge_x = 8000,nudge_y = 0,segment.linetype = 1,segment.curvature = -0.1,segment.ncp = 3,segment.angle = 0,alpha = lab.alpha, arrow = arrow(length = unit(0.02, "npc"))) +
  geom_label_repel(data = wita12_overall.stats, aes(family = lab.font, label=paste("Mean ACI: ", mean), x=60000, y=mean),colour = lab1.text,fill = lab1.fill,nudge_x = 2000,nudge_y = -100,segment.linetype = 1,segment.curvature = -0.1,segment.ncp = 3,segment.angle = 20,alpha = lab.alpha, arrow = arrow(length = unit(0.02, "npc"))) +
  geom_label_repel(data = raining[2,], aes(family = lab.font, label="Heavy rain", x=time, y=value),colour = lab2.text,fill = lab2.fill,nudge_x = 8000,nudge_y = 50,segment.linetype = 1,segment.curvature = -0.1,segment.ncp = 3,segment.angle = 20,alpha = lab.alpha) +
  geom_label_repel(data = hightide_081022, aes(family = lab.font, label="High tide: 1.40m", x=time, y=value),colour = lab2.text,fill = lab2.fill,nudge_x = 8000,nudge_y = 50,segment.linetype = 1,segment.curvature = -0.1,segment.ncp = 3,segment.angle = 20,alpha = lab.alpha) +
  geom_label_repel(data = lowtide_081022, aes(family = lab.font, label="Low tide: 0.29m", x=time, y=value),colour = lab2.text,fill = lab2.fill,nudge_x = 8000,nudge_y = 200,segment.linetype = 1,segment.curvature = -0.1,segment.ncp = 3,segment.angle = 20,alpha = lab.alpha) +
  theme_wita() +
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank()) +
  ylim(1700, 2600)

wita12.day2 <- 
  data %>% 
  filter(date == day2) %>% 
  ggplot(aes(x=time, y=value)) +
  geom_point(colour = points) +
  geom_smooth(colour = "#ffd445", alpha = 0.5) +
  # deployment start
  geom_vline(data = retrieve, aes(xintercept=time), colour = "#f70c23", alpha = 0.8, linetype="dashed", size =1, alpha = 0.5) +
  # mean value for the period of observation
  geom_hline(data = wita12_overall.stats, aes(yintercept=mean), colour = "#0073ff", alpha = 0.5, linetype="solid", size = 1) +
  # events: low and high tide marks
  geom_point(data = lowtide_091022, aes(x=time, y=value), colour = l.tide, shape=25, size=4, stroke=2, alpha=0.6) +
  geom_point(data = hightide_091022, aes(x=time, y=value), colour = h.tide, shape=24, size=4, stroke=2) +
  # labels for events
  geom_label_repel(data = retrieve, aes(family = lab.font, label="Deployment end", x=time, y=1700),colour = lab1.text,fill = lab1.fill,nudge_x = -9000,nudge_y = 0,segment.linetype = 1,segment.curvature = -0.1,segment.ncp = 3,segment.angle = 0,alpha = lab.alpha, arrow = arrow(length = unit(0.02, "npc"))) +
  geom_label_repel(data = hightide_091022, aes(family = lab.font, label="High tide: 1.40m", x=time, y=value),colour = lab2.text,fill = lab2.fill,nudge_x = 5000,nudge_y = 200,segment.linetype = 1,segment.curvature = -0.1,segment.ncp = 3,segment.angle = 20,alpha = lab.alpha) +
  geom_label_repel(data = lowtide_091022, aes(family = lab.font, label="Low tide: 0.25m", x=time, y=value),colour = lab2.text,fill = lab2.fill,nudge_x = -9000,nudge_y = 80,segment.linetype = 1,segment.curvature = -0.1,segment.ncp = 3,segment.angle = 20,alpha = lab.alpha) +
  theme_wita() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(), 
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank()) +
  ylim(1700, 2600)

wita12.day1
wita12.day2

# patch these two plots together --
wita12.scatter <- wita12.day1 +
  labs(title = "Lady Bay Reef: AudioMoth deployment",
       subtitle = "8th October 2022 (from 12pm) to 9th October 2022 (to 10am)",
       y = "Acoustic Complexity Index") +
  wita12.day2 +
  labs(caption = "Wrangling In The Antipodes: 'Acoustic complexity as an indicator of tidal activity'\nTristan Louth-Robins, 2022-23. Github: /TristanLouthRobins") +
  theme_wita()

wita12.scatter
ggsave("exports-1/WITA_12-ACI_scatter.png", width = 36, height = 24, units = "cm")

# --------------------------------------------------------------------------------------------------#

# 2. Now, let's import the newest dataset, generate some summary stats and visualise the observations --

dataset <- "WITA_13-ACIADIAEI_tidy_ladybay_23122331.csv"
data <- read_csv(dataset)

data %>% filter(index == "acoustic_complexity", site.name == "Meadow cove") %>% nrow() 

# Define general factors for time of day and season (season only to be used later on for longer term analysis) ---------------------------------
data$period <- ordered(data$period, levels = c("pre-dawn", "dawn", "morning", "midday", "afternoon", "dusk", "night"))
data$season <- ordered(data$season, levels = c("Summer", "Autumn", "Winter", "Spring"))
data$index <- as.factor(data$index)

# Create variables from observations specific to unique events over period of overnight deployment --------------------------------------------- 
f <- function(d, h, m){
  filter(data, date == d & hour == h & mins == m)
}

# Note on the below: There's a way more efficient way to iterate this over a loop, but I can't currently suss this out.

high_tides <- tibble()
low_tides <- tibble()

# Tide peaks -
high_tides <- f("2022-12-23", 4, 50) %>% mutate(tide_pred = "high tide") %>%  add_row(high_tides)
high_tides <- f("2022-12-23", 18, 50) %>% mutate(tide_pred = "high tide") %>%  add_row(high_tides)
high_tides <- f("2022-12-24", 5, 20) %>% mutate(tide_pred = "high tide") %>%  add_row(high_tides)
high_tides <- f("2022-12-24", 19, 30) %>% mutate(tide_pred = "high tide") %>%  add_row(high_tides)
high_tides <- f("2022-12-25", 5, 50) %>% mutate(tide_pred = "high tide") %>%  add_row(high_tides)
high_tides <- f("2022-12-25", 19, 50) %>% mutate(tide_pred = "high tide") %>%  add_row(high_tides)
high_tides <- f("2022-12-26", 6, 20) %>% mutate(tide_pred = "high tide") %>%  add_row(high_tides)
high_tides <- f("2022-12-26", 20, 10) %>% mutate(tide_pred = "high tide") %>%  add_row(high_tides)
high_tides <- f("2022-12-27", 6, 50) %>% mutate(tide_pred = "high tide") %>%  add_row(high_tides)
high_tides <- f("2022-12-27", 20, 20) %>% mutate(tide_pred = "high tide") %>%  add_row(high_tides)
high_tides <- f("2022-12-28", 7, 20) %>% mutate(tide_pred = "high tide") %>%  add_row(high_tides)
high_tides <- f("2022-12-28", 20, 30) %>% mutate(tide_pred = "high tide") %>%  add_row(high_tides)
high_tides <- f("2022-12-29", 7, 50) %>% mutate(tide_pred = "high tide") %>%  add_row(high_tides)
high_tides <- f("2022-12-29", 20, 50) %>% mutate(tide_pred = "high tide") %>%  add_row(high_tides)
high_tides <- f("2022-12-30", 8, 10) %>% mutate(tide_pred = "high tide") %>%  add_row(high_tides)
high_tides <- f("2022-12-30", 21, 30) %>% mutate(tide_pred = "high tide") %>%  add_row(high_tides)
high_tides <- f("2022-12-31", 8, 30) %>% mutate(tide_pred = "high tide") %>%  add_row(high_tides)
high_tides <- f("2022-12-31", 22, 30) %>% mutate(tide_pred = "high tide") %>%  add_row(high_tides)
# Tide lows -
low_tides <- f("2022-12-23", 12, 30) %>% mutate(tide_pred = "low tide") %>%  add_row(low_tides)
low_tides <- f("2022-12-23", 22, 50) %>% mutate(tide_pred = "low tide") %>%  add_row(low_tides)
low_tides <- f("2022-12-24", 13, 00) %>% mutate(tide_pred = "low tide") %>%  add_row(low_tides)
low_tides <- f("2022-12-24", 23, 20) %>% mutate(tide_pred = "low tide") %>%  add_row(low_tides)
low_tides <- f("2022-12-25", 13, 40) %>% mutate(tide_pred = "low tide") %>%  add_row(low_tides)
low_tides <- f("2022-12-25", 23, 40) %>% mutate(tide_pred = "low tide") %>%  add_row(low_tides)
low_tides <- f("2022-12-26", 14, 00) %>% mutate(tide_pred = "low tide") %>%  add_row(low_tides)
low_tides <- f("2022-12-27", 00, 10) %>% mutate(tide_pred = "low tide") %>%  add_row(low_tides)
low_tides <- f("2022-12-27", 14, 30) %>% mutate(tide_pred = "low tide") %>%  add_row(low_tides)
low_tides <- f("2022-12-28", 00, 50) %>% mutate(tide_pred = "low tide") %>%  add_row(low_tides)
low_tides <- f("2022-12-28", 14, 50) %>% mutate(tide_pred = "low tide") %>%  add_row(low_tides)
low_tides <- f("2022-12-29", 01, 25) %>% mutate(tide_pred = "low tide") %>%  add_row(low_tides)
low_tides <- f("2022-12-29", 15, 00) %>% mutate(tide_pred = "low tide") %>%  add_row(low_tides)
low_tides <- f("2022-12-30", 02, 10) %>% mutate(tide_pred = "low tide") %>%  add_row(low_tides)
low_tides <- f("2022-12-30", 15, 20) %>% mutate(tide_pred = "low tide") %>%  add_row(low_tides)
low_tides <- f("2022-12-30", 02, 10) %>% mutate(tide_pred = "low tide") %>%  add_row(low_tides)
low_tides <- f("2022-12-30", 15, 20) %>% mutate(tide_pred = "low tide") %>%  add_row(low_tides)

# generate the summary stats for all and daily observations -
# because this dataset captures ACI, ADI and AEI, we'll just limit the summary to ACI -
# the dataset also encompasses TWO sites of observation, and we're just interested in the 
# reef meadow site for these visualisations.

(wita13_overall.stats <- summary.stats(data %>% filter(index == "acoustic_complexity",
                                                       site.name == "Meadow cove"), "all"))
(wita13_daily.stats <- summary.stats(data %>% filter(index == "acoustic_complexity",
                                                     site.name == "Meadow cove"), "daily"))

# visualisation ----------------------------------------------------------------

# function for generating all data in a single scatter plot of observations from selected site --

scatter_viz_single_all <- function(site, 
                                   index.type, 
                                   y.min, y.max){
  
  index.name <- str_replace(index.type, "_", " ") %>% 
    str_to_title()
  
  x.label <- ifelse(index.type == "acoustic_complexity", "ACI", 
                    ifelse(index.type == "acoustic_diversity", "ADI", 
                           ifelse(index.type == "acoustic_evenness", "AEI",
                                  "ACOUSTIC INDEX")))
  
  data %>% 
    filter(hour >= 0 & hour <= 24,
           site.name == site,
           index == index.type) %>% 
    ggplot(aes(x=time, y=value)) +
    # mean value for the period of observation
    geom_point(colour=points, size = 2, alpha = 0.7) +
    geom_hline(data = wita13_overall.stats, aes(yintercept=mean), colour = "#0073ff", alpha = 0.5, linetype="solid", size = 1) +
    geom_label_repel(data = wita13_overall.stats, aes(family = lab.font, label=paste("Mean ACI: ", mean), x=60000, y=mean),colour = lab1.text,fill = lab1.fill,nudge_x = 0,nudge_y = 400,segment.linetype = 1,segment.curvature = -0.1,segment.ncp = 3,segment.angle = 0,alpha = lab.alpha, arrow = arrow(length = unit(0.02, "npc"))) +
    geom_smooth(colour="#ffd445", alpha = 0.6) +
    geom_point(data = filter(high_tides, site.name == site, index == index.type), aes(x=time, y=value), colour = h.tide, shape=24, size=7, stroke=1, alpha=0.8) +
    geom_point(data = filter(low_tides, site.name == site, index == index.type), aes(x=time, y=value), colour = l.tide, shape=25, size=7, stroke=1, alpha=0.8) +
    theme_wita() +
    theme(axis.title.x=element_blank(),
          legend.title = element_blank()) +
    ylim(c(y.min, y.max)) +
    labs( 
         subtitle = paste(index.name, "(2022-12-23 to 2022-12-31)"), 
         y = x.label)
}

# function for generating single scatter plot of observations from selected site and date range --

scatter_viz_single <- function(site, 
                               date.range,
                               index.type, 
                               y.min, y.max){
  
  index.name <- str_replace(index.type, "_", " ") %>% 
    str_to_title()
  
  x.label <- ifelse(index.type == "acoustic_complexity", "ACI", 
                    ifelse(index.type == "acoustic_diversity", "ADI", 
                           ifelse(index.type == "acoustic_evenness", "AEI",
                                  "ACOUSTIC INDEX")))
  
  data %>% 
    filter(date == date.range,
           hour >= 0 & hour <= 24,
           site.name == site,
           index == index.type) %>% 
    ggplot(aes(x=time, y=value)) +
    geom_point(colour=points, size = 2, alpha = 0.7) +
    geom_hline(data = wita13_daily.stats %>% filter(date == date.range), aes(yintercept=mean), colour = "#0073ff", alpha = 0.5, linetype="solid", size = 1) +
    geom_label_repel(data = wita13_daily.stats %>% filter(date == date.range), aes(family = lab.font, label=paste("Mean ACI: ", mean), x=60000, y=mean),colour = lab1.text,fill = lab1.fill,nudge_x = 0,nudge_y = 400,segment.linetype = 1,segment.curvature = -0.1,segment.ncp = 3,segment.angle = 0,alpha = lab.alpha, arrow = arrow(length = unit(0.02, "npc"))) +
    geom_smooth(colour="#ffd445", alpha = 0.6) +
    geom_point(data = filter(high_tides, date == date.range, site.name == site, index == index.type), aes(x=time, y=value), colour = h.tide, shape=24, size=7, stroke=1, alpha=0.8) +
    geom_point(data = filter(low_tides, date == date.range, site.name == site, index == index.type), aes(x=time, y=value), colour = l.tide, shape=25, size=7, stroke=1, alpha=0.8) +
    theme_wita() +
    theme(axis.title.x=element_blank(),
          legend.title = element_blank()) +
    ylim(c(y.min, y.max)) +
    labs( 
         subtitle = paste(" (",date.range, ")", sep=""), 
         y = x.label)
}

# generate the scatter plots ---------------------------------------------------

# all ACI observations from 23rd to 31st of Dec 2022 --
scatter_all <- 
  scatter_viz_single_all("Meadow cove",
                         "acoustic_complexity",
                         1650, 3400)

scatter_all 
ggsave("exports-1/WITA_13-ACI_scatter_all_wtides.png", width = 36, height = 24, units = "cm")

scatter_20221224 <- scatter_viz_single("Meadow cove","2022-12-24","acoustic_complexity",1650, 3400)
scatter_20221225 <- scatter_viz_single("Meadow cove","2022-12-25","acoustic_complexity",1650, 3400)
scatter_20221226 <- scatter_viz_single("Meadow cove","2022-12-26","acoustic_complexity",1650, 3400)
scatter_20221227 <- scatter_viz_single("Meadow cove","2022-12-27","acoustic_complexity",1650, 3400)
scatter_20221228 <- scatter_viz_single("Meadow cove","2022-12-28","acoustic_complexity",1650, 3400)
scatter_20221229 <- scatter_viz_single("Meadow cove","2022-12-29","acoustic_complexity",1650, 3400)
scatter_20221230 <- scatter_viz_single("Meadow cove","2022-12-30","acoustic_complexity",1650, 3400)

scatter_20221224
ggsave("exports-1/WITA_13-ACI_scatter_20221224.png", width = 36, height = 24, units = "cm")
scatter_20221225
ggsave("exports-1/WITA_13-ACI_scatter_20221225.png", width = 36, height = 24, units = "cm")
scatter_20221226
ggsave("exports-1/WITA_13-ACI_scatter_20221226.png", width = 36, height = 24, units = "cm")
scatter_20221227
ggsave("exports-1/WITA_13-ACI_scatter_20221227.png", width = 36, height = 24, units = "cm")
scatter_20221228
ggsave("exports-1/WITA_13-ACI_scatter_20221228.png", width = 36, height = 24, units = "cm")
scatter_20221229
ggsave("exports-1/WITA_13-ACI_scatter_20221229.png", width = 36, height = 24, units = "cm")
scatter_20221230
ggsave("exports-1/WITA_13-ACI_scatter_20221230.png", width = 36, height = 24, units = "cm")

# annotated plots with highlighted points and areas of interest --

# single point highlighted --
scatter_viz_single("Meadow cove","2022-12-24","acoustic_complexity",1650, 3400) + 
  geom_point(data = data %>%  filter(day == 24, hour == 14, mins == 00, site.name == "Meadow cove", index == "acoustic_complexity"),
             aes(x=time, y=value), size = 7, colour = "red", shape = 19, stroke = 1, alpha = 0.5)

ggsave("exports-1/WITA_13-ACI_scatter-anno_20221224-1500.png", width = 36, height = 24, units = "cm")

# area highlighted --
scatter_viz_single("Meadow cove","2022-12-24","acoustic_complexity",1650, 3400) + 
  geom_rect(aes(xmin = 37000,
                xmax = 70000,
                ymin = 1650,
                ymax = 2400),
            fill = NA,
            colour = "blue",
            size = 1)

ggsave("exports-1/WITA_13-ACI_scatter-anno_20221224-low_ACI.png", width = 36, height = 24, units = "cm")

# patch the plots together ---

all_singles <- 
scatter_20221224 | scatter_20221225 | scatter_20221226 | scatter_20221227 |
  scatter_20221228 | scatter_20221229 | scatter_20221230

ggsave("exports-1/WITA_13-ACI_scatter_all.png", width = 64, height = 12, units = "cm")

# Extra-annotated version of the plot for 24th of December --
# For comparison with time-compressed annotated spectrogram --

scatter_viz_single("Meadow cove","2022-12-24","acoustic_complexity",1650, 3400) +
  geom_vline(data = data %>% filter(date == "2022-12-24",hour == 2, mins == 0), aes(xintercept = time), colour = "#6d9e09", alpha = 0.6) +
  geom_label(data = data %>% filter(date == "2022-12-24", hour == 2, mins == 0), aes(family = lab.font, label="2:00", x=time, y=3000),colour = lab1.text,fill = lab1.fill,nudge_x = 0,nudge_y = 400) +
  geom_vline(data = data %>% filter(date == "2022-12-24",hour == 9, mins == 0), aes(xintercept = time), colour = "#6d9e09", alpha = 0.6) +
  geom_label(data = data %>% filter(date == "2022-12-24", hour == 9, mins == 0), aes(family = lab.font, label="9:00", x=time, y=3000),colour = lab1.text,fill = lab1.fill,nudge_x = 0,nudge_y = 400) +
  geom_vline(data = data %>% filter(date == "2022-12-24",hour == 15, mins == 40), aes(xintercept = time), colour = "#6d9e09", alpha = 0.6) +
  geom_label(data = data %>% filter(date == "2022-12-24", hour == 15, mins == 40), aes(family = lab.font, label="15:40", x=time, y=3000),colour = lab1.text,fill = lab1.fill,nudge_x = 0,nudge_y = 400) +
  geom_vline(data = data %>% filter(date == "2022-12-24",hour == 19, mins == 0), aes(xintercept = time), colour = "#6d9e09", alpha = 0.6) +
  geom_label(data = data %>% filter(date == "2022-12-24", hour == 19, mins == 0), aes(family = lab.font, label="19:00", x=time, y=3000),colour = lab1.text,fill = lab1.fill,nudge_x = 0,nudge_y = 400) 

ggsave("exports-1/WITA_13-ACI_scatter-xtra_anno_20221224.png", width = 36, height = 24, units = "cm")
 
