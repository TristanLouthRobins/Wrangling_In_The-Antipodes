
# TITLE HERE:

# Required libraries ----------------------------------------------------------------
library(tidyverse)
library(rlang)
library(gghighlight)
library(ggplot2)
library(ggrepel)
library(patchwork)
library(plotly)
library(showtext)

# Import the dataset:
dataset <- "data/WITA_15-ACIADIAEI_tidy_ladybay_23122331.csv"
data <- read_csv(dataset)

# Restrict the acoustic index to only ACI:
data <- data %>% filter(index == "acoustic_complexity")

# Define a function to generate summary stats: 
# date_range: use 'daily' for specific data. use 'all_dates' for entire period.
summary.stats <- function(data, date_range) {
  if(date_range == "all_dates") {
    summary <- 
      data %>% 
      group_by(site.name) %>% 
      summarise(min = min(value),
                max = max(value),
                mean = round(mean(value),0),
                med = median(value),
                std.dev = sd(value)) %>% 
      ungroup() %>% 
      arrange(site.name)
  } else {
    summary <- 
      data %>% 
      group_by(date, site.name) %>% 
      summarise(min = min(value),
                max = max(value),
                mean = round(mean(value),0),
                med = median(value),
                std.dev = sd(value)) %>% 
      ungroup() %>% 
      arrange(site.name)
  }
  return(summary)
}

# Store the summary stats as unique tibbles for these observations --
# Entire data range:
(overall.stats_all <- summary.stats(data, "all_dates"))
(overall.stats_days <- summary.stats(data, "daily"))

# Define general factors for time of day and season (season only to be used later on for longer term analysis) ---------------------------------
data$period <- ordered(data$period, levels = c("pre-dawn", "dawn", "morning", "midday", "afternoon", "dusk", "night"))
data$season <- ordered(data$season, levels = c("Summer", "Autumn", "Winter", "Spring"))
data$index <- as.factor(data$index)

# Create variables from observations specific to unique events over period of overnight deployment --------------------------------------------- 
key_events <- function(d, h, m){
  filter(data, date == d & hour == h & mins == m)
}

# Visualisation aesthetics --
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

# Scatter plot theme set --
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

# Function for generating all data in a single scatter plot of observations from selected site --
scatter_viz_whole_period <- function(site,
                                   summary_data,
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
    geom_point(aes(colour=period), size = 2, alpha = 0.7) +
    scale_color_brewer(palette = "Dark2") +
    geom_hline(data = summary_data, aes(yintercept=mean), colour = "#0073ff", alpha = 0.5, linetype="solid", size = 1) +
    geom_label_repel(data = summary_data, aes(family = lab.font, label=paste("Mean ACI: ", mean), x=60000, y=mean),colour = lab1.text,fill = lab1.fill,nudge_x = 0,nudge_y = 400,segment.linetype = 1,segment.curvature = -0.1,segment.ncp = 3,segment.angle = 0,alpha = lab.alpha, arrow = arrow(length = unit(0.02, "npc"))) +
    geom_smooth(colour="#ffd445", alpha = 0.6) +
    #    geom_point(data = filter(high_tides, site.name == site, index == index.type), aes(x=time, y=value), colour = h.tide, shape=24, size=7, stroke=1, alpha=0.8) +
    #    geom_point(data = filter(low_tides, site.name == site, index == index.type), aes(x=time, y=value), colour = l.tide, shape=25, size=7, stroke=1, alpha=0.8) +
    theme_wita() +
    theme(axis.title.x=element_blank(),
          legend.title = element_blank(),
          legend.position = "none") +
    ylim(c(y.min, y.max)) +
    labs( 
      subtitle = paste(site, ": ", index.name, "(2022-12-23 to 2022-12-31)"), 
      y = x.label)
}

meadow_stat <- overall.stats_all %>% filter(site.name == "Meadow cove")
extent_stat <- overall.stats_all %>% filter(site.name == "Reef extent 1")

scatter_viz_whole_period("Meadow cove",
                         meadow_stat,
                         "acoustic_complexity",
                         1650, 4100)

ggsave("outputs-WITA15/WITA_15-meadow_overall_scatter.png", width = 36, height = 24, units = "cm")

scatter_viz_whole_period("Reef extent 1",
                         extent_stat,
                         "acoustic_complexity",
                         1650, 4100)

ggsave("outputs-WITA15/WITA_15-extent_overall_scatter.png", width = 36, height = 24, units = "cm")

# Function for generating all data in a single box plot of observations from selected site --
boxplot_viz_whole_period <- function(site,
                                     summary_data,
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
    ggplot(aes(x=period, y=value)) +
    # mean value for the period of observation
    geom_boxplot(aes(colour=period), size = 1, alpha = 0.7) +
    scale_color_brewer(palette = "Dark2") +
    theme_wita() +
    theme(axis.title.x=element_blank(),
          legend.title = element_blank(),
          legend.position = "none") +
    ylim(c(y.min, y.max)) +
    labs( 
      subtitle = paste(site, ": ", index.name, "(2022-12-23 to 2022-12-31)"), 
      y = x.label)
}

boxplot_viz_whole_period("Meadow cove",
                         extent_stat,
                         "acoustic_complexity",
                         1650, 4100)

ggsave("outputs-WITA15/WITA_15-meadow_overall_boxplot.png", width = 24, height = 24, units = "cm")


boxplot_viz_whole_period("Reef extent 1",
                         extent_stat,
                         "acoustic_complexity",
                         1650, 4100)

ggsave("outputs-WITA15/WITA_15-extent_overall_boxplot.png", width = 24, height = 24, units = "cm")

