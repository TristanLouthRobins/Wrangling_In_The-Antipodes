# ~WRANGLING IN THE ANTIPODES~            
# 13: Further explorations of ACI on Lady Bay Reef                 
# Tristan Louth-Robins, January 2023     
########################################################################################
# Post can found here:                                                                 #
# https:// #
########################################################################################

# Required dependencies ----------------------------------------------------------------
library(tidyverse)
library(rlang)
library(gghighlight)
library(ggplot2)
library(ggrepel)
library(patchwork)

# Set WD and import dataset --------------------------------------------------------------------------------------------------------------------
setwd("/Users/tristanlouth-robins/Documents/Documents - MacBook Pro/R data and projects/acoustic_ecology_tests/WITA_inprogress/")
dataset <- "data/lady_bay_reef_december_ACI_ADI_AEI.csv"
data <- read_csv(dataset)

data <- data %>% distinct() # ensure that there is no duplicated data.

# Note: This above dataset has been pre-processed with acoustic complexity analysis and tidying using this R script:
# https://github.com/TristanLouthRobins/Wrangling_In_The-Antipodes/blob/main/WITA_G-audiomoth_compute_indices_and_tidy_data

# Define general factors for time of day and season (season only to be used later on for longer term analysis) ---------------------------------
data$period <- ordered(data$period, levels = c("pre-dawn", "dawn", "morning", "midday", "afternoon", "dusk", "night"))
data$season <- ordered(data$season, levels = c("Summer", "Autumn", "Winter", "Spring"))
data$index <- as.factor(data$index)

# Create variables from observations specific to unique events over period of overnight deployment --------------------------------------------- 
f <- function(d, h, m){
  filter(data, date == d & hour == h & mins == m)
}

high_tides <- tibble()
low_tides <- tibble()
# Note on the below: There's a way more efficient way to iterate this over a loop, but I can't currently suss this out.
# Tide peaks -
high_tides <- f("2022-12-23", 4, 50) %>% mutate(tide_pred = "high tide") %>%  add_row(high_tides)
high_tides <- f("2022-12-23", 18, 50) %>% mutate(tide_pred = "high tide") %>%  add_row(high_tides)
high_tides <- f("2022-12-24", 5, 20) %>% mutate(tide_pred = "high tide") %>%  add_row(high_tides)
high_tides <- f("2022-12-24", 19, 30) %>% mutate(tide_pred = "high tide") %>%  add_row(high_tides)
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

# visualisation ----------------------------------------------------------------

# aesthetics --
plot.bg <- "#E7FFFF"
panel.bg <- "#11191a"
grid.minor <- "#000000"
grid.major <- "#304647"
leg.bg <- plot.bg
h.tide <- "#f511e6"
l.tide <- "#17e0ff"  
label_alpha <- 0.5

# Scatter plot theme
theme_wita <- function(){
  theme(
    text = element_text(family = "Avenir", colour = "black"),
    plot.margin = margin(5,2,5,2, "mm"),
    plot.background = element_rect(fill = plot.bg, colour = plot.bg),
    panel.background = element_rect(fill = panel.bg),
    panel.grid.major = element_line(colour = grid.major, size = 0.5),
    panel.grid.minor = element_line(colour = grid.major, size = 0.5),
    legend.title = element_text(family = "Avenir", size = 9),
    legend.text = element_text(family = "Avenir", size = 8),
    legend.background = element_rect(fill = leg.bg),
    legend.key = element_rect(fill = panel.bg),
    legend.position = "right"
  )
}

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
    geom_point(aes(colour=period), size = 2, alpha = 0.7) +
    scale_color_brewer(palette = "PRGn", direction=-1) +
    geom_smooth(colour="#ffd445", alpha = 1) +
    theme_wita() +
    theme(axis.title.x=element_blank(),
          legend.title = element_blank()) +
    ylim(c(y.min, y.max)) +
    labs(title = paste(site, "site"), 
         subtitle = paste(index.name, "2022-12-23 to 2022-12-31"), 
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
    geom_smooth(colour="#ffd445", alpha = 1) +
    geom_point(aes(colour=period), size = 2, alpha = 0.7) +
    scale_color_brewer(palette = "PRGn", direction=-1) +
    geom_point(data = filter(high_tides, date == date.range, site.name == site, index == index.type), aes(x=time, y=value), colour = h.tide, shape=24, size=7, stroke=1, alpha=0.8) +
    geom_point(data = filter(low_tides, date == date.range, site.name == site, index == index.type), aes(x=time, y=value), colour = l.tide, shape=25, size=7, stroke=1, alpha=0.8) +
    theme_wita() +
    theme(axis.title.x=element_blank(),
          legend.title = element_blank()) +
    ylim(c(y.min, y.max)) +
    labs(title = paste(site, "site"), 
         subtitle = paste(index.name, date.range), 
         y = x.label)
}

# function for generating faceted scatter plot of observations from selected site and date range --
# since 23/12 and 31/12 didn't collect a full day's data these dates have been removed.

omit.date.1 <- "2022-12-23"
omit.date.2 <- "2022-12-31"

scatter_viz_facet <- function(site, 
                               index.type, 
                               y.min, y.max,
                              n.rows){
  
  index.name <- str_replace(index.type, "_", " ") %>% 
    str_to_title()
  
  x.label <- ifelse(index.type == "acoustic_complexity", "ACI", 
                    ifelse(index.type == "acoustic_diversity", "ADI", 
                           ifelse(index.type == "acoustic_evenness", "AEI",
                                  "ACOUSTIC INDEX")))
  
  data %>% 
    filter(date != omit.date.1 & date != omit.date.2,
           hour >= 0 & hour <= 24,
           site.name == site,
           index == index.type) %>% 
    ggplot(aes(x=time, y=value)) +
    geom_smooth(colour="#ffd445", alpha = 1) +
    geom_point(aes(colour=period), size = 2, alpha = 0.7) +
    scale_color_brewer(palette = "PRGn", direction=-1) +
    geom_point(data = filter(high_tides, date != omit.date.1 & date != omit.date.2, site.name == site, index == index.type), aes(x=time, y=value), colour = h.tide, shape=24, size=7, stroke=1, alpha=0.6) +
    geom_point(data = filter(low_tides, date != omit.date.1 & date != omit.date.2, site.name == site, index == index.type), aes(x=time, y=value), colour = l.tide, shape=25, size=7, stroke=1, alpha=0.6) +
    theme_wita() +
    theme(axis.title.x=element_blank(),
          legend.title = element_blank()) +
    ylim(c(y.min, y.max)) +
    labs(title = paste(site, "site"), 
         subtitle = paste(index.name), 
         y = x.label) +
    facet_wrap(~date, nrow = n.rows)
}

################################################################################

single_date_set <- "2022-12-26"

scatter_all <- 
  scatter_viz_single_all("Meadow cove",
                         "acoustic_complexity",
                         1500, 4000)

scatter_all
ggsave("exports-1/ACI-meadow_all_dates.png", width = 24, height = 12, units = "cm")

scatter_single <- 
  scatter_viz_single("Meadow cove",
                     single_date_set, 
                     "acoustic_complexity",
                     1500, 4000)

scatter_single
ggsave("exports-1/ACI-meadow_20221226.png", width = 24, height = 12, units = "cm")

scatter_facet <- 
  (scatter_viz_facet("Meadow cove",
                     "acoustic_complexity",
                     1500, 4000,
                     7))

scatter_facet
ggsave("exports-1/ACI-meadow_all_dates_facet.png", width = 24, height = 84, units = "cm")


boxplot_all <- 
data %>% 
  filter(index == "acoustic_complexity",
         site.name == "Meadow cove",
         date != "2022-12-23" & date != "2022-12-31") %>% 
  ggplot(aes(y=value, x=fct_rev(as.factor(date)))) +
  geom_boxplot(colour = "white", fill = "#2e4547") +
  geom_jitter(aes(colour=period), width=0.05, height=0.1, pch=1, size=2, alpha=0.5) +
  scale_color_brewer(palette = "PRGn", direction=-1) +
  labs(title = "Meadow cove site: Acoustic Complexity",
       subtitle = "Boxplot details mean, quartiles and range of data. Overlaid with jittered data points showing distributions on single axis",
       x = "",
       y = "ACI") +
  ylim(1500, 4000) +
  theme_wita()

boxplot_all
ggsave("exports-1/ACI-meadow_boxplot_all.png", width = 24, height = 16, units = "cm")

violin_all <- 
  data %>% 
  filter(index == "acoustic_complexity",
         site.name == "Meadow cove",
         date != "2022-12-23" & date != "2022-12-31") %>% 
  ggplot(aes(y=value, x=fct_rev(as.factor(date)))) +
  geom_violin(colour = "white", fill = "#2e4547") +
  stat_summary(fun.y=median, geom="point", size=2, color="red") +
#  geom_jitter(aes(colour=period), width=0.05, height=0.1, pch=1, size=2, alpha=0.8) +  
  scale_color_brewer(palette = "PRGn", direction=-1) +
  labs(title = "Meadow cove site: Acoustic Complexity",
       subtitle = "Violin plot details density and range of data. Overlaid with point representing median.. Overlaid with jittered data points showing distributions on single axis",
       x = "",
       y = "ACI") +
  ylim(1500, 4000) +
  theme_wita()

violin_all
ggsave("exports-1/ACI-meadow_violin_all.png", width = 24, height = 16, units = "cm")

boxplot_single <- 
  data %>% 
  filter(index == "acoustic_complexity",
         site.name == "Meadow cove",
         date == single_date_set) %>% 
  ggplot(aes(y=value, x=as.factor(period))) +
  geom_boxplot(colour = "white", fill = "#2e4547") +
  geom_jitter(aes(colour=period), width=0.05, height=0.1, pch=1, size=2, alpha=0.8) +
  scale_color_brewer(palette = "PRGn", direction=-1) +
  labs(title = "",
       subtitle = "Boxplot details mean, quartiles and range of data. Overlaid with jittered data points showing distributions on single axis",
       x = "",
       y = "ACI") +
  ylim(1500, 4000) +
  theme_wita() +
  theme(legend.title = element_blank()) 

boxplot_single
ggsave("exports-1/ACI-meadow_boxplot_24.png", width = 8, height = 8, units = "cm")

violin_single <- 
  data %>% 
  filter(index == "acoustic_complexity",
         site.name == "Meadow cove",
         date == single_date_set) %>% 
  ggplot(aes(y=value, x=as.factor(period))) +
  geom_violin(colour = "white", fill = "#2e4547") +
  stat_summary(fun.y=median, geom="point", size=2, color="red") +
#  geom_jitter(aes(colour=period), width=0.05, height=0.1, pch=1, size=2, alpha=0.8) +
  scale_color_brewer(palette = "PRGn", direction=-1) +
  labs(title = "",
       subtitle = "Violin plot details density and range of data. Overlaid with point representing median.",
       x = "",
       y = "ACI") +
  ylim(1500, 4000) +
  theme_wita() +
  theme(legend.title = element_blank()) 

violin_single
ggsave("exports-1/ACI-meadow_violin_26.png", width = 8, height = 8, units = "cm")


# patching these plots together ------------------------------------------------
# Scatter plot surrounding annotation theme
theme_patch_wita <- function(){
  theme(
    text = element_text(family = "Avenir", colour = "black"),
    plot.title = element_text(size = 18),
    plot.margin = margin(5,2,5,2, "mm"),
    plot.background = element_rect(fill = plot.bg, colour = plot.bg),
    panel.background = element_rect(fill = panel.bg),
    panel.grid.major = element_line(colour = grid.major, size = 0.5),
    panel.grid.minor = element_line(colour = grid.major, size = 0.5),
    legend.title = element_text(family = "Avenir", size = 9),
    legend.text = element_text(family = "Avenir", size = 8),
    legend.background = element_rect(fill = leg.bg),
    legend.key = element_rect(fill = panel.bg),
    legend.position = "right"
  )
}

scatter_and_violin <- 
scatter_single / violin_single +
  plot_annotation(title = paste("Lady Bay Reef: Audiomoth deployment"),
                  caption = "Wrangling In The Antipodes: 'Further exploration of Lady Bay Reef'\nSeagrass meadow cove lat-lon: (-35.473913, 138.282811)\n\nTristan Louth-Robins, 2023. Github: /TristanLouthRobins",
                  theme = theme_patch_wita())

scatter_and_violin
ggsave("exports-1/ACI-meadow_scatter_violin_26.png", width = 36, height = 24, units = "cm")




# STATISTICAL CORRELATION: seagrass meadow -------------------------------------

# Correlation plot theme
theme_cor_wita <- function(){
  theme(
    text = element_text(family = "Avenir", colour = "black"),
    plot.margin = margin(5,2,5,2, "mm"),
    plot.background = element_rect(fill = "#ffffff"),
    panel.background = element_rect(fill = "#2e4a45"),
    panel.grid.major = element_line(colour = "#21403a"),
    panel.grid.minor = element_blank(),
    legend.title = element_text(family = "Courier", size = 9, colour = "#000000"),
    legend.text = element_text(family = "Courier", size = 8, colour = "#000000"),
    legend.key = element_rect(fill = "#000000", colour = "#FFFFFF"),
    legend.background = element_rect(fill = "#FFFFFF")
  )
}

# correlation for entire week --------------------------------------------------

cor_aci <- data %>% 
  filter(date != "2022-12-23" & date != "2022-12-31") %>% 
  filter(index == "acoustic_complexity") %>% 
  pivot_wider(names_from = site.name, values_from = value)

ggplot(cor_aci, aes(x=`Meadow cove`, y=`Reef extent 1`)) +
  geom_point(aes(colour = period), alpha = 0.7) +
  scale_color_brewer(palette = "Spectral") +
  geom_smooth(method = "lm", colour = "#FFF000", alpha = 0.7, size =0.5) +
  facet_wrap(~date) +
  theme_cor_wita() 

# correlation for 30/12/22 -----------------------------------------------------

cor_aci <- data %>% 
  filter(date != "2022-12-23" & date != "2022-12-31") %>% 
  filter(index == "acoustic_complexity") %>% 
  pivot_wider(names_from = site.name, values_from = value)

cor_30 <- 
ggplot(cor_aci, aes(x=`Meadow cove`, y=`Reef extent 1`)) +
  geom_point(aes(colour = period), alpha = 0.7) +
  scale_color_brewer(palette = "Spectral") +
  labs(title = "30th December 2022",
       x = "ACI: Reef extent location",
       y = "ACI: Seagrass meadow") +
  xlim(1600, 4000) +
  ylim(1600, 4000) +
  facet_wrap(~period, nrow=1) +
  theme_cor_wita() +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust =1),
  legend.position = "none")


cor_30

# animation of the above -------------------------------------------------------

library(gganimate)

ggplot(cor_aci, aes(x=`Meadow cove`, y=`Reef extent 1`)) +
  geom_point(aes(colour = period), size = 3, alpha = 0.7) +
  scale_color_brewer(palette = "Spectral") +
  theme_cor_wita() +
  facet_wrap(~period) +
  # gganimate components: 
  labs(title = "{frame_time}",
       x = "Reef extent location",
       y = "Seagrass meadow") +
  xlim(1600, 4000) +
  ylim(1600, 4000) +
  transition_time(date) +
  ease_aes('sine-in-out')


cor <- lm(`Reef extent 1` ~ `Meadow cove`, data = cor_aci)
summary(cor)$r.squared  # R^2 for aci correlation between two sites: 0.748

# Next steps -> sample the data to test the model.

################################################################################
################################################################################

# Polar plot -------------------------------------------------------------------

# Scatter plot theme
theme_polar_wita <- function(){
  theme(
    text = element_text(family = "Avenir", colour = "black"),
    axis.text = element_text(colour = "white"),
    plot.margin = margin(5,2,5,2, "mm"),
    plot.background = element_rect(fill = "#ffffff"),
    panel.background = element_rect(fill = "#2e4a45"),
    panel.grid.major = element_line(colour = "#21403a"),
    panel.grid.minor = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    legend.title = element_text(family = "Courier", size = 9),
    legend.text = element_text(family = "Courier", size = 8),
    legend.background = element_rect(fill = "#ffffff")
  )
}

b <- data %>% 
  filter(date != "2022-12-23" & date != "2022-12-31") %>% 
  ggplot(aes(x=time, y=value)) +
  geom_line(aes(colour=period)) +
  geom_point(data = filter(high_tides, date != "2022-12-23" & date != "2022-12-31"), aes(x=time, y=value), colour = "red", shape=24, size=3, stroke=1) +
  geom_point(data = filter(low_tides, date != "2022-12-23" & date != "2022-12-31"), aes(x=time, y=value), colour = "#03fcd3", shape=25, size=3, stroke=1, alpha=0.6) +
  scale_color_brewer(palette = "Set2", direction = 1) +
  ylim(1700, 4000) +
  theme_polar_wita() +
  facet_wrap(~date)

polar <- b + coord_polar()

polar + labs(title = "Lady Bay Reef: AudioMoth deployment\n24/12/22 to 30/12/22",
             subtitle = "Acoustic Complexity measured over a seven-day deployment.",
             y = "ACI value") +
  labs(caption = "Wrangling In The Antipodes: 'Further exploration of ACI on Lady Bay Reef'\nTristan Louth-Robins, 2023. Github: /TristanLouthRobins") 
