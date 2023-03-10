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
# https://patchwork.data-imaginist.com/articles/guides/layout.html

# Set WD and import dataset --------------------------------------------------------------------------------------------------------------------
setwd("/Users/tristanlouth-robins/Documents/Documents - MacBook Pro/R data and projects/acoustic_ecology_tests/WITA_projects/data-WITA_13/")
dataset <- "lady_bay_reef_december_ACI_ADI_AEI.csv"
data <- read_csv(dataset)

data <- data %>% distinct() # ensure that there is no duplicated data.

# Note: This above dataset has been derived from prior acoustic complexity analysis and tidying using this R script:
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

# Scatter plot -----------------------------------------------------------------

# Scatter plot theme
theme_scatter_wita <- function(){
  theme(
    text = element_text(family = "Avenir", colour = "black"),
    plot.margin = margin(5,2,5,2, "mm"),
    plot.background = element_rect(fill = "#DDFFFF"),
    panel.background = element_rect(fill = "#DDFFFF"),
    panel.grid.major = element_line(colour = "#99CCFF"),
    panel.grid.minor = element_blank(),
    legend.title = element_text(family = "Courier", size = 9),
    legend.text = element_text(family = "Courier", size = 8),
    legend.background = element_rect(fill = "#ffffff")
  )
}

label_alpha <- 0.5

omit.date.1 <- "2022-12-23"
omit.date.2 <- "2022-12-31"

scatter_viz_facted <- function(site, index.type, index.name, p_col, x.label, y.min, y.max, title){
  data %>% 
    filter(date != omit.date.1 & date != omit.date.2,
           site.name == site,
           index == index.type) %>% 
    ggplot(aes(x=time, y=value)) +
    geom_point(colour=p_col, size = 1) +
    geom_smooth(colour="#ffffff") +
    geom_point(data = filter(high_tides, date != "2022-12-23" & date != "2022-12-31", site.name == site, index == index.type), aes(x=time, y=value), colour = "red", shape=24, size=4, stroke=1, alpha=0.6) +
    geom_point(data = filter(low_tides, date != "2022-12-23" & date != "2022-12-31", site.name == site, index == index.type), aes(x=time, y=value), colour = "#03fcd3", shape=25, size=4, stroke=1, alpha=0.6) +
    theme_scatter_wita() +
    theme(axis.title.x=element_blank()) +
    ylim(c(y.min, y.max)) +
      labs(subtitle = paste(index.name, " "), y = x.label) +
    facet_wrap(~date, nrow = 1)
}

sel.date <- "2022-12-24"

scatter_viz_single <- function(site, index.type, index.name, p_col, x.label, y.min, y.max, title){
  data %>% 
    filter(date == sel.date,
           site.name == site,
           index == index.type) %>% 
    ggplot(aes(x=time, y=value)) +
    geom_smooth(colour="#009CCE", alpha = 0.5) +
    geom_point(colour=p_col, size = 2, alpha = 0.7) +
    geom_point(data = filter(high_tides, date == sel.date, site.name == site, index == index.type), aes(x=time, y=value), colour = "#BF2D42", shape=24, size=6, stroke=1, alpha=0.6) +
    geom_point(data = filter(low_tides, date == sel.date, site.name == site, index == index.type), aes(x=time, y=value), colour = "#0A46EE", shape=25, size=6, stroke=1, alpha=0.6) +
    theme_scatter_wita() +
    theme(axis.title.x=element_blank()) +
    ylim(c(y.min, y.max)) +
    labs(title = paste(index.name, "- Seagrass meadow"), 
         subtitle = "24th December 2022 (12am to 11:59pm)", 
         y = x.label)
}

(aci_meadow.2412 <- scatter_viz_single("Meadow cove", "acoustic_complexity", "Acoustic Complexity", "#3399FF", "ACI", 1500, 4000))

ggsave("exports-1/ACI-meadow_20221224.png", width = 8, height = 4)

 aci_meadow <- scatter_viz("Meadow cove", "acoustic_complexity", "Acoustic Complexity", "#F7BD5A", "ACI", 1500, 4000)
adi_meadow <- scatter_viz("Meadow cove", "acoustic_diversity", "Acoustic Diversity", "#f5b342", "ADI", 2, 2.3)
aei_meadow <- scatter_viz("Meadow cove", "acoustic_evenness", "Acoustic Evenness", "green", "AEI", 0, 0.75)

aci_ex <- scatter_viz("Reef extent 1", "acoustic_complexity", "Acoustic Complexity", "#eaf043", "ACI", 1500, 4000)
adi_ex <- scatter_viz("Reef extent 1", "acoustic_diversity", "Acoustic Diversity", "#f5b342", "ADI", 2, 2.3)
aei_ex <- scatter_viz("Reef extent 1", "acoustic_evenness", "Acoustic Evenness", "green", "AEI", 0, 0.75)

# labs(title = paste("Lady Bay Reef: AudioMoth deployment - ", "Meadow cove", "\n24/12/22 to 30/12/22", sep="")) + 
aci_meadow / adi_meadow / aei_meadow +
  plot_annotation(title = paste("Lady Bay Reef: AudioMoth deployment - ", "Seagrass meadow cove (-35.473913, 138.282811)", "\n24/12/22 to 30/12/22", sep=""), 
                  caption = "Wrangling In The Antipodes: 'Further exploration of Lady Bay Reef'\nTristan Louth-Robins, 2023. Github: /TristanLouthRobins") 

####

data %>% 
  filter(date != "2022-12-23" & date != "2022-12-31",
         site.name == "Meadow cove",
         index == "acoustic_complexity") %>% 
            ggplot() +
                geom_histogram(aes(x = value), bins = 50) +
  facet_wrap(~date, nrow = 1)

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
