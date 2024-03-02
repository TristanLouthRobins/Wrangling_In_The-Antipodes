# REWRITE THIS SCRIPT TO MAKE IT ACCOMODATE BOTH SITES!

# ~ WRANGLING IN THE ANTIPODES ~            
# 15: Lady Bay Reef Survey: Part 2 - southern reef fringe
# Tristan Louth-Robins, January 2024
# Blog post can found here: https://

# Required libraries -----------------------------------------------------------
library(tidyverse)
library(rlang)
library(gghighlight)
library(ggplot2)
library(ggrepel)
library(patchwork)
library(plotly)
library(showtext)

# DATA IMPORT AND PREP ---------------------------------------------------------
# ------------------------------------------------------------------------------

dataset <- "data/WITA_15-ACIADIAEI_tidy_ladybay_23122331.csv"
data <- read_csv(dataset) 
head(data)

# Define a function to generate summary stats (either overall or grouped by date)
summary.stats <- function(data, condition) {
  if(condition == "all") {
    result <- 
      data %>% 
      group_by(index, site.name) %>% 
      summarise(min = min(value),
                max = max(value),
                mean = round(mean(value),0),
                med = median(value),
                std.dev = sd(value)) %>% 
      ungroup()
  } else {
    result <- 
      data %>% 
      group_by(index, site.name, date) %>% 
      summarise(min = min(value),
                max = max(value),
                mean = round(mean(value),0),
                med = median(value),
                std.dev = sd(value)) %>% 
      ungroup()
  }
  return(result)
}

# Examine the summary stats for the entire dataset by calling the summary.stats function
summary.stats(data, "all")
summary.stats(data, "daily") %>%  print(n = Inf)

# We only want Acoustic Complexity for this analysis though, so let's filter the dataset
dataACI <- data %>% 
  filter(index == "acoustic_complexity")

# Now we'll store the summary stats for the whole period and each day
(wita15_overall.stats <- summary.stats(dataACI, "all"))
(wita15_daily.stats <- summary.stats(dataACI, "daily"))

# VISUALISATION ----------------------------------------------------------------
# ------------------------------------------------------------------------------

# 1. Define aesthetics:
plot.bg <- "#f4f5f2"
panel.bg <- "#f6f7f5"
grid.minor <- "#e8ebe6"
grid.major <- "#e8ebe6"
leg.bg <- plot.bg
h.tide <- "#30022d"
l.tide <- "#190b96"
points <- "#245757"
lab1.text <- "#320c85"
lab1.fill <- "#ffffff"
lab2.text <- "#000000"
lab2.fill <- "#afa9ba"
lab.alpha <- 0.5
titles.font <- "Helvetica"
cap.font <- "Avenir Next Condensed Regular"
lab.font <- "Avenir Next Condensed Regular"

# 2. Create a little datatable for visualising the respective sites on a map:
map_data <- tibble(site = c("Seagrass Meadow", "Reef fringe"),
                   gps = c("-35.473900, 138.282834","-35.473636, 138.282149"))


# 3. Define general plot theme
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

# 4. Visualise the mean daily ACI of the Seagrass meadow and the Reef fringe
wita15daily_ACI <- 
wita15_daily.stats %>% 
  filter(date != "2022-12-23" & date != "2022-12-31") %>% 
  ggplot() +
  geom_point(aes(x = date, y = mean, colour=site.name), size=2) +
  geom_line(aes(x = date, y = mean, colour=site.name, group=site.name)) +
  labs(title = "Daily ACI of each site (24 to 30 December 2022)", x = date, y = "Mean ACI") +
  theme_wita() +
  theme(axis.title.x=element_blank(),
        legend.title = element_blank(),
        legend.position = "right") +
  scale_x_date(date, date_breaks = "1 day") +
  scale_y_continuous(breaks = seq(2000, 3600, by = 200))

wita15daily_ACI
ggsave("exports-WITA15/1.WITA_15-ACI_both_daily_site_avg.png", width = 24, height = 12, units = "cm")

# 5. Define general factors for time of day and season (season only to be used later on for longer term analysis) ---------------------------------
data$period <- ordered(data$period, levels = c("pre-dawn", "dawn", "morning", "midday", "afternoon", "dusk", "night"))
data$season <- ordered(data$season, levels = c("Summer", "Autumn", "Winter", "Spring"))
data$index <- as.factor(data$index)

# 6. Create a new variable called 'seconds' for each time mark:
data$secs <- 0
# Loop through each time and convert to seconds
for (i in 1:nrow(data)) {
  time_components <- strsplit(as.character(data$time[i]), ":")
  hours <- as.numeric(time_components[[1]][1])
  minutes <- as.numeric(time_components[[1]][2])
  data$secs[i] <- hours * 3600 + minutes * 60
}

# 7. Import the tide data (which is stored as a .tsv file) and convert to a tibble()
tide_data <- "data/WITA_15-tide_data.tsv"
(tides <- read_tsv(tide_data) %>% 
  mutate(Date = as.character(Date),
         Mins = as.double(Mins)))
# Initialise an.empty tibble to store the tide data 
tide_marks <- tibble()
# Loop through the tide data to find observations that match, write these to the empty tibble (THIS IS WAYYY MORE EFFICIENT THAN MY PREVIOUS APPROACH!)
for (i in 1:nrow(tides)) {
  tide_marks <- 
  dataACI %>% 
    filter(date == tides$Date[i] &
           hour == tides$Hour[i] &
           mins == tides$Mins[i]) %>% 
    mutate(tide_pred = tides$Tide[i]) %>% 
    add_row(tide_marks)
}

# Get the respective high and low tides and store as separate tibbles
high_tides <- tide_marks %>% filter(tide_pred == "high_tide")
low_tides <- tide_marks %>% filter(tide_pred == "low_tide")

# 8. Generate scatterplot of all ACI across entire period
# Function for generating all data in a single scatter plot of observations from selected site --
scatter_all <- function(site,
                                   dataset,
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
           site.name %in% site,
           index == index.type) %>% 
    ggplot(aes(x=time, y=value)) +
    # mean value for the period of observation
    geom_point(aes(colour=period), size = 2, alpha = 0.7) +
    scale_color_brewer(palette = "Dark2") +
    geom_hline(data = dataset %>% filter(site.name %in% site), aes(yintercept=mean), colour = "#0073ff", alpha = 0.5, linetype="solid", size = 1) +
    geom_label_repel(data = dataset %>% filter(site.name %in% site), aes(family = lab.font, label=paste("Mean ACI: ", mean), x=60000, y=mean),colour = lab1.text,fill = lab1.fill,nudge_x = 0,nudge_y = 400,segment.linetype = 1,segment.curvature = -0.1,segment.ncp = 3,segment.angle = 0,alpha = lab.alpha, arrow = arrow(length = unit(0.02, "npc"))) +
    geom_smooth(colour="#ffd445", alpha = 0.6) +
   geom_point(data = filter(high_tides, site.name == site, index == index.type), aes(x=time, y=value), colour = h.tide, shape=24, size=7, stroke=1, alpha=0.8) +
   geom_point(data = filter(low_tides, site.name == site, index == index.type), aes(x=time, y=value), colour = l.tide, shape=25, size=7, stroke=1, alpha=0.8) +
    theme_wita() +
    theme(axis.title.x=element_blank(),
          legend.title = element_blank(),
          legend.position = "right") +
    ylim(c(y.min, y.max)) +
    labs( 
      subtitle = paste(index.name, "(2022-12-23 to 2022-12-31)"), 
      y = x.label)
}

# Generate the scatter plot 
# All ACI observations from 23rd to 31st of Dec 2022
scatter_all <- 
  scatter_all(c("Seagrass meadow", "Reef fringe"),
                         wita15_overall.stats,
                         "acoustic_complexity",
                         1650, 4100) +
  facet_wrap(~site.name, nrow=2)

scatter_all 
ggsave("exports-WITA15/2.WITA_15-ACI_scatter_all_wtides_coloured.png", width = 36, height = 24, units = "cm")

# 9. Generate a boxplot of all ACI distributions across the entire period 
boxplot_all <- function(site,
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
           site.name %in% site,
           index == index.type) %>% 
    ggplot(aes(x=period, y=value)) +
    # mean value for the period of observation
    geom_boxplot(aes(colour=period), size = 0.5) +
    scale_color_brewer(palette = "Dark2") +
    theme_wita() +
    theme(axis.title.x=element_blank(),
          panel.background = element_rect(fill = "#e4e6e1", colour = "#e4e6e1"),
          panel.grid.major = element_line(colour = "#b5b8b0", size = 0.5),
          panel.grid.minor = element_line(colour = "#e8ebe6", size = 0.5), 
          legend.title = element_blank(),
          legend.position = "none") +
    ylim(c(y.min, y.max)) +
    labs( 
      subtitle = paste(index.name, "(2022-12-23 to 2022-12-31)"), 
      y = x.label)
}

boxplot_all <- 
  boxplot_all(c("Seagrass meadow", "Reef fringe"),
                         "acoustic_complexity",
                         1650, 4100) +
  facet_wrap(~site.name, nrow=1)

boxplot_all 
ggsave("exports-WITA15/3.WITA_15-ACI_boxplot_all_coloured.png", width = 36, height = 24, units = "cm")

# 10. Each of the site with tide markers
scatter_all_meadow <- 
  scatter_all(c("Seagrass meadow"),
              wita15_overall.stats,
              "acoustic_complexity",
              1650, 4100)

scatter_all_meadow
ggsave("exports-WITA15/4.WITA_15-ACI_scatter_all_meadow.png", width = 36, height = 24, units = "cm")

scatter_all_fringe <- 
  scatter_all(c("Reef fringe"),
              wita15_overall.stats,
              "acoustic_complexity",
              1650, 4100)

scatter_all_fringe
ggsave("exports-WITA15/4.WITA_15-ACI_scatter_all_fringe.png", width = 36, height = 24, units = "cm")

# 11. Mean daily ACI with 29/12/2022 highlighted
wita15daily_ACI_29highlighted <- 
wita15daily_ACI +
  geom_point(data = wita15_daily.stats %>%  filter(date == "2022-12-29", site.name == "Reef fringe", index == "acoustic_complexity"),
             aes(x=date, y=mean), size = 7, colour = "#f7550f", shape = 19, stroke = 1, alpha = 0.5) +
  geom_label_repel(data = wita15_daily.stats %>% filter(date == "2022-12-29", site.name == "Reef fringe", index == "acoustic_complexity"), aes(family = lab.font, label=paste("Reef fringe, mean ACI: ", mean), x=date, y=mean),colour = lab1.text,fill = lab1.fill,nudge_x = 0,nudge_y = -100,segment.linetype = 1,segment.curvature = -0.1,segment.ncp = 3,segment.angle = 0,alpha = lab.alpha, arrow = arrow(length = unit(0.02, "npc"))) +
  geom_point(data = wita15_daily.stats %>%  filter(date == "2022-12-29", site.name == "Seagrass meadow", index == "acoustic_complexity"),
             aes(x=date, y=mean), size = 7, colour = "#42f5f5", shape = 19, stroke = 1, alpha = 0.5) +
  geom_label_repel(data = wita15_daily.stats %>% filter(date == "2022-12-29", site.name == "Seagrass meadow", index == "acoustic_complexity"), aes(family = lab.font, label=paste("Seagrass meadow, mean ACI: ", mean), x=date, y=mean),colour = lab1.text,fill = lab1.fill,nudge_x = 0,nudge_y = 100,segment.linetype = 1,segment.curvature = -0.1,segment.ncp = 3,segment.angle = 0,alpha = lab.alpha, arrow = arrow(length = unit(0.02, "npc"))) 

wita15daily_ACI_29highlighted
ggsave("exports-WITA15/5.WITA_15-ACI_both_daily_site_avg_29_highlighted.png", width = 24, height = 12, units = "cm")

# 12. Annotated scatter plot of 29/12/2022 with annotations
# Function for generating single scatter plot of observations from selected site and date range --
scatter_single <- function(site, 
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
           site.name %in% site,
           index == index.type) %>% 
    ggplot(aes(x=time, y=value)) +
    geom_point(aes(colour=period), size = 2, alpha = 0.7) +
    scale_color_brewer(palette = "Dark2") +
#    geom_hline(data = wita15_daily.stats %>% filter(date == date.range), aes(yintercept=mean), colour = "#0073ff", alpha = 0.5, linetype="solid", size = 1) +
#   geom_label_repel(data = wita15_daily.stats %>% filter(date == date.range, site.name %in% site), aes(family = lab.font, label=paste("Mean ACI: ", mean), x=60000, y=mean),colour = lab1.text,fill = lab1.fill,nudge_x = 0,nudge_y = 400,segment.linetype = 1,segment.curvature = -0.1,segment.ncp = 3,segment.angle = 0,alpha = lab.alpha, arrow = arrow(length = unit(0.02, "npc"))) +
    geom_smooth(colour="#ffd445", alpha = 0.6) +
    geom_point(data = filter(high_tides, date == date.range, site.name %in% site, index == index.type), aes(x=time, y=value), colour = h.tide, shape=24, size=7, stroke=1, alpha=0.8) +
    geom_point(data = filter(low_tides, date == date.range, site.name %in% site, index == index.type), aes(x=time, y=value), colour = l.tide, shape=25, size=7, stroke=1, alpha=0.8) +
    theme_wita() +
    theme(axis.title.x=element_blank(),
          legend.title = element_blank(),
          legend.position = "right") +
    ylim(c(y.min, y.max)) +
    labs( 
      subtitle = paste(" (",date.range, ")", sep=""), 
      y = x.label)
}

# Generate a plot of both sites for 29/12/2022:

# Define x-axis time params for the rectangle annotation:
rect_data <- data %>% 
  filter(date == "2022-12-29",
         index == "acoustic_complexity")

get_seconds <- function(h, m){
  seconds <- (h * 3600) + (m * 60)
  return(seconds)
}

time_min_early <- get_seconds(00, 0)
time_max_early <- get_seconds(06, 0)

time_min_middle <- get_seconds(10, 0)
time_max_middle <- get_seconds(16, 0)

time_min_late <- get_seconds(18, 0)
time_max_late <- get_seconds(23, 55)

scatter_20221229 <- scatter_single(c("Seagrass meadow", "Reef fringe"),
                                       "2022-12-29",
                                       "acoustic_complexity",
                                       1650, 3600) +
  facet_wrap(~site.name, ncol=2)

scatter_20221229_early <- 
  scatter_20221229 +
  geom_rect(aes(xmin = time_min_early,
                xmax = time_max_early,
                ymin = 1650,
                ymax = 3600),
            fill = NA,
            colour = "#ff0000",
            size = 0.5) 

scatter_20221229_early
ggsave("exports-WITA15/6a.WITA_15-ACI_scatter_20221229_early.png", width = 36, height = 24, units = "cm")

scatter_20221229_middle <- 
scatter_20221229 +
  geom_rect(aes(xmin = time_min_middle,
                xmax = time_max_middle,
                ymin = 1650,
                ymax = 3600),
            fill = NA,
            colour = "#ff0000",
            size = 0.5) 

scatter_20221229_middle
ggsave("exports-WITA15/6b.WITA_15-ACI_scatter_20221229_middle.png", width = 36, height = 24, units = "cm")

scatter_20221229_late <- 
  scatter_20221229 +
  geom_rect(aes(xmin = time_min_late,
                xmax = time_max_late,
                ymin = 1650,
                ymax = 3600),
            fill = NA,
            colour = "#ff0000",
            size = 0.5) 

scatter_20221229_late
ggsave("exports-WITA15/6c.WITA_15-ACI_scatter_20221229_late.png", width = 36, height = 24, units = "cm")



##########################################################################################


# single point highlighted --
scatter_viz_single("Reef fringe","2022-12-24","acoustic_complexity",1650, 3400) + 
  geom_point(data = data %>%  filter(day == 24, hour == 14, mins == 00, site.name == "Reef fringe", index == "acoustic_complexity"),
             aes(x=time, y=value), size = 7, colour = "red", shape = 19, stroke = 1, alpha = 0.5)

ggsave("exports-WITA14/WITA_14-ACI_scatter-anno_20221224-1500.png", width = 36, height = 24, units = "cm")




# patch the plots together ---

all_singles <- 
  scatter_20221224 | scatter_20221225 | scatter_20221226 | scatter_20221227 |
  scatter_20221228 | scatter_20221229 | scatter_20221230

ggsave("exports-WITA14/WITA_14-ACI_scatter_all.png", width = 64, height = 12, units = "cm")

# Extra-annotated version of the plot for 24th of December --
# For comparison with time-compressed annotated spectrogram --

scatter_20221224_anno <- 
  scatter_viz_single("Reef extent 1","2022-12-24","acoustic_complexity",1650, 3400) +
  geom_vline(data = data %>% filter(date == "2022-12-24",hour == 2, mins == 0), aes(xintercept = time), colour = "#6d9e09", alpha = 0.6) +
  geom_label(data = data %>% filter(date == "2022-12-24", hour == 2, mins == 0), aes(family = lab.font, label="2:00", x=time, y=3000),colour = lab1.text,fill = lab1.fill,nudge_x = 0,nudge_y = 400) +
  geom_vline(data = data %>% filter(date == "2022-12-24",hour == 9, mins == 0), aes(xintercept = time), colour = "#6d9e09", alpha = 0.6) +
  geom_label(data = data %>% filter(date == "2022-12-24", hour == 9, mins == 0), aes(family = lab.font, label="9:00", x=time, y=3000),colour = lab1.text,fill = lab1.fill,nudge_x = 0,nudge_y = 400) +
  geom_vline(data = data %>% filter(date == "2022-12-24",hour == 15, mins == 40), aes(xintercept = time), colour = "#6d9e09", alpha = 0.6) +
  geom_label(data = data %>% filter(date == "2022-12-24", hour == 15, mins == 40), aes(family = lab.font, label="15:40", x=time, y=3000),colour = lab1.text,fill = lab1.fill,nudge_x = 0,nudge_y = 400) +
  geom_vline(data = data %>% filter(date == "2022-12-24",hour == 19, mins == 0), aes(xintercept = time), colour = "#6d9e09", alpha = 0.6) +
  geom_label(data = data %>% filter(date == "2022-12-24", hour == 19, mins == 0), aes(family = lab.font, label="19:00", x=time, y=3000),colour = lab1.text,fill = lab1.fill,nudge_x = 0,nudge_y = 400) 

ggsave("exports-WITA14/WITA_14-ACI_scatter-xtra_anno_20221224.png", width = 36, height = 24, units = "cm")

# Patch everything together into one big infographic --
title <- toupper("Wrangling In The Antipodes: Lady Bay - AudioMoth Deployment (Far reef extent)")
subtitles <- str_wrap("This data analysis examines observations made with an AudioMoth deployed 
                      at a far extent of Lady Bay Reef. The data has been  pre-processed 
                      as a measure of acoustic complexity (ACI) and plotted against 24-hour time 
                      to gain an insight into the trends of ACI over daily periods of the deployment.", 160)
caption <- "Wrangling In The Antipodes: 'Part 2 - reef extent location and comparative analysis'\nTristan Louth-Robins, 2023. Github: /TristanLouthRobins"

font_add("Antonio", "/Users/tristanlouth-robins/Library/Fonts/Antonio-VariableFont_wght.ttf")
font_add("Hiragino Sans W0", "/Users/tristanlouth-robins//Library/Fonts/ヒラギノ角ゴシック W0.ttc")

base <- ggplot() +
  labs(title = title,
       subtitle = subtitles,
       caption = caption) +
  theme_wita() +
  theme(plot.title = element_text(family = "Antonio", size = 36, colour = "#000000", margin=margin(0,0,10,0)),
        plot.subtitle = element_text(family = "Hiragino Sans W0", size = 24, colour = "#000000", margin=margin(0,0,10,0)),
        plot.caption = element_text(family = "Hiragino Sans W0", size = 18, colour = "#000000"),
        plot.margin = margin(1,1,1,1, "cm"),
        plot.background = element_rect(fill = plot.bg, colour = plot.bg))

# Let's make a histogram to show the distribution of the ACI values --

meadow_data <- data 

meadow_data <- meadow_data %>% 
  filter(index == "acoustic_complexity", site.name == "Reef extent 1") 

histo <- 
  ggplot(meadow_data) +
  geom_histogram(aes(x = value, fill = period), bins = sqrt(nrow(meadow_data))) +
  scale_fill_brewer(palette = "Dark2") +
  facet_wrap(~period, nrow = 1) +
  labs(x="", y="",title = "Acoustic complexity distributions over daily periods") +
  theme_wita() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

# Patch and inset everything together --

ptwk <- 
  base +
  inset_element(scatter_all, left = 0, right = 0.37, bottom = 0.50, top = 0.95) +
  inset_element(histo, left = 0.40, right = 0.99, bottom = 0.50, top = 0.95) +
  inset_element(all_singles, left = 0, right = 0.99, bottom = 0.05, top = 0.45) 
ptwk

ggsave("exports-WITA14/WITA_14-patchwork_project.png", width = 72, height = 48, units = "cm")


