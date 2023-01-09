library(tidyverse)
library(ggplot2)
library(patchwork)

data <- read_csv("data/soundprint_stats_220109.csv")

# factorise variables ------------------------------
data <- data %>% 
  mutate(type = as.factor(type),
         area = as.factor(area),
         suburb = as.factor(suburb),) %>% 
  mutate(category = ifelse(rating <= 70, "Quiet",
                           ifelse(rating > 70 & rating < 76, "Moderate",
                                  ifelse(rating >= 76 & rating < 81, "Loud",
                                         ifelse(rating >= 81 & rating < 90, "Very Loud", 
                                                ifelse(rating > 90, "Nope", "x")))))) %>%
  mutate(category = as.factor(category)) %>% 
  na.omit()

data$category <- ordered(data$category, levels = c("Quiet", "Moderate", "Loud", "Very Loud", "Nope"))


library(gt)

(boxplot_stats <- 
  data %>% 
  group_by(type) %>% 
  summarise(count = n(), 
            mean_db = round(mean(rating), 1),
            median_db = median(rating),
            sd_db = round(sd(rating), 1),
            min_db = min(rating),
            max_db = max(rating)) %>% 
  ungroup() %>% 
    gt())

(venue_stats <- 
  data %>% 
  select(venue, rating, category) %>% 
  arrange(desc(rating)) %>%
    ungroup() %>% 
    gt())

# simple theme template for visualisations ----------
theme_sndprint <- function(){
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

# box plot of Adelaide CBD venue ratings by type ----
box <- 
ggplot(data, aes(x = type, y = rating)) +
  geom_boxplot(colour ="#a3e67e",
               fill = "#0c1c03",
               outlier.shape = 1,
               outlier.stroke = 2,
               outlier.colour = "#a3e67e") +
  labs(title = "SoundPrint dB Rating by venue type",
       subtitle = "Adelaide CBD venues, 31 observations",
       x = "Venue Type", y = "dB rating") +
  theme_sndprint()

# distribution of Adelaide CBD venues ---------------
dist <- 
ggplot(data, aes(x = rating)) +
  geom_histogram(bins = sqrt(nrow(data)), fill = "#a3e67e") +
  labs(title = "SoundPrint dB Rating Distribution",
       subtitle = "Adelaide CBD venues, 31 observations",
       x = "dB rating", y = "Count") +
  theme_sndprint()

# stacked bar plot of Adelaide CBD venue types by loudness categories ----
data_grouped <- 
data %>% 
  group_by(type, category) %>% 
  summarise(n = n())

bar <-
ggplot(data_grouped, aes(fill = category, y = n, x = type)) +
  geom_bar(position = "dodge", stat = "identity", colour = "#000000") +
  scale_fill_manual(values = c("#81f542", "#cbf542", "#f5e942", "#f5a442", "#f54242"),
                    name = "Loudness categories",
                    breaks = c("Quiet", "Moderate", "Loud", "Very Loud", "Nope"),
                    labels = c("Quiet: < 70dB", 
                                "Moderate: 71-75dB",
                                "Loud: 76-80dB",
                                "Very Loud: 81-90dB",
                                "Nope/Earplugs!: > 90dB"),
                     ) +
  labs(title = "SoundPrint Loudness Categories by venue type",
       subtitle = "Adelaide CBD venues, 31 observations",
       x = "Venue Type", y = "Count") +
  theme_sndprint()

(box | dist) / bar 

