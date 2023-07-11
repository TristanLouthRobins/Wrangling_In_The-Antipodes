library(tidyverse)
library(rlang)
library(gghighlight)
library(ggplot2)
library(ggrepel)
library(patchwork)
library(plotly)
library(showtext)
setwd("/Users/tristanlouth-robins/Documents/Documents - MacBook Pro/R data and projects/acoustic_ecology_tests/Wrangling_In_The-Antipodes")

data <- read_csv("data/mf_beach_and_sheoak.csv")

# Scatter plot aesthetics and theme --------------------------------------------
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
# theme --
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

plt_index <- function(index_type, site) {
  data %>% 
    filter(index == index_type,
           site.name == site,
           date == "2023-07-03" | date == "2023-07-04") %>% 
    ggplot() +
    geom_point(aes(x=time, y=value), colour = points, alpha=0.7) +
    geom_smooth(aes(x=time, y=value), method="loess", se=F) +
    facet_wrap(~date, nrow=1) +
    ylim(0,12)
}

plt_aci_bch <-  plt_index("bioacoustic_index", "mf_beach") + 
  labs(subtitle = "Site 1: 'beach'",
       caption = "blah blah blah") + theme_wita() 

plt_aci_she <-  plt_index("bioacoustic_index", "mf_sheoak") + 
  labs(subtitle = "Site 2: swamp",
       caption = "blah blah blah") + theme_wita() 

plt_aci_bch
plt_aci_she

plt_aci_bch / plt_aci_she

