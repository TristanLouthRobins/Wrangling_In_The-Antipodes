library(tidyverse)

creek_data <- read_csv('data/mf_beach_and_sheoak.csv')
creek_data_20db <- read_csv('data/mf_beach-ADI-20dbthresh.csv')

ad_creek <- 
creek_data %>% 
  filter(index == 'acoustic_diversity', site.name == 'mf_beach')

ad_creek_2 <- 
  creek_data_20db %>% 
  filter(index == 'acoustic_diversity', site.name == 'mf_beach')

ad_sheoak <- 
  creek_data %>% 
  filter(index == 'acoustic_diversity', site.name == 'mf_sheoak')

library(ggplot2)

# VISUALISATIONS
viz_scatter <- function(data) {
  data %>% 
    ggplot() +
    geom_point(aes(x = time, y = value)) +
    facet_wrap(~date)
}

vis_dist <- function(data) {
  data %>% 
    ad_creek %>% 
    ggplot() +
    geom_histogram(aes(x = value))
}

viz_scatter(ad_creek)
viz_scatter(ad_creek_2)
viz_scatter(ad_sheoak)        


# Visualise the frequency bins of given audio
library(seewave)
library(tuneR)

mf_beach20230708_050000 <- readWave("data-audio/WITA_sup1/mf_beach-AD_0.751-20230708_050000.WAV")
mf_beach20230708_073000 <- readWave("data-audio/WITA_sup1/mf_beach-AD_2.30-20230708_073000.WAV")
mf_beach20230702_155000 <- readWave("data-audio/WITA_sup1/mf_beach-AD_0.00_20db_-20230702_155000.WAV")

spec_050000 <- meanspec(mf_beach20230708_050000, f=24000, plot=T)
fbands(spec_050000, bands=10)

?fbands

spec_073000 <- meanspec(mf_beach20230708_073000, f=24000, plot=T)
fbands(spec_073000, bands=10)


library(soundecology)
# set threshold
acoustic_diversity(mf_beach20230708_073000, max_freq = 10000, db_threshold = -20, 
                   freq_step = 50, shannon = TRUE)

# mf_beach20230708_050000
# threshold = -50 = 0.750
# threshold = -40 = 0.185
# threshold = -30 = 0.117


# mf_beach20230708_073000
# threshold = -50 = 2.3024
# threshold = -40 = 2.28
# threshold = -30 = 1.855


