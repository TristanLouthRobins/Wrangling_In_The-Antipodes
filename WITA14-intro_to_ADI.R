# ~ WRANGLING IN THE ANTIPODES ~            
# 14: Introduction to the Acoustic Diversity Index (ADI)
# Tristan Louth-Robins, March 2024
# Blog post can found here: https://wranglingintheantipodes.wordpress.com/

library(tidyverse)
library(soundecology)
library(seewave)
library(tuneR)

# create functions to perform spectral analysis and compute ADI --
# 1. spectral_viz: spectral analysis of given file
spectral_viz <- function(file, range) {
  meanspec(file, f=range, plot=T)
}

# 2. discrete: discretise spectral analysis into frequency bins --
discrete <- function(spectral_analysis) {
  fbands(spectral_analysis, bands=10)
}

# 3. compute the ADI for given soundfile -- 
compute_adi <- function(file) {
  acoustic_diversity(file, max_freq = 10000, db_threshold = -20, 
                     freq_step = 50, shannon = TRUE)
}

# create the spectral analysis, bins and perform ADI --
dawn <- readWave("data-audio/WITA-14/audio-dawn.WAV")

spec.dw <- spectral_viz(dawn, 16000)
discrete(spec.dw)
compute_adi(dawn) # Acoustic Diversity Index: 3.670878








