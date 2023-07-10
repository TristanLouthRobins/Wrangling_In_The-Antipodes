library(ggplot2)
setwd("/Users/tristanlouth-robins/Documents/Documents - MacBook Pro/R data and projects/acoustic_ecology_tests/Wrangling_In_The-Antipodes")

data <- read_csv("data/mf_beach_and_sheoak.csv")

bi_plt <- 
data %>% 
  filter(index == "bioacoustic_index",
         date != "2023-07-01" & date != "2023-07-08") %>% 
  ggplot() +
  geom_point(aes(x=time, y=value, colour=site.name), alpha=0.7) +
  labs(title = "Bioacoustic index",
       y="Value",
       x="Time") +
  facet_wrap(~date, nrow=1)

aci_plt <- 
  data %>% 
  filter(index == "acoustic_complexity",
         date != "2023-07-01" & date != "2023-07-08") %>% 
  ggplot() +
  geom_point(aes(x=time, y=value, colour=site.name), alpha=0.7, shape = 3) +
  labs(title = "Acoustic Complexity index",
       y="Value",
       x="Time") +
  facet_wrap(~date, nrow=1)

aci_plt

adi_plt <- 
  data %>% 
  filter(index == "acoustic_diversity",
         date != "2023-07-01" & date != "2023-07-08") %>% 
  ggplot() +
  geom_point(aes(x=time, y=value, colour=site.name), alpha=0.7) +
  labs(title = "Acoustic Diversity index",
       y="Value",
       x="Time") +
  facet_wrap(~date, nrow=1)


