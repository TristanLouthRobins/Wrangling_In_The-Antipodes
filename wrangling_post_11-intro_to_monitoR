###########################################
# ~WRANGLING IN THE ANTIPODES~            #
# 11: Intro to monitoR                    #
# Tristan Louth-Robins, August 2022       #
########################################################################################
# Post can found here:                                                                 #
# https://wranglingintheantipodes.wordpress.com/2022/04/09/ndsi-parameters-in-context/ #
########################################################################################

library(monitoR)
library(tidyverse)

setwd("/acoustic_ecology_tests/MonitoR_tests/reed_warbler")

# read in audio clips of Australian Reed Warbler song repertoire:
warbler1 <- readWave("warbler-1.wav")
warbler2 <- readWave("warbler-2.wav")
warbler3 <- readWave("warbler-3.wav")

# read in audio clip of the bonus pigeon:
pigeon <-  readWave("bonus_pidgeon.wav")

# Examine the spectrogram of the .wav files
viewSpec(pigeon)

# read in audio for testing correlation:
eval <-  readWave("test_audio.wav")
eval2 <-  readWave("test_audio2.wav")
long <- readWave("really_long_test.wav")

# make correlation templates for the Reed Warbler:
warbler1.cor <- makeCorTemplate("warbler-1.wav", 
                                write.wav = FALSE, 
                                t.lim = c(0.1, 2.0), # can be NA
                                score.cutoff = 0.5, # set threshold
                                frq.lim = c(2, 6),
                                name = "w1") # freq lim of spectrogram plot

warbler2.cor <- makeCorTemplate("warbler-2.wav", 
                                write.wav = FALSE, 
                                t.lim = c(0.1, 1.0), # can be NA
                                score.cutoff = 0.5, # set threshold
                                frq.lim = c(2, 6),
                                name = "w2") # freq lim of spectrogram plot

warbler3.cor <- makeCorTemplate("warbler-3.wav", 
                                write.wav = FALSE, 
                                t.lim = c(0.1, 1.0), # can be NA
                                score.cutoff = 0.5, # set threshold
                                frq.lim = c(2, 6),
                                name = "w3") # freq lim of spectrogram plot

# make correlation templates for the Bonus Pidgeon:

pidgeon.cor <- makeCorTemplate("bonus_pidgeon.wav", 
                                write.wav = FALSE, 
                                t.lim = c(0.1, 1.0), # can be NA
                                score.cutoff = 0.5, # set threshold
                                frq.lim = c(0.3, 1),
                                name = "p") # freq lim of spectrogram plot

# combine the correlation templates for analysis:
ctemps <- combineCorTemplates(warbler1.cor,
                              warbler2.cor,
                              warbler3.cor,
                              pigeon.cor)

# view all these templates
plot(ctemps)

# compute scores
cscores <- corMatch("really_long_test.wav", ctemps)

# 
cdetects <- findPeaks(cscores)

# extract the detections
detections <- getDetections(cdetects)

# convert the detections to a dataframe and save a .csv version for reference
peaks_test <- data.frame(detections)
write_csv(peaks_test, "results/reallylongtest_peaks.csv")

# tidying up the dataframe for ggplot visualisation
detections <- detections %>% mutate(species = ifelse(template == "p", "pigeon", "reed warbler"))
detections$template <- factor(detections$template, levels=c("w1", "w2", "w3", "p"))
levels(detections$template) <- c("Reed Warbler A",
                                 "Reed Warbler B",
                                 "Reed Warbler C",
                                 "Pigeon")

# create summary statistics
summary_stats <- detections %>% 
  group_by(template) %>% 
  summarise(n = n(),
            min = min(score),
            max = max(score),
            mean = mean(score),
            sd = sd(score),
            median = median(score))

library(RColorBrewer) 
library(glue)

# variables for the ggplot visualisation
font.plot <- "Tahoma"
font.subtitle <- "Geneva"
font.axis <- "Arial"
font.legend <- "Arial"
font.labels <- "Arial"

indigo <- "#3b374a"

text <- glue("Test data a 30-minute audio file, an excerpt derived from a 2-hour recording made at Middle Farm, adjacent to \nCarrickalinga Creek in September 2021. \n
Correlation templates for Reed Warbler and Common pigeon (score >= 0.5) resulted in the following detections: \n
Reed Warbler: call A = {summary_stats$n[[1]]} detections; call B = {summary_stats$n[[2]]} detections; call C = {summary_stats$n[[3]]} detections\nCommon pigeon: 'cooing' call = {summary_stats$n[[4]]} detections")

p <- ggplot(detections) +
  geom_point(aes(x=time, y=score, shape=species, colour=template, stroke=1), alpha=0.7, size=2) +
  geom_hline(yintercept=0.5, colour = "yellow", alpha=0.6) +
  xlab("Duration (seconds)") +
  ylab("Correlation score") +
  theme(
    plot.background = element_rect(fill = "#f5cd67", colour = "#000000", size = 1),
    panel.background = element_rect(fill = "black"),
    panel.grid = element_blank(),
    plot.margin = margin(0.8,6,1.1,1, "cm"),
    legend.position = c(1.4, 1),
    legend.justification = c(1, 1),
    legend.background =  element_rect(fill = "white", colour = "black"),
    legend.title = element_text(
      family = font.legend,
      colour = "black",
      face = "bold",
      size = 11),
    legend.text = element_text(
      family = font.labels,
      size = 9
    ),
    axis.title.x = element_text(family=font.axis, size=10, face="bold"),
    axis.title.y = element_text(family=font.axis, size=10, face="bold"),
    axis.text.x = element_text(family=font.axis, size=8),
    axis.text.y = element_text(family=font.axis, size=8),
    plot.title = element_text(
      size = 12, lineheight = 0.9,
      family = font.plot, face = "bold", colour = indigo
    ),
    plot.subtitle = element_text(
      size = 10, family = font.subtitle, colour = indigo
    ),
    plot.caption = element_text(
      size = 8, family = font.axis, colour = indigo
    )) +
  ylim(0.0,1.0) +
  labs(title = "monitoR cross-correlation testing",
          subtitle = text,
          caption = "Wrangling In The Antipodes: 'Acoustic detection in monitoR - an overview'\nTristan Louth-Robins, 2022. Github: /TristanLouthRobins") +
  scale_shape_discrete(limits = c("reed warbler",
                                  "pigeon"),
                       name = "Species type",
                       labels = c("Australian Reed Warbler",
                                  "Common Australian pigeon")) +
  scale_color_discrete(limits = c("Reed Warbler A", "Reed Warbler B", 
                                  "Reed Warbler C", "Pigeon"),
                       name = "Call type",
                       labels = c("Reed Warbler: call A",
                                  "Reed Warbler: call B",
                                  "Reed Warbler: call C",
                                  "Pigeon: 'cooing' call")) 

# annotating the pigeon region in the above plot
pigeon_region <- detections %>% 
  group_by(template) %>% 
  filter(template == "Pigeon") %>% 
  summarise(min = min(time),
            max = max(time))

p +
  geom_vline(xintercept=pigeon_region$min, colour = "white", linetype = "dashed", alpha=0.4) +
  geom_vline(xintercept=pigeon_region$max, colour = "white", linetype = "dashed", alpha=0.4) 

# faceted version of the above plot  
f <- p + facet_wrap(template ~., nrow=4) +
  theme(legend.position = "none",
        strip.text.x = element_text(
          size=12, face="bold"
        ),
        strip.background = element_rect(
          colour="black", fill="orange"
        ))

f

# computing proportion of scores <= 0.6
total <- tally(detections)

low <- detections %>% 
  filter(score <= 0.6) %>% 
  tally()

round(low / total, 2) # 79% of scores <= 0.6

# histogram of detection scores 
ggplot(detections, aes(score)) +
  geom_histogram(bins = 15) +
  geom_vline(xintercept = 0.6, colour = "red") +
  theme_light()

# stacked bar chart representing prop of scores in given range
score_range_df <- detections %>% 
  mutate(score_range = ifelse(score <= 0.55, "<0.55",
                              ifelse(score > 0.55 & score <= 0.60, "<0.60",
                              ifelse(score > 0.60 & score <= 0.65, "<0.65", 
                              ifelse(score > 0.65 & score <= 0.70, "<0.70", ">0.70"
         )))))

# summary statistics of score ranges
range_stat <- score_range_df %>% 
  group_by(score_range) %>% 
  summarise(n = n()) %>% 
  mutate(prop = round((n / sum(n)) * 100., 1))

# ggplot visualisation of score regions as stacked bar chart
text2 <- glue("{range_stat$prop[[1]] + range_stat$prop[[2]]}% of the detections have a correlation score of 0.60 or less")

ggplot(score_range_df, aes(fill=template, y=score, x=score_range)) +
  geom_bar(position="stack", stat="identity") +
  theme(
    plot.background = element_rect(fill = "#f5cd67", colour = "#000000", size = 1),
    panel.background = element_rect(fill = "black"),
    panel.grid = element_blank(),
    plot.margin = margin(0.8,6,1.1,1, "cm"),
    legend.position = c(1.35, 1),
    legend.justification = c(1, 1),
    legend.background =  element_rect(fill = "white", colour = "black"),
    legend.title = element_text(
      family = font.legend,
      colour = "black",
      face = "bold",
      size = 11),
    legend.text = element_text(
      family = font.labels,
      size = 9
    ),
    axis.title.x = element_text(family=font.axis, size=10, face="bold"),
    axis.title.y = element_text(family=font.axis, size=10, face="bold"),
    axis.text.x = element_text(family=font.axis, size=8),
    axis.text.y = element_text(family=font.axis, size=8),
    plot.title = element_text(
      size = 12, lineheight = 0.9,
      family = font.plot, face = "bold", colour = indigo
    ),
    plot.subtitle = element_text(
      size = 10, family = font.subtitle, colour = indigo
    ),
    plot.caption = element_text(
      size = 8, family = font.axis, colour = indigo
    )) +
  labs(title = "Template scores for correlation testing",
       subtitle = text2,
       caption = "Wrangling In The Antipodes: 'Acoustic detection in monitoR - an overview'\nTristan Louth-Robins, 2022. Github: /TristanLouthRobins") +
  xlab("Score range") +
  ylab("Count") +
  scale_fill_discrete(limits = c("Reed Warbler A", "Reed Warbler B", 
                                  "Reed Warbler C", "Pigeon"),
                       name = "Call type",
                       labels = c("Reed Warbler: call A",
                                  "Reed Warbler: call B",
                                  "Reed Warbler: call C",
                                  "Pigeon: 'cooing' call")) 
