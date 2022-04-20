## make_labels() function ~ LABEL AUDIO DATA FOR ANALYSIS
## Tristan Louth-Robins, 2021.
## See accompanying blog post here: https://wranglingintheantipodes.wordpress.com/2021/10/20/preparing-big-audio-files-for-analysis/

library(tidyverse)

# Function: Make labels of specified number (n) with given parameters
make_labels <- function(start, end, n, timecode, date, format){
  
  labels <- tibble(start, end, timecode)
  n <- n - 1
  
  for(i in 1:n) {
    start <- end
    end <- start + 60
    timecode <- timecode + 1
    
    labels <- labels %>% 
      add_row(start = start,
              end = end,
              timecode = timecode)
  }
  
  labels <- labels %>% 
    mutate(label_name = paste(date, timecode, format, sep = "")) %>% 
    select(-timecode)
  
  return(labels)
}

# Call make_labels() with specified parameters
labels <- make_labels(0, 60, 60, 60000, "20210630_", ".WAV") 

labels

# Write tibble to tab (enter your directory path)
# Check directory path: getwd()
write_tsv(labels, "[your output path goes here.]",
          col_names = FALSE)
