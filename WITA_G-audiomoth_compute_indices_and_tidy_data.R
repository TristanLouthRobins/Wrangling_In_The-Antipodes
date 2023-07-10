# Script for computing acoustic indices using the soundecology package.
# Tristan Louth-Robins. 2021-23

# Version 2.8 (9th July 2023): 
# 2.0 - all three steps now in single script.
# 2.5 - fixed the error in factorisation turning all month variables into a 'Summer' category.
# 2.6 - cleaned up file import code, cleaner and more efficient.
# 2.7 - new function for user input of site variable.
# 2.8 - feature to create directory/folder for outputted results

# STEP 1: COMPUTE ACOUSTIC INDICES ---------------------------------------------
library(soundecology)
library(tidyverse)

defaultwd <- "/Users/tristanlouth-robins/data_science/acoustic_ecology/"
setwd(defaultwd)

data_src <- "raw_data" # Sub folder containing raw acoustic data:
data_import <- paste(defaultwd, data_src, sep = "")

# Function: Setup for analysis output folder -----------------------------------
folder_setup <- function() {
    cat("To create new folder")
    folder <- readline(prompt = "Name destination folder for results output: ")
    resultswd <- paste(defaultwd, "results/", folder, sep = "")
    if (file.exists(resultswd)) {
      cat("The folder already exists!")
    } else {
      cat("Created new folder for results: ", folder)
      dir.create(resultswd)
      print(resultswd)
      return(resultswd)
    }
}

# Function: compute defined index from imported data batch ---------------------
compute_indices <- function(index, batch, note){
  # build string for output file name: 
  e.mothid <- readline(prompt = "enter moth id: ")
  mothid <- paste("MOTH", e.mothid, sep = "")
  date <- readline(prompt = "enter data range (as dd-mm-yy format): ")
  site.name <- readline(prompt = "enter site name: ")
  site <- paste(mothid, date, site.name, sep = "_")
  
  csv <- paste(site, batch, index, note, sep="_")
  file <- paste(csv, ".csv", sep="")
  dest <- paste(resultswd, file, sep="/")
  
  multiple_sounds(directory = data_import, 
                  resultfile = dest,
                  soundindex = index,
                  no_cores = "max")
}

# Create a folder for the analysis output -----
resultswd <- folder_setup()

# Define abbreviate var names for acoustic indices names -----------------------
bi <-  "bioacoustic_index"
aci <-  "acoustic_complexity"
adi <-  "acoustic_diversity"
aei <- "acoustic_evenness"
ndsi <-  "ndsi"

# Perform the processing of the data using the chosen acoustic index -----------
compute_indices(adi,          # <-- index
                0,            # <-- batch number
                "")           # <-- misc. note

# STAGE 2: READ IN RESULTS AND TIDY ----------------------------------------------------------------------------------
# Required libraries  -
library(forcats)
library(stringr)
library(lubridate)
library(chron)

# Include site variable for dataset -------------------------------------------------------------------
setwd(resultswd)

file <- list.files(pattern="*.csv") 

site_input <- function(file){
  site_name <- readline(prompt="Enter site name: ")
  for (i in file) {
    print(i)
    data <- read_csv(i) %>% 
    mutate(site = site_name)
    print(data)
    print("Site name assigned and .csv file updated.")
    write_csv(data, i) 
  }
}

site_input(file)

# Function for tidying the imported results -------------------
tidy_data <- function(input) {
  output <- input %>% 
    separate(FILENAME, into = c("date", "time", sep = "_")) %>% 
    mutate(date = gsub("^(.{4})(.*)$", "\\1-\\2", date), date = gsub("^(.{7})(.*)$", "\\1-\\2", date)) %>%
    mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
    mutate(time = gsub("^(.{2})(.*)$", "\\1:\\2", time), time = gsub("^(.{5})(.*)$", "\\1:\\2", time)) %>% 
    mutate(time = chron(times = time)) %>% # coerce chr time string into time data format.
    mutate(mins = format(strptime(time,"%H:%M:%S"), '%M')) %>% 
    mutate(hour = format(strptime(time,"%H:%M:%S"), '%H')) %>% 
    mutate(mins = as.numeric(mins), hour = as.numeric(hour)) %>% 
    select(date, time, INDEX, mins, hour, LEFT_CHANNEL, site) %>% 
    rename(value = LEFT_CHANNEL, index = INDEX, site.name = site) %>% 
    group_by(date) 
  
  output <- output %>% 
    mutate(date = as.character.Date(date)) %>% 
    mutate(date.data = date) %>% 
    separate(date.data, into = c("year", "month", "day", sep = "-")) %>% 
    select(- `-`) 
  
  output <- output[,c("date", "time", "year", "month", "day", "mins", "hour", "index", "value", "site.name")]  
  
  return(output)
}

# Function for creating categorical variables ----------------------
cat_data <- function(tidy_df) {
  cat <- tidy_df %>% 
    # Period of day, defined by range of 'hour' variable. 
    mutate(period = ifelse(hour %in% 0:4, "pre-dawn", 
                           ifelse(hour %in% 5:7, "dawn", 
                                  ifelse(hour %in% 8:11, "morning",
                                         ifelse(hour %in% 12:13, "midday",
                                                ifelse(hour %in% 14:17, "afternoon",
                                                       ifelse(hour %in% 18:19, "dusk", 
                                                              ifelse(hour %in% 20: 23, "night","X"))))))) 
    ) %>% 
    mutate(month = as.numeric(month),
           period = as.factor(period)) %>% 
    # Season, defined by range of 'month' variable.
    mutate(season = ifelse(month %in% c(1,2,12), "Summer",
                           ifelse(month %in% c(3,4,5), "Autumn",
                                  ifelse(month %in% c(6,7,8), "Winter",
                                         ifelse(month %in% c(9,10,11), "Spring","X"))))
    ) %>% 
    mutate(season = as.factor(season))
  
  # re-order factors 
  cat$period <- ordered(cat$period, levels = c("pre-dawn", "dawn", "morning", "midday", "afternoon", "dusk", "night"))
  cat$season <- ordered(cat$season, levels = c("Summer", "Autumn", "Winter", "Spring"))
  
  # re-coerce date variable as date data type:
  cat <- cat %>% 
    mutate(date = as.Date(date, format = "%Y-%m-%d")) %>% 
    # coerce split (individual) date variable as factors
    mutate(year = as.factor(year),
           month = as.factor(month),
           day = as.factor(day)) 
  
  return(cat) 
}  

# Import tabular data for tidying ----------------------------------------------------
import_path <- resultswd
setwd(import_path)

# For single datasets, assign the 't.s' variable.
# Enter the file you want to import below:
# file <- "moth003-23_to_31-12-22-lady_bay_meadow_cove_0_acoustic_evenness_.csv"
# t.s <- read_csv(file)

# For multiple batched datasets, assign the 't' variable below:
# The below will pull all csv files stored in the designated sub_folder into a single dataframe.

t <- list.files(pattern="*.csv") %>% 
  map_df(~read_csv(.))

tidy_t <- tidy_data(t)
complete_t <- cat_data(tidy_t)
head(complete_t)

# STAGE 3: WRITE TIDY DATA TO NEW CSV -------------------------------------------------
# For single datasets:
tidy_file <- file %>% str_remove("_.csv") %>% paste("-tidy", ".csv", sep="")

# for merged datasets:
merged_data <- "lady_bay_reef_december_ACI_ADI_AEI.csv"

write_csv(complete_t, tidy_file)
