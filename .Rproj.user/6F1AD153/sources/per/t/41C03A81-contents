# Script for computing acoustic indices using the soundecology package.
# Tristan Louth-Robins. 2021-24
# Version 3.0 - "Clean Steve"

# SECTION 1: COMPUTE ACOUSTIC INDICES ---------------------------------------------
# Required dependencies for entire process
library(soundecology)
library(tidyverse)
library(lubridate)
library(chron)

# Verify and set working directory. Alternatively, specify a different wd if you like.
defaultwd <- "/Users/tristanlouth-robins/data_science/acoustic_ecology/"
setwd(defaultwd)

# Whatever directory you specify, ensure that you have created a sub_folder called 'raw_data'.
data_src <- "raw_data" 
data_import <- paste(defaultwd, data_src, sep = "") # append 'raw_data' to your directory. 

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
  date <- readline(prompt = "enter date range (avoid using '/' characters, use dashes instead!): ")
  site.name <- readline(prompt = "enter site name: ")
  site <- paste(mothid, date, site.name, sep = "_")
  
  csv <- paste(site, batch, index, note, sep="_")
  file <- paste(csv, ".csv", sep="")
  dest <- paste(resultswd, file, sep="/")
  
  multiple_sounds(directory = data_import, 
                  resultfile = dest,
                  soundindex = index,
                  no_cores = "max"
                  )
  print(paste("A .csv file of the analysis has now been written to: '", resultswd, "' Continue to SECTION 2 and 3 to tidy and export your data!"))
}


# Create a folder for the analysis output --------------------------------------
# Once instatiated, you need to enter this into the Console below --------------
# For example, you might name the folder based on your project or a particular site.
resultswd <- folder_setup()

# Define abbreviated var names for acoustic indices names ----------------------
bi <-  "bioacoustic_index"
aci <-  "acoustic_complexity"
adi <-  "acoustic_diversity"
aei <- "acoustic_evenness"
ndsi <-  "ndsi"

# Perform the processing of the data using the chosen acoustic index -----------
compute_indices(adi,          # <-- index
                0,            # <-- batch number
                "")           # <-- misc. note

###############################################################################
# Continue processing as many analyses as you need before running eveything   #
# below to tidy and compile the analyses into a single dataset.               #
###############################################################################

################################################################################
################################################################################

# SECTION 2: READ IN RESULTS AND TIDY --------------------------------------------

# We are now going to reassign our working directory to the 'results' subfolder
# so that R can pull the .csv file the compute_indices function just created.
setwd(resultswd)

# Look for any .csv files in this directory.
file <- list.files(pattern="*.csv") 

# This function provides a user input for setting up a unique categorical variable
# for the site of the recording. When prompted, enter this in the Console.
# Examples: 'eastern_wetland', 'garden', 'city_park'
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

# Function for tidying the imported results ------------------------------------
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

# Function for creating categorical variables ----------------------------------
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

# For SINGLE datasets, assign the 'single.dt' variable below.
# If you have processed MULTIPLE datasets which you want to merge into a single
# dataset then SKIP AHEAD to SECTION 3B (line 186)

single.dt <- read_csv(file)

# SECTION 3A: WRITE TIDY DATA TO NEW CSV ---------------------------------------
# For single datasets:
tidy_file <- file %>% str_remove("_.csv") %>% paste("-tidy", ".csv", sep="")
write_csv(complete_t, tidy_file)

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

# For MULTIPLE batched datasets, assign the 'multiple.dt' variable below:
# The below will merge all .csv files stored in the designated sub_folder into a single dataframe.

multiple.dt <- list.files(pattern="*.csv") %>% 
  map_df(~read_csv(.))

tidy_dt <- tidy_data(multiple.dt)
complete_t <- cat_data(tidy_dt)
head(complete_t)

# SECTION 3B: WRITE ALL MERGED TIDY DATA TO NEW CSV ----------------------------

merged_data <- paste(complete_t$site.name[1], "-", "merged.csv", sep = "")
write_csv(complete_t, merged_data)

# SECTION 4: COMBINE ADDITIONAL SITES INTO ONE BIG DATASET ---------------------

# If you have done analyses for more than one site and you want to merge all of 
# these together, check your merged datasets in 'results' and do the following:

# Set variables for each of your datasets and read these in.
data1 <- read_csv("yourdata1.csv") # <-- update with your filename!
data2 <- read_csv("yourdata2.csv") # <-- update with your filename!

all <- full_join(data1, data2) # Include all of your data in full_join.
write_csv(all, merged_data) # <-- this will either be 'tidy_file' (single dataset) or 'merged_data' (merged dataset)



