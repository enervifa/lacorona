# Generic data reader

require(tidyverse)
require(lubridate)

# Source the read scripts
script_dir <- "LaCorona_Scripts/lacorona"
source(paste(script_dir,"read_files.R",sep="/"))
source(paste(script_dir,"read_rain.R",sep="/"))
source(paste(script_dir,"read_weather.R",sep="/"))
source(paste(script_dir,"read_well.R",sep="/"))

# What do you want to read?
main_type <- "Flumes"
which_catch <- "V1V2"
which_site <- "V1"
what_to_plot <- list("Level","Temperature")
specific <- "HOBO"
# ISCO, Stevens, HOBO

# generic read function
Read_data <- function(main_type,which_catch,
                      which_site,
                      what_to_plot,
                      specific = NULL,
                      fill_missing = TRUE) {
  # interpret the main type to select the folder
  folder_data <- main_folder_chooser(main_type)
  # which read data function to use?
  
  # in here the code to choose the read data functions
  
  # here the code to choose the filenames and paths
  # file_names
  # paths
  
  # functions in a list: function_list
  data <- list()
  for (i in 1:length(function_list)) {
    # we need to clarify file naming conventions
    data[[i]] <- do.call(function_list[[i]],filenames[[i]],
                         input_dir = paths[[i]])
  }
  # investigate the missing data
  if (fill_missing == T) {
    missing <- list()
    # 1. function to quantify the missing data in each dataset
    # 2. decision on matching (which_max)
    # 3. matching and filling
    
  }
  
  # manipulate data to plot exactly what is required
  
  
  
}


main_folder_chooser <- function(main_type) {
  folder <- case_when(
    grepl("flume",main_type, ignore.case = T) == T ~ "Flumes"
    grepl("weather",main_type, ignore.case = T) == T ~ "Weather"
    grepl("rain",main_type, ignore.case = T) == T ~ "Rain"
    grepl("well",main_type, ignore.case = T) == T ~ "Well"
  )
  return(folder)
}

readin_fun_chooser <- function(main_type, specific) {
# create a list of possible functions based on 
  folder <- case_when(
    grepl("flume",main_type, ignore.case = T) == T ~ "Flumes"
    grepl("weather",main_type, ignore.case = T) == T ~ "Weather"
    grepl("rain",main_type, ignore.case = T) == T ~ "Rain"
    grepl("well",main_type, ignore.case = T) == T ~ "Well"
  )
  return(folder)
  
  
  # first deal with specific
  if (is.null(specific) == T) {
    # in this case there can be more than one function
  } else {

  }
  
}