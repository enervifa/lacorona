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
which_catch <- "V1V2" # or V3V4
which_site <- 1 # can be 1,2,3 or 4
what_to_plot <- list("Level","Temperature")
specific <- "HOBO"
# ISCO, Stevens, HOBO

# generic read function
Read_data <- function(main_type, which_catch,
                      which_site = 1, 
                      what_to_plot, main_path = "SampleFiles",
                      specific = NULL,
                      fill_missing = TRUE) {
  # interpret the main type to select the folder
  folder_data <- main_folder_chooser(main_type)
  # which read data function to use?
  
    # in here the code to choose the read data functions
  
  # here the code to choose the file names and paths
  
  
  # paths
  if (is.null(specific) == F) {
    path_a <- paste(main_path, folder_data, which_catch, specific,
                    sep="/")
  } else {
    # based on what to plot, find specific folder
  }
  
  # find file
  filenames <- file_name_select(path_a, which_site)
  
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
  # now also includes character for file names
  folder <- case_when(
    grepl("flume",main_type, ignore.case = T) == T ~ "Flumes",
    grepl("weather",main_type, ignore.case = T) == T ~ "Weather",
    grepl("rain",main_type, ignore.case = T) == T ~ "Rain",
    grepl("well",main_type, ignore.case = T) == T ~ "Well",
  )
  return(folder)
}

 
# Use acronym at the first part of the file
# function can be changed when we agree on file names
file_name_select <- function(path_a, which_site) {
  files <- list.files(path = path_a, pattern = "csv")
  files_ini <- substr(files, 1,
                      (nchar(files) - 10))
  #browser()
  file_select <- files[grep(which_site, files_ini)]
  return(file_select)
}

# # tester
# main_path <- "SampleFiles"
# folder_data <- "Flumes"
# specific <- "HoboU20OutsideWell"
# path_a <- paste(main_path,folder_data,which_catch, specific,
#                 sep="/")
# test <- file_name_select(path_a, which_site)
# test


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