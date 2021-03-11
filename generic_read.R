# Generic data reader

require(tidyverse)
require(lubridate)

# Source the read scripts
script_dir <- "LaCorona_Scripts/lacorona"
source(paste(script_dir,"read_files.R",sep="/"))
source(paste(script_dir,"read_rain.R",sep="/"))
source(paste(script_dir,"read_weather.R",sep="/"))
source(paste(script_dir,"read_well.R",sep="/"))
# lookup table
folder_function_decision_table <- 
  read_csv(paste(script_dir,"folder_function_decision_table.csv", sep="/"))

# What do you want to read?
main_type <- "Flumes"
which_catch <- "V1V2" # or V3V4
which_site <- 1 # can be 1,2,3 or 4
what_to_plot <- list("Level","Temperature")
specific <- "HOBO"
# ISCO, Stevens, HOBO

# generic read function
read_data <- function(main_type, which_catch,
                      which_site = 1, 
                      what_to_plot, main_path = "SampleFiles",
                      specific = NULL,
                      fill_missing = TRUE, 
                      lookup = folder_function_decision_table) {
  browser()
  # interpret the main type to select the folder
  folder_data <- main_folder_chooser(main_type)
  # which read data function to use?
  # Use the functions and paths in fun_rows select to read in the data
  fun_data <- rows_select(what_to_plot,lookup)

  # paths
  path_a <- path_fun(main_path, folder_data, which_catch, specific,
                     fun_data)  
  
  # find file names
  files_to_read <- lapply(path_a, file_name_select, which_site)
  
  # now read the files
  # functions in a list: function_list
  data <- list()
  function_list <- as.list(fun_data[3:6]) # hard coded 3:6
  for (i in 1:length(function_list)) {
    # we need to clarify file naming conventions
    data[[i]] <- do.call(function_list[[i]],
                         list(file_name = file_to_read[[i]],
                         input_dir = path_a[[i]]))
  
    # investigate the missing data
    if (fill_missing == T) {
      missing <- list()
      # 1. function to quantify the missing data in each dataset
      missing[[i]] <- length(data[[i]][is.na(data[[i]])==T])
      
      # 2. decision on matching (which_max)
      # 3. matching and filling
    }  
  }
  
  # manipulate data to plot exactly what is required

}

# testing
read_data(main_type, which_catch,
          which_site = 1, 
          what_to_plot)


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

# function to select correct row from look-up table
rows_select <- function(what_to_plot, lookup) {
  # find rows for first variable
  funs_rows <- grep(what_to_plot[[1]],lookup$`variable 1`,
                    ignore.case = T)
  # if there is only one variable to plot
  #(make this so it can be time later)
  if (is.null(what_to_plot[[2]])) {
    funs_rows_select <- lookup[is.na(lookup[funs_rows,"variable 2"]),]
  } else {
    funs_rows_select <- lookup[grep(what_to_plot[[2]],
                                    lookup[funs_rows,"variable 2"],
                                    ignore.case = T),]
  }
  return(funs_rows_select)
}

# function to create paths
path_fun <- function(main_path, folder_data, which_catch, specific,
                     fun_data) {
  if (is.null(specific) == F) {
    path <- paste(main_path, folder_data, which_catch, specific,
                  sep="/")
  } else {
    # number of paths needed
    no_paths <- length(na.omit(fun_data[(ncol(fun_data)-3):ncol(fun_data)]))
    # empty vector
    path <- rep(NA, no_paths)
    for (i in 1:length(path)) {
      # not very elegant, needs vectorisation
      path[i] <- paste(main_path, folder_data, which_catch,
                       fun_data[i + 6], sep="/") # hard coded 5
    }
  }
  return(path)
}    

# # function to find all filenames in the folder
# find_file <- function(path_a, which_site, specific, fun_data) {
#   filenames <- list()
#   if (is.null(specific) == F) {
#     filenames[[1]] <- file_name_select(path_a, which_site)
#   } else {
#     no_paths <- length(na.omit(fun_data[(ncol(fun_data)-3):ncol(fun_data)]))
#     specific_folders <- na.omit(fun_data[(ncol(fun_data)-3):ncol(fun_data)])
#     for (i in 1:no_paths) {
#       filenames[[i]] <- file_name_select(path_a[[i]], which_site, 
#                                          specific_folders[i])
#     }
#     return(file_names)    
#   }
#   
# }



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


