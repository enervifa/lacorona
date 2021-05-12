# Generic data reader

require(tidyverse)
require(lubridate)

# Source the read scripts
script_dir <- "LaCorona_Scripts/lacorona"
source(paste(script_dir,"read_files.R",sep="/"))
source(paste(script_dir,"read_rain.R",sep="/"))
source(paste(script_dir,"read_weather.R",sep="/"))
source(paste(script_dir,"read_well.R",sep="/"))

### ----- use case 1
# What do you want to read?
main_type <- "Flumes"
which_catch <- 4 # or 2, 3 4
what_to_plot <- c("Water Level, meters","Flow (m3/sec)")

### ---- use case 2
# main_type <- "Flumes"
# which_catch <- 1 # or 2, 3 4
# what_to_plot <- list("Level","Temperature")
# specific <- "HOBOU20"
# ISCO, HOBOU20, HOBOU12(Stevens), Emergency (only for V1V2)

### --- use case 3
# main_type <- "Rain"
# which_catch <- 2 # or 1, 3 4
# what_to_plot <- list("Rain","Temperature")


# generic read function
read_data <- function(main_type, which_catch,
                      what_to_plot, main_path = "SampleFiles",
                      specific = NULL,
                      fill_missing = F) {

  # interpret the main type to select the folder
    fun <- case_when(
      main_type == "Flumes" ~ "read_flume",
      main_type == "Wells" ~ "read_well",
      main_type == "Rain" ~ "read_rain",
      main_type == "Weather" ~ "read_weather"
    )
  
    # read in the main data
    data_r <- do.call(fun,
                      list(which_catch = which_catch, 
                           main_p = main_path))
    if (main_type == "Flumes") {
      data_r <- bind_rows(data_r)
    }
    
    
    if (fill_missing == T) {
      ## -----------------------------------------
      ## in here do something with fill_missing = T
      ## ------------------------------------------
      # start with looking at the isco data
      test <- sum(ifelse(is.na(data[[1]]$`Level (ft)`)==T,1,0))
      
    }
    # number of data in "isco" file
    isco_data <- data_r %>% filter(logger == "isco")
    n_isco <- nrow(isco_data)
    # if there are no missing data
    if (fill_missing == F | nrow(na.omit(isco_data)) == n_isco) {
      data_r <- data_r %>% filter(logger == "isco")
    }
    
    
    # check if weather data or rain data is needed
    if (main_type != "Rain" || any(grepl("Rainfall", what_to_plot)) == T) {
      data_rain <- read_rain(which_catch = which_catch,
                             main_p = main_path)
    }
    if (main_type != "Weather" || any(grepl("Temperature", what_to_plot)) == T) {
      data_w <- read_weather(which_catch = which_catch,
                             main_p = main_path)
    }
#browser()
    # add the flow data if plotting of flow data is required
    if (any(grepl("Flow",what_to_plot))==T) {
      data_plotting <- flow_convert(data_r, catch = which_catch,
                                    path = main_path)
    } else {
      data_plotting <- data_r
    }
    
        
    
    # merge with rainfall data if exists
    if (exists("data_rain")) {
      data_plotting <- left_join(data_plotting,data_rain, 
                                 by = "Date and Time")
    }
    # merge with weather data if exists
    if (exists("data_w")) {
      data_plotting <- left_join(data_plotting,data_w, 
                                 by = "Date and Time")
    }
    browser()
    # manipulate data to plot exactly what is required
  data_plotting %>%
    pivot_longer(c(what_to_plot[1],what_to_plot[2]), values_to = "Measurements",
                 names_to = "Variables") %>%
    ggplot(aes(`Date and Time`,Measurements)) + geom_line() +
    theme_bw() + facet_wrap(~Variables, scales="free")

}

# testing
read_data(main_type,which_catch,
          what_to_plot)


# Auxiliary functions
# read_flume, the output of this function is a list with data from all loggers associated with the flumes
read_flume <- function(which_catch,
                            main_p = main_path) {
  if (which_catch < 3) {
    path <- paste(main_p,"Flumes/V1V2", sep="/")
    logger_order <- c("isco","stevens","hobou20")
    
  } else {
    path <- paste(main_p,"Flumes/V3V4", sep="/")
    logger_order <- c("isco","hobou20","stevens")
  }
  dir_list <- dir(path = path)
  # exclude the emergency spill way HoboU20. Need to check with Chip
  dir_list <- dir_list[!grepl("Emergency",dir_list)]
  # Create an empty data list
  data <- list()

  # run down logger order to read in the data from each logger
  # this for loop needs to be rewritten as a function and using map
  for (i in 1:length(logger_order)){
    # find the path to the specific logger output
    read_path <- paste(path,dir_list[grep(logger_order[i],dir_list,
                                          ignore.case = T)],sep="/")
    # find the list of files
    filelist <- list.files(path=read_path,pattern ="csv")
    # use do.call to call the different functions
    # needs to be extended to read files with multiple dates
    data[[i]] <- do.call(paste0("read_",logger_order[i]),
                         # figure out which file to read
                         list(filename =filelist[ifelse(which_catch < 3,
                                                        which_catch, which_catch - 2)], 
                              input_dir = read_path)) %>% 
      # add a column with the name of logger
      mutate(logger = logger_order[i])
  }
  names(data) <- logger_order
  # convert the isco logger data from ft to depth in meters
  is <- grep("isco",logger_order)
  data[[is]] <- data[[is]] %>%
    mutate(`Water Level, meters` = `Level (ft)`*0.3048) %>%
    select(`Date and Time`, `Water Level, meters`, logger)
             
  
  # Convert the stevens logger from volt to depth
  st <- grep("stevens",logger_order)
  data[[st]] <- data[[st]] %>%
    mutate(`Water Level, meters` =
             case_when(
               which_catch == 1 ~ -0.03549 + 1.2*`Volt, V`,
               which_catch == 2 ~ -0.666 + 1.2*`Volt, V`,
               which_catch == 3 ~ 3.266 -1.28761*`Volt, V`,
               which_catch == 2 ~ -0.65 + 1.2*`Volt, V`
             )) %>%
    select(`Date and Time`,`Water Level, meters`, logger)
  
 
  # return list of logger data
  return(data)
}

# # test
# data_r <- do.call(read_flume,
#                   list(which_catch = 1, 
#                        main_p = "SampleFiles"))
# str(data_r)

# flow conversion for flumes
# HL flumes is simply an equation
HL_convert <- function(H) {
  # using http://www2.alterra.wur.nl/Internet/webdocs/ilri-publicaties/publicaties/Pub20/pub20-h7.2.pdf
  flow <- exp(0.3160 + 2.3466*log(H) + 0.2794*log(H)^2)
  return(flow)
}
# But what to do with the emergency spillway?

Tri_flume_convert <- function(data_H, catch_in, 
                              main_p) {
  # need to read in the velocity
  # find the list of files
  #browser()
  filelist <- list.files(path=paste0(main_p,"/Flumes/V3V4/ISCOsampler"),
                         pattern =".vel")
  data_v <- read_isco_velocity(filename =filelist[catch_in-2], 
                            input_dir = paste0(main_p,"/Flumes/V3V4/ISCOsampler")) 
  
  data_all <- left_join(data_H,data_v)
  # now we need to know the width of the flume
  width = 3*0.3048 # convert to m
  flow <- width*data_all$`Water Level, meters`*(data_all$`velocity (ft/s)`*0.3048)
  return(flow)
}

flow_convert <- function(data_in, catch, path) {
   # convert level in meters to m3/sec
  # HL flume
  #browser()
  if (which_catch < 3) {
    data_in <- data_in %>% 
      mutate(`Flow (m3/sec)` = HL_convert(`Water Level, meters`))
  } else {
    data_in <- data_in %>%
      mutate(`Flow (m3/sec)` = Tri_flume_convert(data_in, catch_in = catch, 
                                                             main_p = path)) 
  }
}


read_well <- function(which_catch,
                       fill_missing = F, main_p = main_path, Automatic = T) {
    read_path <- paste(main_p,"Wells",
                       ifelse(Automatic ==T, "Automatic", "Manual"), sep="/")
    # the filename from the main path (assuming "automatic")
    filelist <- list.files(read_path, pattern = "csv")
    # needs to be extended to read multiple date files
    data <- read_hobo_well(filelist[which_catch],read_path)
  

  if (fill_missing == T) {
    ## -----------------------------------------
    ## in here do something with fill_missing = T
    ## ------------------------------------------
  }
  # return list of logger data
  return(data)
}


read_rain <- function(which_catch,
                      fill_missing = F, main_p = main_path, Automatic = T) {
  # account for missing rain gauges for 2 and 3
  if (which_catch == 2) {
    which_catch <- 1
    message("using raingauge from catchment 1")
  }
  if (which_catch == 3) {
    which_catch <- 4
    message("using raingauge from catchment 4")
  }
  #browser()
  read_path <- paste(main_p,"Rain",
                     ifelse(Automatic ==T, "Automatic", "Manual"), sep="/")
  # the filename from the main path (assuming "automatic")
  filelist <- list.files(read_path, pattern = "csv")
  data <- read_hobo_rain(filelist[ifelse(which_catch==1,1,2)],read_path)
  
  
  if (fill_missing == T) {
    ## -----------------------------------------
    ## in here do something with fill_missing = T
    ## ------------------------------------------
  }
  # return list of logger data
  return(data)
}

# read_weather auxillary function
read_weather <- function(which_catch,
                      fill_missing = F, main_p = main_path, Automatic = T) {
  #browser()
  read_path <- paste(main_p,"Weather",sep="/")
  # the filename from the main path (assuming "automatic")
  file <- list.files(read_path, pattern = "dat")
  # this needs to be extended for multiple files and combining
  data <- read_hobo_weather(file,read_path)
  
  
  if (fill_missing == T) {
    ## -----------------------------------------
    ## in here do something with fill_missing = T
    ## ------------------------------------------
  }
  # return list of logger data
  return(data)
}
# main_folder_chooser <- function(what_to_plot) {
#   folder <- list()
#   # now also includes character for file names
#   folder[[1]] <- case_when(
#     grepl("level",what_to_plot[[1]], ignore.case = T) == T ~ "Flumes",
#     grepl("temperature",what_to_plot[[1]], ignore.case = T) == T ~ "Weather",
#     grepl("rain",what_to_plot[[1]], ignore.case = T) == T ~ "Rain",
#     grepl("well",what_to_plot[[1]], ignore.case = T) == T ~ "Well",
#   )
#   if (length(what_to_plot) > 1) {
#     # second variable
#     folder[[2]] <- case_when(
#       grepl("level",what_to_plot[[2]], ignore.case = T) == T ~ "Flumes",
#       grepl("temperature",what_to_plot[[2]], ignore.case = T) == T ~ "Weather",
#       grepl("rain",what_to_plot[[2]], ignore.case = T) == T ~ "Rain",
#       grepl("well",what_to_plot[[2]], ignore.case = T) == T ~ "Well",
#     )
#   }
#   return(folder)
# }

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

# # function to create paths
# path_fun <- function(main_path, folder_in, which_catch, specific,
#                      fun_data) {
#   if (is.null(specific) == F) {
#     path <- paste(main_path, folder_in, which_catch, specific,
#                   sep="/")
#   } else {
#     # empty vector
#     path <- list()
#     paths <- fun_data[6:10] # hard coded
#       # not very elegant, needs vectorisation
#       path[[i]] <- map2(folder_in, paths, paste, main_path, which_catch,
#                        sep="/") # hard coded 5
#     }
#   }
#   return(path)
# }    

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



# # Use acronym at the first part of the file
# # function can be changed when we agree on file names
# file_name_select <- function(path_a, which_site) {
#   browser()
#   files <- list.files(path = path_a, pattern = "csv")
#   files_ini <- substr(files, 1,
#                       (nchar(files) - 10))
#   #browser()
#   file_select <- files[grep(which_site, files_ini)]
#   return(file_select)
# }

# # tester
# main_path <- "SampleFiles"
# folder_data <- "Flumes"
# specific <- "HoboU20OutsideWell"
# path_a <- paste(main_path,folder_data,which_catch, specific,
#                 sep="/")
# test <- file_name_select(path_a, which_site)
# test


