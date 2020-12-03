# Reading in la Corona data

# Functions to read in all the Flume data

require(tidyverse)
require(lubridate)

read_hobo <- function(filename, dir = "SampleFiles/Flumes/V1V2/HoboU20OutsideWell",
                          coltypes = cols("d","c","d","d","d","d","c","c","c","c"),
                          skip = 1, plotit = F) {
#browser()
      file_read <- read_csv(paste(dir,filename,sep="/"),
                            skip = skip, col_types = coltypes)
      file_read <- file_read %>%
        mutate(`Date and Time` = mdy_hms(`Date Time, GMT-03:00`)) 
      colnames(file_read)[3:6] <- c("Abs Pressure kPa", "Temp, °C",
                                    "Bar Pressure kPa",
                                    "Water Level, meters")
      file_out <- file_read %>%
        select(`Date and Time`, `Temp, °C`,
               `Water Level, meters`)
    
      if (plotit == T) {
        p <- file_out %>%
          na.omit() %>%
          pivot_longer(cols = `Temp, °C`:`Water Level, meters`,
                       names_to = "Measures", values_to ="values") %>%
          ggplot(aes(`Date and Time`,values, colour = Measures)) +
            geom_line() + facet_wrap(~Measures, ncol = 2, scales = "free")
        print(p)
      }
  return(file_out)
}

# testing
read_dir <- "SampleFiles/Flumes/V1V2/HoboU20OutsideWell"
filenames <- dir(path = read_dir, pattern = ".csv")

test <- read_hobo(filenames[1],dir = read_dir,
                      plotit = T) 
head(test)

# read the stevens logger
read_stevens <- function(filename, dir = inputdir,
                         coltypes = cols("d","c","d","d","c","c","c","c","c"),
                         skip = 1, plotit = F) {
  
  file_read <- read_csv(paste(dir,filename,sep="/"),
                        skip = skip, col_types = coltypes)
  file_read <- file_read %>%
    mutate(`Date and Time` = mdy_hms(`Date Time, GMT-03:00`)) 
  colnames(file_read)[3:4] <- c("Volt, V", "Temp, °C")
  file_out <- file_read %>%
    select(`Date and Time`, `Volt, V`,`Temp, °C`)
  
  if (plotit == T) {
    p <- file_out %>%
      na.omit() %>%
      pivot_longer(cols = `Volt, V`:`Temp, °C`,
                   names_to = "Measures", values_to ="values") %>%
      ggplot(aes(`Date and Time`,values, colour = Measures)) +
      geom_line() + facet_wrap(~Measures, ncol = 2, scales = "free")
    print(p)
  }
  return(file_out)
}

# testing
read_dir <- "SampleFiles/Flumes/V1V2/StevensHoboU12"
filenames <- dir(path = read_dir, pattern = ".csv")

test_stevens <- read_stevens(filenames[1],dir = read_dir,
                  plotit = T) 
head(test_stevens)
