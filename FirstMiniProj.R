# CMSC 197 (Introduction to Data Science)
# FIRST MINI PROJECT
#
# Submitted by: JAYVEE B. CASTAÑEDA
# B.S. in Computer Science - IV, UPV


## Problem 1: Pollutant Mean function

pollutantmean <- function(directory, pollutant, id = 1:332){
  
  # set working directory when directory is "specdata"
  if(grep("specdata", directory) == 1) {
    directory <- ("./specdata/")
  }
  
  # find all files in the "specdata" folder
  files <- as.character(list.files(directory))
  file_paths <- paste(directory, files, sep = "")
  
  dataset <- c() #Initialized dataset
  
  # Go through each file
  for (i in id){
    current <- read.csv(file_paths[i], header=T, sep=",")
    head(current)
    pollutant
    # when data is NA, remove them from dataset
    na_removed <- current[!is.na(current[, pollutant]), pollutant]
    dataset <- c(dataset, na_removed)
  }
  
  # # Compute and return mean
  result.mean <-  mean(dataset)
  return(result.mean)
}

# You may run the function by executing the following command in the console:
#
#       pollutantmean(directory, pollutant, id)
#
# Examples:
#       pollutantmean("specdata", "sulfate", 1)
#       pollutantmean("specdata", "sulfate", 1:10)
#       pollutantmean("specdata", "nitrate", 45)
#       pollutantmean("specdata", "nitrate", 70:72)


## Problem 2: Complete function

complete <- function(directory, id = 1:332) {
  
  # set working directory when directory is "specdata"
  if(grep("specdata", directory) == 1) {
    directory <- ("./specdata/")
  }
  # find the length of id vector
  id_len <- length(id)
  complete_data <- rep(0, id_len)
  
  # find all files in the "specdata" folder
  files <- as.character( list.files(directory) )
  file_paths <- paste(directory, files, sep="")
  
  # go through each file to add to complete data
  j <- 1
  for (i in id) {
    current <- read.csv(file_paths[i], header=T, sep=",")
    complete_data[j] <- sum(complete.cases(current))
    j <- j + 1
  }
  
  # store and return result
  result <- data.frame(id = id, nobs = complete_data)
  return(result)
}

# You may run the function by executing the following command in the console:
#
#       complete(directory, id)
#
# Examples:
#       complete("specdata", 3)
#       complete("specdata", c(4, 8, 10, 12, 14))
#       complete("specdata", 40:35)
#       complete("specdata", 18:20)


## Problem 3: Correlation function

corr <- function(directory, threshold = 0) {
  
  # set working directory when directory is "specdata"
  if(grep("specdata", directory) == 1) {
    directory <- ("./specdata/")
  }
  
  # get the complete table
  complete_table <- complete("specdata", 1:332)
  nobs <- complete_table$nobs
  
  # find the valid ids
  ids <- complete_table$id[nobs > threshold]
  
  # find the length of ids vector
  id_len <- length(ids)
  corr_vector <- rep(0, id_len)
  
  # find all files in the "specdata" directory
  files <- as.character( list.files(directory) )
  file_paths <- paste(directory, files, sep="")
  j <- 1
  
  # go through each file to find the correlation
  for(i in ids) {
    current <- read.csv(file_paths[i], header=T, sep=",")
    corr_vector[j] <- cor(current$sulfate, current$nitrate, use="complete.obs")
    j <- j + 1
  }
  
  # store and return result
  result <- corr_vector
  return(result)   
}

# You may run the function by executing the following command in the console:
#
#       corr(directory, threshold)
#
# Examples:
#       corr("specdata", 150)
#       corr("specdata", 300)
#       corr("specdata", 4000)
#       corr("specdata") - Note: No threshold is given, so it is ZERO (0) by default
#
# You may also execute the following after to summarize the data:
#       head(cr); summary(cr)


## Problem 4: Hospital 30-Day Death (Mortality) Rates from Heart Attack

# Read data from file
outcome <- read.csv('outcome-of-care-measures.csv', colClasses = "character")

# Take only the data for Column 11 (which contains the mortality rates from heart attack)
mortalityrates <- outcome[, 11]

# Convert character data to numerics
num_version <- as.numeric(mortalityrates)

# Create histogram from the num_version of data
hist(num_version,
  main = "Hospital 30-Day Death (Mortality) Rates from Heart Attack", # Main title
  xlab = "Deaths", # Label for x-axis
  col = "cyan") # color of bars