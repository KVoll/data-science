################################################################################
##  Kristi Voll                                                               ##
##  8/13/2014                                                                 ##
##  Coursera CourseID rprog-008 (R Programming) Assignment 1 -- Part 1        ##
##  ========================================================================  ##
##                                                                            ##
##  Write a function named 'pollutantmean' that calculates the mean of a      ##
##  pollutant (sulfate or nitrate) across a specified list of monitors.       ##
##  The function 'pollutantmean' takes three arguments: 'directory',          ##
##  'pollutant', and 'id'. Given a vector monitor ID numbers, 'pollutantmean' ##
##  reads that monitors' particulate matter data from the directory specified ##
##  in the 'directory' argument and returns the mean of the pollutant across  ##
##  all of the monitors, ignoring any missing values coded as NA. A prototype ##
##  of the function is as follows                                             ##
################################################################################


pollutantmean <- function(directory, pollutant, id = 1:332) {
    # Calculates the mean of a pollutant (sulfate or nitrate) across a specified
    # list of monitors.
    #
    # Args:
    #   directory: A character vector of length 1 indicating the location of 
    #   the CSV files
    #   pollutant: A character vector of length 1 indicating the name of the 
    #   pollutant for which we will calculate the mean; either "sulfate" or 
    #   "nitrate".
    #   id: An integer vector indicating the monitor ID numbers
    #   to be used.
    #
    # Returns:
    #   The mean of the pollutant across all monitors list in the 'id' vector 
    #   (ignoring NA values)

    docs <- sapply(id, filename, directory)  # filepaths to data

    data <- sapply(docs, read.csv)

    vec <- vector('numeric')
    for (i in 1:length(data[pollutant, ])) {
        vec <- c(vec, data[pollutant, ][[i]][!is.na(data[pollutant, ][[i]])])
    }
    
    mean(vec)
}


filename <- function(id, dir) {
    # Generates the filepath to where the data is stored.
    #
    # Args:
    #   directory: A character vector of length 1 indicating
    #   the location of the CSV files.
    #   id: An integer vector indicating the file path.
    #
    # Returns:
    #   The filepath for the given 'id'.
    
    if (id < 10) {
        doc <- paste(dir, "/00", id, ".csv", sep="")
    }
    else if (id < 100) {
        doc <- paste(dir, "/0", id, ".csv", sep="")
    } else {
        doc <- paste(dir, "/", id, ".csv", sep="")
    }
    
    doc
}