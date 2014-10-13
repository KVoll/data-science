################################################################################
##  Kristi Voll                                                               ##
##  8/13/2014                                                                 ##
##  Coursera CourseID rprog-008 (R Programming) Assignment 1 -- Part 2        ##
##  ========================================================================  ##
##                                                                            ##
## Write a function that reads a directory full of files and reports the      ##
## number of completely observed cases in each data file. The function should ##
## return a data frame where the first column is the name of the file and the ##
## second column is the number of complete cases.                             ##
################################################################################
source("pollutantmean.R")


complete <- function(directory, id=1:332) {
    # Reads a directory full of files and reports the number of completely
    # obseverd cases in each data file.
    #
    # Args:
    #   directory: A character vector of length 1 indicating the location of the 
    #   CSV files.
    #   id: An integer vector indicating the monitor ID numbers.
    #
    # Returns:
    #   A data frame of the form:
    #       id nobs
    #       1  117
    #       2  1041
    #   where 'id' is the monitor ID number and 'nobs' is the number of complete
    #   cases.
    docs <- sapply(id, filename, directory)  # filepaths to data
    data <- sapply(docs, read.csv)  # read data from CSV files
    pollutants <- c('sulfate', 'nitrate')  # vector of possible pollutants
    
    vec <- vector('numeric', length=length(data[pollutants[1], ]))
    
    for (i in 1:length(data[pollutants[1], ])) {
        
        # store number of observed cases for both pollutants
        x <- length(data[pollutants[1], ][[i]][!is.na(data[pollutants[1], ][[i]])])
        y <- length(data[pollutants[2], ][[i]][!is.na(data[pollutants[2], ][[i]])])
        
        # get the smaller of the two values
        if (x < y) {
            vec[i] <- x
        } else {
            vec[i] <- y
        }
        
    }
    
    # return the data frame
    return(data.frame(id=id, nobs=vec))
}