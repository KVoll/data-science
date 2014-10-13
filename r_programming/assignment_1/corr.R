################################################################################
##  Kristi Voll                                                               ##
##  8/13/2014                                                                 ##
##  Coursera CourseID rprog-008 (R Programming) Assignment 1 -- Part 3        ##
##  ========================================================================  ##
##                                                                            ##
## Write a function that takes a directory of data files and a threshold for  ##
## complete cases and calculates the correlation between sulfate and nitrate  ##
## for monitor locations where the number of completely observed cases (on    ##
## all variables) is greater than the threshold. The function should return   ##
## a vector of correlations for the monitors that meet the threshold          ##
## requirement.# If no monitors meet the threshold requirement, then the      ##
## function should return a numeric vector of length 0.                       ##
################################################################################
source("complete.R")


corr <- function(directory, threshold=0) {
    # Calculates the correlation between sulfate and nitrate for monitor
    # locations where the number of completely observed cases (on all variables)
    # is greater than the threshold.
    #
    # Args:
    #   directory: A character vector of length 1 indicating the location of the
    #   CSV files.
    #   threshold: A numeric vector of length 1 indicating the number of 
    #   completely observed observations (on all variables) required to compute 
    #   the correlation between nitrate and sulfate; the default is 0.
    #
    # Returns:
    #   A numeric vector of correlations
    data <- extractdata(directory)
    pollutants <- c('sulfate', 'nitrate')  # vector of possible pollutants
    
    complete.obs <- complete(directory)
    thresh.id <- complete.obs[, 'id'][complete.obs[, 'nobs'] > threshold]
    cr <- vector('numeric')
    if (length(thresh.id) > 0) {
        for (i in 1:length(thresh.id)) {
            snMatrix <- cbind(sulfate=data['sulfate', ][[thresh.id[i]]], 
                              nitrate=data['nitrate', ][[thresh.id[i]]])
            cr[i] <- cor(snMatrix[, 'sulfate'], snMatrix[, 'nitrate'], use='complete.obs')
            
        }
    }
    
    cr
}


extractdata <- function(directory, id=1:332) {
    docs <- sapply(id, filename, directory)  # filepaths to data
    sapply(docs, read.csv)  # read data from CSV files
}