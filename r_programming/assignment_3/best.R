best <- function(state, outcome) {
    outcome.names <- c('heart attack', 'heart failure', 'pneumonia')
    
    ## Read the outcome-of-care-measures.csv file
    measures <- read.csv('outcome-of-care-measures.csv', colClass="character")
    colnames(measures)[c(11, 17, 23)] <- outcome.names
    
    ## Check that state and outcome are valid
    if (!(state %in% measures$State)) {
        stop('invalid state')
    }
    if(!(outcome %in% outcome.names)) {
        stop('invalid outcome')
    }
    
    ## Coerce outcome mortality rates into numeric
    measures[, outcome] <- as.numeric(measures[, outcome])
    
    hospital.rate <- na.omit(measures[measures$State==state, c('Hospital.Name', outcome)])
    
    rates.min <- min(hospital.rate[, outcome])
    hospitals <- hospital.rate[hospital.rate[,outcome]==rates.min, 'Hospital.Name']
    
    if (length(hospitals) != 1) {
        hospitals.best <- sort(hospitals)
        return(as.character(hospitals.best[1]))
    }
    
    as.character(hospitals[1])
}
