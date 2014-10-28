rankhospital <- function(state, outcome, num = "best") {
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
    
    nhosp <- length(measures$State[measures$State == state])
    
    ## Coerce outcome mortality rates into numeric
    measures[, outcome] <- as.numeric(measures[, outcome])
    
    hospital.rates <- na.omit(measures[measures$State==state, c('Hospital.Name', outcome)])
    colnames(hospital.rates)[] <- c('Hospital.Name', 'Rate')
    hospital.rates$Rate <- as.numeric(hospital.rates$Rate)
    
    ## 30-day death rate
    rates <- hospital.rates[order(hospital.rates$Rate, hospital.rates$Hospital.Name), ]
    rates[, 'Rank'] <- 1:nrow(rates)
    
    ## Return hospital name in that state with the given rank
    if (num == 'best') {
        return(rates[1, 'Hospital.Name'])
    } else if (num == 'worst') {
        worst <- rates[rates$Rate == rates$Rate[nrow(rates)], ][1]
        return(worst$Hospital.Name[1])
    } else if (num > nhosp) {
        return(NA)
    } else {
        return(rates$Hospital.Name[rates$Rank == num])
    }
}
