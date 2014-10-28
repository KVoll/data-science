rankall <- function(outcome, num="best") {
    outcome.names <- c('heart attack', 'heart failure', 'pneumonia')
    
    ## Check that outcome is valid
    if (!(outcome %in% outcome.names)) {
        stop('invalid outcome')
    }
    rate <- sub(' ', '.', outcome)
    ## Read the outcome-of-care-measures.csv file
    measures <- read.csv('outcome-of-care-measures.csv', colClass="character")
    colnames(measures)[c(11, 17, 23)] <- sub(' ', '.', outcome.names)
    
    
    ## Coerce outcome mortality rates into numeric
    measures[, rate] <- as.numeric(measures[, rate])
    
    state <- sort(unique(measures$State))
    
    mat <- matrix(nrow=length(state), ncol=2, byrow=T)
    mat[, 2] <- state
    for (i in 1:length(state)) {
        mes <- na.omit(measures[measures$State == state[i], c(rate, 'Hospital.Name')])
        ord <- mes[order(mes[, rate], mes$Hospital.Name), ]
        ord[, 1] <- as.numeric(ord[, 1])
        if (is.na(as.numeric(num))) {
            if (num == 'best') {
                mat[[i, 1]] <- ord[1, 2]
            } 
            else if (num == 'worst') {
                #print(sort(ord[, 1], decreasing=T))
                #print(ord)
                #print(length(measures[measures$State == state[i], 'Hospital.Name']))
                ord[, 1] <- sort(ord[, 1])
                mat[[i, 1]] <- ord[order(ord[1], decreasing=T), ][1, 2] 
            } 
        }
        else if (num > length(measures[measures$State == state[i], 'Hospital.Name'])) {
            mat[[i, 1]] <- NA
        } 
        else {
            mat[[i, 1]] <- ord[num, 2]
        }
        
    }
    df <- as.data.frame(mat, row.names=state)
    colnames(df)[] <- c('hospital', 'state')
    return(df)
}