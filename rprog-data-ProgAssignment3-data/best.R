
## This function  I got from stackoverflow 
## http://stackoverflow.com/questions/6364783/capitalize-the-first-letter-of-both-words-in-a-two-word-string
## ==> We need to capitalize 
simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep="", collapse=" ")
}

best <- function(state, outcomestring) {
    ## Read outcome data into a DataFrame
    outcomeDF <- read.csv("outcome-of-care-measures.csv", colClasses="character")

    ## Check that state and outcome are valid
    
    ## Check State passed in exists in the "State" vector in Paste
    if(is.na(match(state, outcomeDF$State)) ) {
        stop("invalid state")
    }

    ## Now that we know state is good, we can use it to get subset
    ## of the data-frame that contains just the one state passed in
    oneState <- outcomeDF[outcomeDF$State==state,]

    ## Now capitalize the "outcome" string which can be something like "heart failure"
    ## but convert it all to lower, why? cuz we want to bring it to a known state
    ## then Capitalize this variable using the func defined above
    ## Then we pass it to gsub which replaces all instances of SPACE with "."
    outcomestring2 <- gsub(" +", ".", simpleCap(tolower(outcomestring)))
    
    ## Now we Build the string that contains the long ass outcome variable name
    ## and see if it exists?
    
    fullOutcome <- paste("Hospital.30.Day.Death..Mortality..Rates.from.",outcomestring2,sep="",collapse="")
    
    ## Check outcomestring passed in exists in the resultset
    if(is.na(match(fullOutcome, names(oneState)))) {
        stop("invalid outcome")
    }
    
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    
    ## we use oneState[[...]] instead of oneState$fullOutcome as the 2nd type doesn't seem to 
    ## work to subset, it was returning a NULL result
    onehosp <- oneState[which.min(oneState[[fullOutcome]]),]
    return(onehosp$Hospital.Name)

}
