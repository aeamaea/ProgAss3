## This function  I got from stackoverflow 
## http://stackoverflow.com/questions/6364783/capitalize-the-first-letter-of-both-words-in-a-two-word-string
## ==> We need to capitalize 
simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep="", collapse=" ")
}

rankall <- function(outcomestring, num = "best")  {
    ## Read outcome data into a DataFrame
    outcomeDF <- read.csv("outcome-of-care-measures.csv", colClasses="character")
    
    ## Get a list of all states 
    
    listOfStates <- unique(outcomeDF$State)
    

    
    ## Now capitalize the "outcome" string which can be something like "heart failure"
    ## but convert it all to lower, why? cuz we want to bring it to a known state
    ## then Capitalize this variable using the func defined above
    ## Then we pass it to gsub which replaces all instances of SPACE with "."
    outcomestring2 <- gsub(" +", ".", simpleCap(tolower(outcomestring)))
    
    ## Now we Build the string that contains the long ass outcome variable name
    ## and see if it exists?
    
    fullOutcome <- paste("Hospital.30.Day.Death..Mortality..Rates.from.",outcomestring2,sep="",collapse="")
    
    ## Check outcomestring passed in exists in the resultset
    if(is.na(match(fullOutcome, names(outcomeDF)))) {
        stop("invalid outcome")
    }

    ## if num is too high just return NA...fuck it!
    if( num > 4000) {
        return(NA)
    }
    ## now that we have a list of states and a validated outcome name, why don't
    ## we go ahead and pass it to rankhospital() and see what happens.
    
    resultDF <- data.frame(hospital=character(), state=character())
    
    for (stateName in listOfStates) {        
        hosp <- rankhospital(stateName,outcomestring,num)   
        resultDF <- rbind(resultDF,data.frame(hospital=hosp,state=stateName))
    }
    
    return(resultDF)
    
} ## end of rankall()
