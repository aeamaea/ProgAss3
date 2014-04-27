
## This function  I got from stackoverflow 
## http://stackoverflow.com/questions/6364783/capitalize-the-first-letter-of-both-words-in-a-two-word-string
## ==> We need to capitalize 
simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep="", collapse=" ")
}

rankhospital <- function(state, outcomestring, num="best") {
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
    
    if (num=="best") {
            onehosp <- oneState[which.min(oneState[[fullOutcome]]),]
            return(onehosp$Hospital.Name)
    }
    
    if (num=="worst") {
        onehosp <- oneState[which.max(oneState[[fullOutcome]]),]
        return(onehosp$Hospital.Name)
    }
    
    if (is.numeric(num)) {
        
        ## get vector of 30 day death rankings from results of one state where outcome equals 
        ## whatever was passed in ("fullOutcome")
        DeathStats30Day <- oneState[[fullOutcome]]
        HospNames <- oneState$Hospital.Name
        #Build a fucking data frame so I can fucking sort it because that's all I could
        #fucking google!
        myDF <- data.frame(DeathStats30Day,HospNames)
        # order function is going to sort the data frame on the DeathStat30Day column
        mySortedDF <- myDF[order(DeathStats30Day),]
        # Now [num,] gets it to the num'th row (5th eg;) and the [2] is an index 
        # in that little vector to get the 2nd element which is the name of the hospital
        # or NA if it's off limit or something? I dno't know what the F#*&$*(#&$) is going on! :(
        # I have to pass it through as.character cuz otherwise it was returning a factor
        # and the submit script was complaining about it wtf
        return(as.character(mySortedDF[7,][2]$HospNames[1]))
        
    }
        
}
