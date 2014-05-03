## rankall() -- Prog Assignment 3, Part 3 for R Programming course on Coursera.
## aeamaea -- Amr Malik

rankall <- function(outcomestring, num = "best")  {
  ## Read outcome data into a DataFrame
  outcomeDF <- read.csv("outcome-of-care-measures.csv", colClasses="character")
  
  ## Get a list of all states 
  
  listOfStates <- unique(outcomeDF$State)

  ## Get just State,Hospital Name, and the 3 outcomes we care about
  newDF <- outcomeDF[,c("State","Hospital.Name","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure","Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")]
  ## give short names, long names for outcomes are stupid
  names(newDF) <- c("State","Hospital.Name","HA30d","HF30d","PN30d")
  
  ## convert columns 3:5 to numeric (these are the outcome numbers)
  newDF[,c(3,4,5)] <- sapply(newDF[,c(3,4,5)], as.numeric)
  
  ## Build 3 Data frames each has one outcome and ALL states
  HA <- newDF[,c(1,2,3)] ## State,Hosp,Outcome=30dayHeartAttack
  HF <- newDF[,c(1,2,4)] ## State,Hosp,Outcome=30dayHeartFail
  PN <- newDF[,c(1,2,5)] ## State,Hosp,Outcome=30dayPneumonia
  
  ## Now repopulate these data frames such that each has no NA's
  HA <- HA[!is.na(HA[3]), ] ## get ALL rows where 3rd col aka the outcome i.e., 30 day Heart Attack is not NA
  HF <- HF[!is.na(HF[3]), ] ## remove NAs for Heart Failure
  PN <- PN[!is.na(PN[3]), ] ## remove NAs for pneumonia
  
  ## Now we order each of these outcome vectors by State,Outcome,HospitalName
  HA <- HA[order(HA$State,HA$HA30d,HA$Hospital.Name),]
  HF <- HF[order(HF$State,HF$HF30d,HF$Hospital.Name),]
  PN <- PN[order(PN$State,PN$PN30d,PN$Hospital.Name),]
  
  if(!is.na(match(toupper(outcomestring), c("HEART ATTACK","HEART FAILURE","PNEUMONIA" ) ))) {
    if (toupper(outcomestring)=="HEART ATTACK") {
      myDF <- HA
    } 
    
    if (toupper(outcomestring)=="HEART FAILURE") {
      myDF <- HF
    }  
    
    if (toupper(outcomestring)=="PNEUMONIA") {
      myDF <- PN
    }  
    
  } else {stop("invalid outcome")}
  
  ## check if num is numeric but huge (larger than # of observations)
  if(is.numeric(num) && num >= 5000) {
    stop("NA")
  }
  
  ## At this point, each outcome is in its own dataframe, NA values have been removed
  ## WE also ordered each data frame by State,Outcome in that state, and the hospital with the outcome
  ## Now all we need to do is go through each data frame and pick out the relevant row and append 
  ## to the output vector
  ##
  ## Based on num we just get whatever row we want. Best = top row, Worst is top of reverse, and nth is just 
  ## whatever num happens to be indexed into the data frame for that particular row.
  ## 
  
  resultDF <- NULL
  
  for (statevar in listOfStates) { 
  
    stateRows <- nrow(myDF[myDF$State==statevar,]) ## Upper bound on rows for this state for outcome=Heart Attack

    ## each of these IF statements returns one row, 
    ## we need to build the resultDF as we iterate state by state
    
    if (num=="best") {
      resultDF <- rbind(resultDF,myDF[myDF$State==statevar,][1,2:3]) # first row should have lowest outcome since it's been sorted
    }
    
    if (num=="worst") {
      resultDF <- rbind(resultDF,myDF[myDF$State==statevar,][stateRows,2:3])  # upper limit has the highest rate for outcome
    }
    
    if(is.numeric(num)) {
      resultDF <- rbind(resultDF,myDF[myDF$State==statevar,][num,2:3])
    }
    

  }
  
  return(resultDF)
  
} ## end of rankall()