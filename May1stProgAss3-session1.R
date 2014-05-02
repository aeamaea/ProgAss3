quit
quit()
?is.numeric
x <- "best"
if (x > 2000) { print("whaa?")}
x > 2000
"x" > 1000
"x" < 1000
"x"
is.numeric(x)
if(is.numeric(x) && x > 4000) { print("x is number and greater than 4000!")}
x
x <- 5000
if(is.numeric(x) && x > 4000) { print("x is number and greater than 4000!")}
outcomeDF <- read.csv("outcome-of-care-measures.csv", colClasses="character")
getwd()
dir()
cd("rprog-data-ProgAssignment3-data")
paste(getwd(),"/rprog-data-ProgAssignment3-data")
paste(getwd(),"/rprog-data-ProgAssignment3-data",sep="")
setwd(paste(getwd(),"/rprog-data-ProgAssignment3-data",sep=""))
getwd
dir
getwd()
dir()
outcomeDF <- read.csv("outcome-of-care-measures.csv", colClasses="character")
length(outcomeDF)
str(outcomeDF)
str(outcomeDF)
summary(outcomeDF)
ls()
rm(x)
ls
ls()
head(str(outcomeDF))
newDF <- outcomeDF[,c(State,Hospital.Name,Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)]
newDF <- outcomeDF[,c("State","Hospital.Name","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack")]
str(newDF)
newDF <- outcomeDF[,c("State","Hospital.Name","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")]
"
newDF <- outcomeDF[,c("State","Hospital.Name","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure","Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")]
str(newDF)
names(newDF) <- c("State","Hospital.Name","30DayHeartAttack","30DayHeartFailure","30DayPneumonia")
newDF
head(newDF)
names(newDF) <- c("State","Hospital.Name","30dHeartAttack","30dHeartFailure","30dPneumonia")
head(newDF)
?options
options(width=160)
head(newDF)
head(newDF,100)
head(newDF)
newDF[1,]
newDF[1,3]
as.numeric(newDF[1,3])
newDF <- newDF[,c(1,2,as.numeric(3))]
head(newDF)
newDF[1,3]
history()
outcomeDF <- read.csv("outcome-of-care-measures.csv", stringsAsFactors=FALSE)
newDF <- outcomeDF[,c("State","Hospital.Name","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure","Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")]
names(newDF) <- c("State","Hospital.Name","30DayHeartAttack","30DayHeartFailure","30DayPneumonia")
head(newDF)
newDF[1,3]
str(newDF)
lilDF <- newDF[1:10,]
lilDF
lilDF[,as.numeric(1,2,3,45)]
lilDF[,as.numeric(1,2,3,4,5)]
lilDF[,c(1,2,3,4,5)]
lilDF[,3]
lilDF[,sapply(lilDF[,c(3,4,5)],as.numeric)]
lilDF[,sapply(lilDF,as.numeric)]
as.numeric(lilDF[,c(3,4,5)])
as.numeric(unlist(lilDF[,c(3,4,5)]))
xx <- as.numeric(unlist(lilDF[,c(3,4,5)]))
xx
?sapply
lilDF
lilDF[,c(3,4,5)] <- sapply(lilDF[,c(3,4,5), as.numeric)
lilDF[,c(3,4,5)] <- sapply(lilDF[,c(3,4,5)], as.numeric)
lilDF
newDF[,c(3,4,5)] <- sapply(newDF[,c(3,4,5)], as.numeric)
length(newDF)
str(newDF)
?save.history
?history
savehistory(file="~/may1hist.R")
dir("~/*.R")
state <- "TX"
head(newDF[,State==state])
head(newDF[,newDF$State==state])
head(newDF[,State])
head(newDF)
head(newDF[,1])
head(newDF[newDF$State=="AL",])
head(newDF[newDF$State=="AL",3])
head(newDF[newDF$State=="AL",1:3])
xx <- (newDF[newDF$State=="AL",1:3])
str(xx)
xx <- (newDF[newDF$State==state,1:3])
str(xx)
yy <- xx[!is.na(xx[3], ]
yy <- xx[!is.na(xx[3]), ]
str(yy)
HA <- newDF[,c(1,2,3)]
HF <- newDF[,c(1,2,4)]
PN <- newDF[,c(1,2,5)]
head(HA)
HANA <- HA[!is.na(HA[3]), ]
HFNA <- HF[!is.na(HF[3]), ]
PNNA <- PN[!is.na(PN[3]), ]
nrows(HANA)
nrow(HANA)
savehistory()
xx <- HANA[rev(order(30DayHeartAttack)),][unique(State),]
xx <- HANA[rev(order(HANA$30DayHeartAttack)),][unique(HANA$State),]
xx <- HANA[rev(order(HANA[3])),][unique(HANA[1]),]
head(HANA[order(HANA$State),])
head(HANA[rev(order(HANA$State)),])
head(HANA[rev(order(HANA$State)),][unique(HANA$30DayHeartAttack),] )
head(HANA[rev(order(HANA$State)),][unique(HANA$"30DayHeartAttack"),] )
xx <- HANA[rev(order(HANA$State)),][unique(HANA$"30DayHeartAttack"),] 
str(xx)
xx <- HANA[rev(order(HANA$"30DayHeartAttack")),][unique(HANA$State),] 
head(xx)
xx <- HANA[rev(order(HANA$"30DayHeartAttack")),]
head(xx)
xx <- HANA[order(HANA$"30DayHeartAttack"),]
head(xx)
nrows(xx)
nrow(xx)
more(xx)
head(xx,100)
xx <- HANA[order(HANA$"30DayHeartAttack")&State=="TX",]
xx <- HANA[order(HANA$"30DayHeartAttack")& HANA$State=="TX",]
nrow(xx)
head(xx)
head(xx,20)
class(xx)
class(xx[3])
class(xx[,3])
class(xx[3,])
str(x)
str(xx)
xx[order(xx$30DayHeartAttack),]
xx[order(xx$"30DayHeartAttack"),]
xx[order(xx$"30DayHeartAttack"),][2,]
"2 here is the 2nd line in the sorted frame, but question is why didn't the order work in the first instance?"
savehistory()
savehistory(file="~/May1stProgAss3-session1.R")
