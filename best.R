best <- function(state, outcome) {                                                  
  ## Read outcome data                                                        
  ## Check that state and outcome are valid                                   
  ## Return hospital name in that state with lowest 30-day death rate 
  
  d <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ##Prepare the column name dynamically for the outcome passed
  s<-strsplit(outcome," ")[[1]]
  u<- paste(toupper(substring(s,1,1)),substring(s,2),sep="",collapse=" ")
  a<-gsub(" ", ".", u)
  p<-paste("Hospital.30.Day.Death..Mortality..Rates.from",a,sep=".")
  
  ## Check if input parameters i.e. state , outcome are valid
  if (!state %in% unique(d$State)) {
    stop("invalid state")
  } else if (!p %in% colnames(d)) {
    stop("invalid outcome")
  }
  
  ##Sub group data for the State passed as input
  o<-d[d$State==state,]
  
  ##for the state, get the subset based on the outcome passed and coerce it to numeric
  n<-as.numeric(o[[p]])
  
  ## finds best (i.e. lowest) 30-day mortality for the specified outcome in that state
  min <-min(n,na.rm=TRUE)
  min_idx<- which(n==min)
  
  ##Finds the hospital name for the best 30-day mortality rate for the specified outcome
  o[min_idx,"Hospital.Name"]
}   