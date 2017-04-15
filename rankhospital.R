rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  
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
  
  ## finds length i.e. no of rows for outcome array with non NA values, used for worst scenario
  l<-dim(o[!is.na(n),])[1]
  
  if(num=="best"){
    rk<- rank_hos(n,o,1)
  }
  else if(num=="worst"){
    rk<- rank_hos(n,o,l)
  }
  else if(num>l){
    rk<-NA
  }
  else{
    rk<- rank_hos(n,o,num)
  }
  return(rk)
}   

rank_hos <- function(outcome_subset, state_subset, num) {
  rn <-order(outcome_subset,state_subset[, "Hospital.Name"],na.last=TRUE)
  hos<-state_subset[, "Hospital.Name"][rn][num]
  return(hos)
}