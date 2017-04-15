rankall <- function(outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  
  d <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ##Prepare the column name dynamically for the outcome passed
  s<-strsplit(outcome," ")[[1]]
  u<- paste(toupper(substring(s,1,1)),substring(s,2),sep="",collapse=" ")
  a<-gsub(" ", ".", u)
  p<-paste("Hospital.30.Day.Death..Mortality..Rates.from",a,sep=".")
  
  ## Check if input parameters i.e.  outcome is valid
   if (!p %in% colnames(d)) {
    stop("invalid outcome")
  }
  state_arr <- sort(unique(d$State))
  l <- length(state_arr)
  hospital<-NULL
  for (i in 1:l)
  { ##Sub group data for the State passed as input
    o<-d[d$State==state_arr[i],]
  
    ##for the state, get the subset based on the outcome passed and coerce it to numeric
    n<-as.numeric(o[[p]])
  
    ## finds length i.e. no of rows for outcome array with non NA values, used for worst scenario
    l<-dim(o[!is.na(n),])[1]
  
    if(num=="best"){
      hospital[i]<- rank_hos(n,o,1)
    }
    else if(num=="worst"){
      hospital[i]<- rank_hos(n,o,l)
    }
    else if(num>l){
      hospital[i]<-NA
    }
    else{
      hospital[i]<- rank_hos(n,o,num)
    }
  }
  df <- data.frame(hospital=hospital, state=state_arr)
  return(df)
}   

rank_hos <- function(outcome_subset, state_subset, num) {
  rn <-order(outcome_subset,state_subset[, "Hospital.Name"],na.last=TRUE)
  hos<-state_subset[, "Hospital.Name"][rn][num]
  return(hos)
}