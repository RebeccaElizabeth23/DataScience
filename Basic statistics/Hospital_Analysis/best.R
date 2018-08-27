# This function is designed to return the name of hospital with the lowest
# 30 day mortality rate by state and disease.
# It will take 2 string arguments; the 2-letter state and the disease
best <- function(state, outcome) {
  ## Read outcome data
  datafile <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  # Checks to see if the state and outcome arguments exist
  checkstate <- any(datafile$State == state)
  checkoutcome <-  any(outcome == c("heart attack", "heart failure", "pneumonia"))
  
  # If the state doesnt exist, stop function
  if (checkstate == TRUE) {
  } else {
    # If the state isnt valid, return a message to the user
    stop("invalid state")
  }
  
  # If the outcome doesnt exist, stop function
  if (checkoutcome == TRUE) {
  } else {
    # If the outcome isnt valid, return a message to the user
    stop("invalid outcome")
  }
  
  # Find the relevant data column depending on outcome specified
  if (outcome == "heart attack") {
    datacolumn <- 11
  } else if (outcome == "heart failure") {
    datacolumn <- 17
  } else if (outcome == "pneumonia") {
    datacolumn <- 23
  }
  
  # Creates a subset of the main datafile that just related to desired state
  statedata <- subset(datafile, datafile$State == state)
  # Convert characters to numerical to allow operations - suppress NA warning
  suppressWarnings(statedata[, datacolumn] <- as.numeric(statedata[, datacolumn]))
  
  # Pull out the relevant data for the specified outcome measure
  outcomedata <- data.frame(statedata[,datacolumn])
  # Omitting the NA values,what is the minimum value
  lowestvalue <- min(na.omit(outcomedata))
  # Returns the row no. for hospitals that have this value (best hospitals)
  besthospitalindex <- which(statedata[,datacolumn] == lowestvalue)
  
  # Find name of best hospital and save alphabetically (important for ties)
  hospitalname <- sort(statedata[besthospitalindex, 2])

  
  # Return hospital name in that state with lowest 30-day death rate
  # Return 1st hospital alphabetically if tie has occured
  return (hospitalname[1])
}