# This function is designed to return the name of hospital that matches
# the desired rank (by state and disease) for the 30 day mortality rate
# It will take 3 arguments arguments; the 2-letter state and the disease
# and the desired rank ; this can be a number, or 'best' or 'worst' strings.

rankhospital <- function(state, outcome, num = "best") {
  # Read the outcome data and save
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
  
  # Removes the hspitals with a NA for the outcome variable 
  minusnalog <- complete.cases(statedata[,datacolumn])
  statedatacomplete <- statedata[minusnalog,]

  # Sort the state data rows numericlly by outcome variable, then name (column 2)
  statedatacomplete <- statedatacomplete[order( statedatacomplete[,datacolumn], statedatacomplete[,2]),]  
  
  # Check the rank requested - assign index to best and worse requests
  if (num == 'worst') {
    # it worst, assign rank to case with highest outcome value (last in ordered)
    rank <- dim(statedatacomplete[1])[1]
  } else if (num == 'best') {
    rank <- 1L
  } else {
    rank <- num
  }

  # Find name of best hospital and save alphabetically (important for ties)
  hospitalname <- statedatacomplete[rank,2]
  
  # Return the name of the corresponding hospital
  return(hospitalname)
  
}