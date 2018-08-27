rankall <- function(outcome, num = "best") {
  
  # Read the outcome data and save
  datafile <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  checkoutcome <-  any(outcome == c("heart attack", "heart failure", "pneumonia"))
  # If the outcome doesnt exist, stop function
  if (checkoutcome == TRUE) {
    # Find the relevant data column depending on outcome specified
    if (outcome == "heart attack") {
      datacolumn <- 11
    } else if (outcome == "heart failure") {
      datacolumn <- 17
    } else if (outcome == "pneumonia") {
      datacolumn <- 23
    }
  } else {
    # If the outcome isnt valid, return a message to the user
    stop("invalid outcome")
  }
  
  states <-sort(unique(datafile$State))
  # Set an empty data frame to record the hospitals for e
  finalranks <- data.frame(matrix(vector(), 0, 2, dimnames=list(c(), c("Hospital","State"))))
  
# For each state in the datafile
  for (thisstate in states) {
    
    
    # Creates a subset of the main datafile that just related to desired state
    statedata <- subset(datafile, datafile$State == thisstate)
    # Convert characters to numerical to allow operations - suppress NA warning
    suppressWarnings(statedata[, datacolumn] <- as.numeric(statedata[, datacolumn]))
    
    # Removes the hospitals with a NA for the outcome variable 
    minusnalog <- complete.cases(statedata[,datacolumn])
    statedatacomplete <- statedata[minusnalog,]
    
    # Sort the state data rows numerically by outcome variable, then name (column 2)
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
    
    # Find name of  hospital and save alphabetically (important for ties)
    hospitalname <- statedatacomplete[rank,2]

    thisdata <- data.frame(Hospital=hospitalname, State=thisstate)
    # Save the hospital and state code to output file
    finalranks <- rbind(finalranks, thisdata)

  }
  
  return(finalranks)
}