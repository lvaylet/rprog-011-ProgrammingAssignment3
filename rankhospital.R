rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with given rank 30-day death rate
  
  ## Expected output:
  # > source("rankhospital.R")
  # > rankhospital("TX", "heart failure", 4)
  # [1] "DETAR HOSPITAL NAVARRO"
  # > rankhospital("MD", "heart attack", "worst")
  # [1] "HARFORD MEMORIAL HOSPITAL"
  # > rankhospital("MN", "heart attack", 5000)
  # [1] NA
  
  # Read outcome data
  full_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  # Check that state is valid (i.e. in the list of available states)
  if (!(state %in% full_data$State)) {
    stop("invalid state")
  }
  
  # Check that outcome is valid and set corresponding column name
  column <- if (outcome == "heart attack") {
    "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  } else if (outcome == "heart failure") {
    "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  } else if (outcome == "pneumonia") {
    "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  } else {
    stop("invalid outcome")
  }
  
  # Extract hospital names and 30-day death rates for specified outcome
  data_for_state <- full_data[full_data$State == state, c("Hospital.Name", column)]
  # Convert 30-day death rate to numeric (as all variables were read as characters)
  data_for_state[, column] <- as.numeric(data_for_state[, column])
  
  # Sort data frame by increasing 30-day death rate, then by increasing hospital name to break ties
  data_for_state <- data_for_state[order(data_for_state[column], data_for_state$Hospital.Name, na.last = NA), ]
  
  # Return hospital name with lowest 30-day death rate
  hospital <- if (num == "best") {
    data_for_state$Hospital.Name[1] 
  } else if (num == "worst") {
    data_for_state$Hospital.Name[length(data_for_state$Hospital.Name)]
  } else if (num <= length(data_for_state$Hospital.Name)) {
    data_for_state$Hospital.Name[num]
  } else {
    NA
  }

  return(hospital)
}
