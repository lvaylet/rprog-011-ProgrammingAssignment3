best <- function(state, outcome) {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death rate
  
  ## Expected output:
  # > source("best.R")
  # > best("TX", "heart attack")
  # [1] "CYPRESS FAIRBANKS MEDICAL CENTER"
  # > best("TX", "heart failure")
  # [1] "FORT DUNCAN MEDICAL CENTER"
  # > best("MD", "heart attack")
  # [1] "JOHNS HOPKINS HOSPITAL, THE"
  # > best("MD", "pneumonia")
  # [1] "GREATER BALTIMORE MEDICAL CENTER"
  # > best("BB", "heart attack")
  # Error in best("BB", "heart attack") : invalid state
  # > best("NY", "hert attack")
  # Error in best("NY", "hert attack") : invalid outcome
  
  # Read outcome data
  full_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  # Check that state is valid (i.e. in the list of available states)
  if (!(state %in% full_data$State)) {
    stop("invalid state")
  }
  
  # Check that outcome is valid and set corresponding column name
  column <- if (outcome == "heart attack") {
    "Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  } else if (outcome == "heart failure") {
    "Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  } else if (outcome == "pneumonia") {
    "Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  } else {
    stop("invalid outcome")
  }
  
  # Extract hospital names and 30-day death rates for specified outcome
  data_for_state <- full_data[full_data$State == state, c("Hospital.Name", column)]
  # Convert 30-day death rate to numeric (as all variables were read as characters)
  data_for_state[, 2] <- as.numeric(data_for_state[, 2])
  
  # Sort data frame by increasing 30-day death rate, then by increasing hospital name to break ties
  data_for_state <- data_for_state[order(data_for_state[column], data_for_state$Hospital.Name), ]
  
  # Return hospital name with lowest 30-day death rate
  return(data_for_state$Hospital.Name[1])
}
