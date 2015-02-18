rankall <- function(outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the (abbreviated) state name
  
  ## Expected output:
  # > source("rankall.R")
  # > head(rankall("heart attack", 20), 10)
  # hospital state
  # AK <NA> AK
  # AL D W MCMILLAN MEMORIAL HOSPITAL AL
  # AR ARKANSAS METHODIST MEDICAL CENTER AR
  # AZ JOHN C LINCOLN DEER VALLEY HOSPITAL AZ
  # CA SHERMAN OAKS HOSPITAL CA
  # CO SKY RIDGE MEDICAL CENTER CO
  # CT MIDSTATE MEDICAL CENTER CT
  # DC <NA> DC
  # DE <NA> DE
  # FL SOUTH FLORIDA BAPTIST HOSPITAL FL
  # > tail(rankall("pneumonia", "worst"), 3)
  # hospital state
  # WI MAYO CLINIC HEALTH SYSTEM - NORTHLAND, INC WI
  # WV PLATEAU MEDICAL CENTER WV
  # WY NORTH BIG HORN HOSPITAL DISTRICT WY
  
  # Read outcome data
  full_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
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
  
  # Extract states, hospital names and 30-day death rates for specified outcome
  data_all_states <- full_data[, c("State", "Hospital.Name", column)]

  # Convert 30-day death rate to numeric (as all variables were read as characters)
  data_all_states[, column] <- as.numeric(data_all_states[, column])
  
  # Sort data frame by state, then by increasing 30-day death rate, then by increasing hospital name to break ties
  data_all_states <- data_all_states[order(data_all_states$State, data_all_states[column], data_all_states$Hospital.Name, na.last = NA), ]
  
  # Extract names of all states and loop over them to get hospital name with requested rank
  result.state <- sort(unique(data_all_states$State))
  result.hospital = rep(NA, length(result.state))
  for (i in 1:length(result.state)) {
    data_for_state <- data_all_states[data_all_states$State == result.state[i], ]
    
    # Return hospital name with lowest 30-day death rate
    result.hospital[i] <- if (num == "best") {
      data_for_state$Hospital.Name[1] 
    } else if (num == "worst") {
      data_for_state$Hospital.Name[length(data_for_state$Hospital.Name)]
    } else if (num <= length(data_for_state$Hospital.Name)) {
      data_for_state$Hospital.Name[num]
    } else {
      NA
    }
  }
  
  return(data.frame(hospital = result.hospital, state = result.state))
}
