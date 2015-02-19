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
  data_all_states[, column] <- suppressWarnings(as.numeric(data_all_states[, column]))
  
  # Sort data frame by state, then by increasing 30-day death rate, then by increasing hospital name to break ties
  data_all_states <- data_all_states[order(data_all_states$State, data_all_states[column], data_all_states$Hospital.Name, na.last = NA), ]
  
  # Split full data for each state, then use lapply to 
  data_by_state <- split(data_all_states, data_all_states$State)
  
  helper <- function(data_state, num) {
    # Return hospital name with reauested 30-day death rate
    r <- if (num == "best") {
      data_state$Hospital.Name[1] 
    } else if (num == "worst") {
      data_state$Hospital.Name[length(data_state$Hospital.Name)]
    } else if (num <= length(data_state$Hospital.Name)) {
      data_state$Hospital.Name[num]
    } else {
      NA
    }
  }
  
  result <- lapply(data_by_state, helper, num)
  
  return(data.frame(hospital = unlist(result), state = names(result), row.names = names(result)))
}
