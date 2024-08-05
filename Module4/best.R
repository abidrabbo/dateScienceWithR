# Programming Assignment 3

pwd <- getwd() # the present working directory
nwd <- file.path(pwd, "Module4", "ProgAssignment3-data") # Path to the new working directory
setwd(nwd)

# 1- Plot the 30-day mortality rates for heart attack
# Read the outcome data
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)

ncol(outcome)
nrow(outcome)
names(outcome)

# Histogram of the 30-day death rates from heart attack (column 11 in the outcome dataset)
outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11])

# 2- Finding the best hospital in a state
best <- function(state, outcome) {
  # read the data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  data <- data[, c(2, 7, 13, 19, 25)] # select only the columns that are concerned
  # Rename columns
  names(data) <- c("Hospital", "State", "HA", "HF", "PN")
  # HA: outcome rate for Heart Attack
  # HF: outcome rate for Heart Failure
  # PN: outcome rate for PNeumonia

  # check the validity of the arguments
  invalid_state <- "invalid state"
  invalid_outcome <- "invalid outcome"

  try(
    if (!(state %in% data$State)) stop(invalid_state) # The state column in the new dataframe has the index 2 and the new name for this column is "State".
  )
  
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  try(
    if (!(outcome %in% outcomes)) stop(invalid_outcome)
  )

  # filter the dataframe by State name
  data_by_state <- subset(data, State == state)

  # search in the outcome values by outcome name
  if (outcome == "heart attack") {
  min <- min(as.numeric(data_by_state$HA), na.rm = TRUE) # compute the minimum value for column HA which is for the lower rates from heart attack
    print(paste("for",outcome, "the best value is:", min))
    hospitals <- subset(data_by_state, data_by_state$HA == toString(sprintf("%.1f",min)))$Hospital # list of hospitals that are the best

    # Sort the names alphabetically
    sorted_hospitals <- sort(hospitals)

    # Get only the first hospital in the sorted list
    best_hospital <- sorted_hospitals[1]
    print(paste("The best hospital is:", best_hospital))
    
  } else if (outcome == "heart failure") {
    min <- min(as.numeric(data_by_state$HF), na.rm = TRUE) # compute the minimum value for column HF which is for the lower rates from heart failure
    print(paste("for",outcome, "the best value is:", min))
    hospitals <- subset(data_by_state, data_by_state$HF == toString(sprintf("%.1f",min)))$Hospital

    # Sort the names alphabetically
    sorted_hospitals <- sort(hospitals)

    # Get only the first hospital in the sorted list
    best_hospital <- sorted_hospitals[1]
    print(paste("The best hospital is:", best_hospital))
    
  } else {
    min <- min(as.numeric(data_by_state$PN), na.rm = TRUE) # compute the minimum value for column PN which is for the lower rates from pneumonia
    print(paste("for",outcome, "the best value is:", min))
    hospitals <- subset(data_by_state, data_by_state$PN == toString(sprintf("%.1f",min)))$Hospital

    # Sort the names alphabetically
    sorted_hospitals <- sort(hospitals)

    # Get only the first hospital in the sorted list
    best_hospital <- sorted_hospitals[1]
    print(paste("The best hospital is:", best_hospital))
    
  }

}