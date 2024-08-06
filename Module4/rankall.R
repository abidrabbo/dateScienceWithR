# Programming Assignment 3

# pwd <- getwd() # the present working directory
# nwd <- file.path(pwd, "Module4", "ProgAssignment3-data") # Path to the new working directory
# setwd(nwd)

# 4- Ranking hospitals in all states
rankall <- function(outcome, num = "best") {
  # read the data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  data <- data[, c(2, 7, 11, 17, 23)] # select only the columns that are concerned
  # Rename columns
  names(data) <- c("Hospital", "State", "HRHA", "HRHF", "HRPN")
  # HRHA: Hospital Rate for Heart Attack
  # HRHF: Hospital Rate for Heart Failure
  # HRPN: Hospital Rate for Pneumonia
  
  # Check that outcome are valid
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  invalid_outcome <- "invalid outcome"
  
  try(if (!(outcome %in% outcomes))
    stop(invalid_outcome))
  
  # Split the dataframe by State name
  data_by_state <- split(data, data$State)
  
  # Now, initialize a dataframe for all hospitals that match the rank
  hospitals_by_rank <- data.frame(Hospital = character(), State = character())
  
  # Now we will loop throw each state for the selected outcome to select the hospitals that match the rank
  
  states <- sort(unique(data$State))
  
  for (state in states) {
    # search in the outcome values by outcome name
    if (outcome == "heart attack") {
      HRHA_by_state <- data_by_state[[state]] # select only the the sub-dataframe that matches the present state name
      HRHA_by_state <- HRHA_by_state[, c(1, 2, 3)] # select only the concerned columns
      
      # Convert HRHA to numeric, coercing non-numeric values to NA (to remove the rows with "Not Available" data)
      HRHA_by_state$HRHA <- as.numeric(HRHA_by_state$HRHA)
      
      # Remove rows with NA values in the HRHA column
      HRHA_by_state <- HRHA_by_state[!is.na(HRHA_by_state$HRHA), ]
      
      # sort data_by_state by HRHA column then by Hospital name alphabetically
      HRHA_by_state_sorted <- HRHA_by_state[order(HRHA_by_state$HRHA, HRHA_by_state$Hospital), ]
      
      # Then add a 'Rank' column populated with numbers that go from 1 to the number of rows of this sorted dataframe
      HRHA_by_state_sorted$Rank <- 1:nrow(HRHA_by_state_sorted)
      
      # Now, the best hospital will have the rank 1
      # The worst hospital will have the rank nrow(df)
      # The other hospitals will be searched by rank
      # If the value of num is smaller than "best" or grater than "worst" the program will then return the value of NA for the name of the hospital
      
      # so :
      best <- 1
      worst <- nrow(HRHA_by_state_sorted)
      
      # Now, we will evaluate the values of num then select the name of the hospital depending on the provided value
      if (num == "best") {
        num <- best
      } else if (num == "worst") {
        num <- worst
      } else {
        num <- as.integer(num) # in this case the value of num will be a number
        hospital_na <- "NotNA" # in general, hospital value is not NA
        
        if (num < best || num > worst) {
          hospital_na <- NA
        }
      }
      
      if (is.na(hospital_na)) {
        # if hospital_na == NA,
        hospital <- hospital_na # then we will assign NA to the hospital variable to fill the row in the dataframe hospitals_by_rank
        
      } else {
        hospital <- HRHA_by_state_sorted$Hospital[HRHA_by_state_sorted$Rank == num]
      }
      
      # Then append the hospital name and the state name to the empty dataframe "hospitals_by_rank"
      selected_hospital_state <- data.frame(Hospital = hospital, State = state)
      hospitals_by_rank <- rbind(hospitals_by_rank, selected_hospital_state)
      
    } else if (outcome == "heart failure") {
      HRHF_by_state <- data_by_state[[state]] # select only the the sub-dataframe that matches the present state name
      HRHF_by_state <- HRHF_by_state[, c(1, 2, 4)] # select only the concerned columns
      
      # Convert HRHF to numeric, coercing non-numeric values to NA (to remove the rows with "Not Available" data)
      HRHF_by_state$HRHF <- as.numeric(HRHF_by_state$HRHF)
      
      # Remove rows with NA values in the HRHF column
      HRHF_by_state <- HRHF_by_state[!is.na(HRHF_by_state$HRHF), ]
      
      # sort data_by_state by HRHF column then by Hospital name alphabetically
      HRHF_by_state_sorted <- HRHF_by_state[order(HRHF_by_state$HRHF, HRHF_by_state$Hospital), ]
      
      # Then add a 'Rank' column populated with numbers that go from 1 to the number of rows of this sorted dataframe
      HRHF_by_state_sorted$Rank <- 1:nrow(HRHF_by_state_sorted)
      # Now, the best hospital will have the rank 1
      # The worst hospital will have the rank nrow(df)
      # The other hospitals will be searched by rank
      # If the value of num is smaller than "best" or grater than "worst" the program will then return the value of NA for the name of the hospital
      
      # so :
      best <- 1
      worst <- nrow(HRHF_by_state_sorted)
      
      # Now, we will evaluate the values of num then select the name of the hospital depending on the provided value
      if (num == "best") {
        num <- best
        hospital_na <- "NotNA" # in general, hospital value is not NA
      } else if (num == "worst") {
        num <- worst
        hospital_na <- "NotNA" # in general, hospital value is not NA
      } else {
        num <- as.integer(num) # in this case the value of num will be a number
        hospital_na <- "NotNA" # in general, hospital value is not NA
        
        if (num < best || num > worst) {
          hospital_na <- NA
        }
      }
      if (is.na(hospital_na)) {
        # if hospital_na == NA,
        hospital <- hospital_na # then we will assign NA to the hospital variable to fill the row in the dataframe hospitals_by_rank
        
      } else {
        hospital <- HRHF_by_state_sorted$Hospital[HRHF_by_state_sorted$Rank == num]
      }
      
      # Then append the hospital name and the state name to the empty dataframe "hospitals_by_rank"
      selected_hospital_state <- data.frame(Hospital = hospital, State = state)
      hospitals_by_rank <- rbind(hospitals_by_rank, selected_hospital_state)
      
    } else { # When outcume == "pneumonia"
      HRPN_by_state <- data_by_state[[state]] # select only the the sub-dataframe that matches the present state name
      HRPN_by_state <- HRPN_by_state[, c(1, 2, 5)] # select only the concerned columns

      # Convert HRPN to numeric, coercing non-numeric values to NA (to remove the rows with "Not Available" data)
      HRPN_by_state$HRPN <- as.numeric(HRPN_by_state$HRPN)
      
      # Remove rows with NA values in the HRPN column
      HRPN_by_state <- HRPN_by_state[!is.na(HRPN_by_state$HRPN), ]
      
      # sort data_by_state by HRPN column then by Hospital name alphabetically
      HRPN_by_state_sorted <- HRPN_by_state[order(HRPN_by_state$HRPN, HRPN_by_state$Hospital), ]
      
      # Then add a 'Rank' column populated with numbers that go from 1 to the number of rows of this sorted dataframe
      HRPN_by_state_sorted$Rank <- 1:nrow(HRPN_by_state_sorted)
      
      # Now, the best hospital will have the rank 1
      # The worst hospital will have the rank nrow(df)
      # The other hospitals will be searched by rank
      # If the value of num is smaller than "best" or grater than "worst" the program will then return the value of NA for the name of the hospital
      
      # so :
      best <- 1
      worst <- nrow(HRPN_by_state_sorted)
      
      # Now, we will evaluate the values of num then select the name of the hospital depending on the provided value
      if (num == "best") {
        num <- best
        hospital_na <- "NotNA" # in general, hospital value is not NA
      } else if (num == "worst") {
        num <- worst
        hospital_na <- "NotNA" # in general, hospital value is not NA
      } else {
        num <- as.integer(num) # in this case the value of num will be a number
        hospital_na <- "NotNA" # in general, hospital value is not NA
        
        if (num < best || num > worst) {
          hospital_na <- NA
        }
      }
      
      if (is.na(hospital_na)) {
        # if hospital_na == NA,
        hospital <- hospital_na # then we will assign NA to the hospital variable to fill the row in the dataframe hospitals_by_rank
        
      } else {
        hospital <- HRPN_by_state_sorted$Hospital[HRPN_by_state_sorted$Rank == num]
      }
      
      # Then append the hospital name and the state name to the empty dataframe "hospitals_by_rank"
      selected_hospital_state <- data.frame(Hospital = hospital, State = state)
      hospitals_by_rank <- rbind(hospitals_by_rank, selected_hospital_state)
      
    }
  }
  return(hospitals_by_rank)
}