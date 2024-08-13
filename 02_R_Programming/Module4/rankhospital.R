# Programming Assignment 3

pwd <- getwd() # the present working directory
nwd <- file.path(pwd, "Module4", "ProgAssignment3-data") # Path to the new working directory
setwd(nwd)

# 3- Ranking hospitals by outcome in a state
rankhospital <- function(state, outcome, num = "best") {
  # read the data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  data <- data[, c(2, 7, 11, 17, 23)] # select only the columns that are concerned
  # Rename columns
  names(data) <- c("Hospital", "State", "HRHA", "HRHF", "HRPN")
  # HRHA: Hospital Rate for Heart Attack
  # HRHF: Hospital Rate for Heart Failure
  # HRPN: Hospital Rate for Pneumonia
  
  # Testing the content of our data :
  # print(head(data, n=2))
  # print(tail(data, n=2))
  # print(data[c(3935, 4085, 4103, 3954, 4010, 3962), ]) # The example in the .pdf file.

  # Check that state and outcome are valid
  states <- data$State
  outcomes <- c("heart attack", "heart failure", "pneumonia")

  invalid_state <- "invalid state"
  invalid_outcome <- "invalid outcome"

  try(
    if (!(state %in% states)) stop(invalid_state)
  )

  try(
    if (!(outcome %in% outcomes)) stop(invalid_outcome)
  )

  # filter the dataframe by State name
  data_by_state <- subset(data, State == state)

  # search in the outcome values by outcome name
  if (outcome == "heart attack") {
    HRHA_by_state <- data_by_state[, c(1, 2, 3)] # select only the concerned columns
    
    # Convert HRHA to numeric, coercing non-numeric values to NA (to remove the rows with "Not Available" data)
    HRHA_by_state$HRHA <- as.numeric(HRHA_by_state$HRHA)
    
    # Remove rows with NA values in the HRHA column
    HRHA_by_state <- HRHA_by_state[!is.na(HRHA_by_state$HRHA), ]
    
  # sort data_by_state by HRHA column then by Hospital name alphabetically
    HRHA_by_state_sorted <- HRHA_by_state[order(HRHA_by_state$HRHA, HRHA_by_state$Hospital), ]
    
    # Then add a 'Rank' column populated with numbers that go from 1 to the number of rows of this sorted dataframe
    HRHA_by_state_sorted$Rank <- 1:nrow(HRHA_by_state_sorted)
    print(head(HRHA_by_state_sorted, n=3))
    print(tail(HRHA_by_state_sorted, n=3))
    
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
      print(paste("The best hospital rank for", outcome, "is:"))
    } else if (num == "worst") {
      num <- worst
      print(paste("The worst hospital rank for", outcome, "is:"))
    } else {
      num <- as.integer(num) # in this case the value of num will be a number
      if (num < best || num > worst) {
        hospital <- NA
        print(paste("There is no rank", num, "in the selected dataframe. The hospital name for this rank is:", hospital))
      } else {
        print(paste("The hospital which has the rank of", num, "is:"))
      }
      
    }
    
    hospital <- HRHA_by_state_sorted$Hospital[HRHA_by_state_sorted$Rank == num]
    print(hospital)
    
  } else if (outcome == "heart failure") {
    HRHF_by_state <- data_by_state[, c(1, 2, 4)] # select only the concerned columns
    
    # Convert HRHF to numeric, coercing non-numeric values to NA (to remove the rows with "Not Available" data)
    HRHF_by_state$HRHF <- as.numeric(HRHF_by_state$HRHF)
    
    # Remove rows with NA values in the HRHF column
    HRHF_by_state <- HRHF_by_state[!is.na(HRHF_by_state$HRHF), ]
    
    # sort data_by_state by HRHF column then by Hospital name alphabetically
    HRHF_by_state_sorted <- HRHF_by_state[order(HRHF_by_state$HRHF, HRHF_by_state$Hospital), ]
    
    # Then add a 'Rank' column populated with numbers that go from 1 to the number of rows of this sorted dataframe
    HRHF_by_state_sorted$Rank <- 1:nrow(HRHF_by_state_sorted)
    print(head(HRHF_by_state_sorted, n=3))
    print(tail(HRHF_by_state_sorted, n=3))
    
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
      print(paste("The best hospital rank for", outcome, "is:"))
    } else if (num == "worst") {
      num <- worst
      print(paste("The worst hospital rank for", outcome, "is:"))
    } else {
      num <- as.integer(num) # in this case the value of num will be a number
      if (num < best || num > worst) {
        hospital <- NA
        print(paste("There is no rank", num, "in the selected dataframe. The hospital name for this rank is:", hospital))
      } else {
        print(paste("The hospital which has the rank of", num, "is:"))
      }
    }
    
    hospital <- HRHF_by_state_sorted$Hospital[HRHF_by_state_sorted$Rank == num]
    print(hospital)
  
  } else {
    HRPN_by_state <- data_by_state[, c(1, 2, 5)] # select only the concerned columns
    
    # Convert HRPN to numeric, coercing non-numeric values to NA (to remove the rows with "Not Available" data)
    HRPN_by_state$HRPN <- as.numeric(HRPN_by_state$HRPN)
    
    # Remove rows with NA values in the HRPN column
    HRPN_by_state <- HRPN_by_state[!is.na(HRPN_by_state$HRPN), ]
    
    # sort data_by_state by HRPN column then by Hospital name alphabetically
    HRPN_by_state_sorted <- HRPN_by_state[order(HRPN_by_state$HRPN, HRPN_by_state$Hospital), ]
    
    # Then add a 'Rank' column populated with numbers that go from 1 to the number of rows of this sorted dataframe
    HRPN_by_state_sorted$Rank <- 1:nrow(HRPN_by_state_sorted)
    print(head(HRPN_by_state_sorted, n=3))
    print(tail(HRPN_by_state_sorted, n=3))
    
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
      print(paste("The best hospital rank for", outcome, "is:"))
    } else if (num == "worst") {
      num <- worst
      print(paste("The worst hospital rank for", outcome, "is:"))
    } else {
      num <- as.integer(num) # in this case the value of num will be a number
      if (num < best || num > worst) {
        hospital <- NA
        print(paste("There is no rank", num, "in the selected dataframe. The hospital name for this rank is:", hospital))
      } else {
        print(paste("The hospital which has the rank of", num, "is:"))
      }
    }
    
    hospital <- HRPN_by_state_sorted$Hospital[HRPN_by_state_sorted$Rank == num]
    print(hospital)
   
  }

}