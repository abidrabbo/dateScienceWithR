library(dplyr)
complete <- function(directory, id = 1:332){
  pwd <- getwd()
  sub_directory <- file.path(pwd, directory)
  selected_files <- list()
  for (n in id) {
    file_number <- sprintf("%03d", n)
    full_path <- file.path(sub_directory, paste0(file_number, ".csv"))
    selected_files <- append(selected_files, list(full_path))
  }
  # read data from the selected files
  # first, we initialize an empty list for all dataframes
  dfs_lst <- list()
  
  # then we loop throw all csv files and append the resulted dataframe to the list of dataframes.
  
  for (file in selected_files) {
    # Read the CSV file into a dataframe
    df <- read.csv(file)
    
    # Append the dataframe to the list
    dfs_lst <- append(dfs_lst, list(df))
  }
  
  # Then we combine all dataframes into a single dataframe
  combined_dataframe <- bind_rows(dfs_lst)
  
  # To calculate the number of observed cases in each file we can split the dataframes by ID then calculate the number of rows which have a not NA value for sulfate or for nitrate
  comDf_completeCaces <- combined_dataframe[complete.cases(combined_dataframe),]
  
  # Now, we will split this dataframe by ID
  df_spl <- split(comDf_completeCaces, comDf_completeCaces$ID)
  
  # Now, we will count the observed cases for each ID
  # First, we will create an empty dataframe for the results
  results <- data.frame(id = integer(), nobs = integer())
  #print("id  nobs")
  # Then collect the information with a for loop
  for (i in id) {
    n <- as.integer(i) # the i variable here is a double so we convert it into an integer.
    sub_df <- df_spl[[toString(i)]] # The sub_dataframe that corresponds to the actual id. It was necessary to convert "i" to a string otherwise the for loop will not work with all the id vectors. 
    nobs <- nrow(sub_df) # The number of rows for this id.
    nobs <- ifelse(is.null(nobs), 0, nobs)  # if there is no complete cases, we will get the value "NULL", so we have to replace NULL with 0
    #print(paste(i, nobs))
    results <- rbind(results, data.frame(id = n, nobs = nobs)) # Then append the Id and the number of rows to the empty dataframe
  }
  print(results) # Printing the dataframe with the complete caces for each ID.
}

# Testing the function
complete("specdata", 304)
