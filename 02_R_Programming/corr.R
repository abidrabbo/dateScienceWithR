library(dplyr)
corr <- function(directory, threshold = 0){
  pwd <- getwd() # the present working directory
  # create the path for the specdata directory
  sub_directory <- file.path(pwd, directory)

  # Now, we want to list the files in this sub_directory (with its full path)
  files <- list.files(sub_directory, full.names = TRUE)

  dfs_lst <- list()
  for (file in files) {
    # Read the CSV file into a dataframe
    df <- read.csv(file)
    
    # Append the dataframe to the list
    dfs_lst <- append(dfs_lst, list(df))
  }
  
  # Then we combine all dataframes into a single dataframe
  combined_dataframe <- bind_rows(dfs_lst)
  
  # Create a dataframe with for only the complete observable cases
  comDf_completeCaces <- combined_dataframe[complete.cases(combined_dataframe),]
  # Now, compute the number of complete observable cases by ID
  # Split the comDf_completeCaces dataframe by ID
  df_spl <- split(comDf_completeCaces, comDf_completeCaces$ID)
  # Count the observed cases for each ID
  df_nobs <- data.frame(id = integer(), nobs = integer()) # initialize an empty dataframe
  for (i in 1:length(files)) {
    n <- as.integer(i) # the i variable here is a double so we convert it into an integer.
    sub_df <- df_spl[[toString(i)]] # The sub_dataframe that corresponds to the actual id. It was necessary to convert "i" to a string otherwise the for loop will not work with all the id vectors. 
    nobs <- nrow(sub_df) # The number of rows for this id.
    nobs <- ifelse(is.null(nobs), 0, nobs)  # if there is no complete cases, we will get the value "NULL", so we have to replace NULL with 0
    #print(paste(i, nobs))
    df_nobs <- rbind(df_nobs, data.frame(id = n, nobs = nobs)) # Then append the Id and the number of rows to the empty dataframe
  }

  # Select the IDs that meet the threshold condition
  # Initialize a vector to store the IDs
  ids <- integer()
  for (i in 1:nrow(df_nobs)) {
    nobs_i <- df_nobs$nobs[i]
    if (nobs_i > threshold) {
      id_i <- df_nobs$id[i]
      ids <- c(ids, id_i)
    }
  }
  # Now, compute the correlation between sulfate and nitrate for each selected ID
  correlations <- numeric()
  for (i in ids) {
    sub_df2 <- df_spl[[toString(i)]] # The sub_dataframe that corresponds to the actual id.
    cr <- cor(sub_df2$sulfate, sub_df2$nitrate)
    correlations <- c(correlations, cr)
  }
  return(correlations)
  
}

#Testing the function
cr <- corr("specdata", 150)
head(cr)