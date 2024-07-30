pwd <- getwd() # this is for the present working directory
#print(paste0("The pwd is :", pwd))

# create the path for the specdata directory
directory <- file.path(pwd, "specdata")
#print(paste0("The path for specdata directory is :", directory))

# Now, we want to list the files in this specdata directory
files <- list.files(directory)
#print(files)

# Now, testing the for loop for "files" to list all files vertically
for (file in files) {
  print(file)
}
# this seems good but to read csv files we need the full path for each file. so we can do:
files <- list.files(directory, full.names = TRUE)
# Now, testing our new list of files with the full path
for (file in files) {
  print(file)
}
# seems perfect :)

# Now, we have to read each file to a dataframe then put all dataframes into a single one

# first, we initialize an empty list for all dataframes
dfs_lst <- list()

# then we loop throw all csv files and append the resulted dataframe to the list of dataframes.
#First of all, we have to load the "dplyr" package which is necessary for bind_rows function
# The easiest way to get dplyr is to install the whole tidyverse:
install.packages("tidyverse")

# Alternatively, install just dplyr:
# install.packages("dplyr")

library(dplyr)

for (file in files) {
  # Read the CSV file into a dataframe
  df <- read.csv(file)
  
  # Append the dataframe to the list
  dfs_lst <- append(dfs_lst, list(df))
}

# Then we combine all dataframes into a single dataframe
combined_dataframe <- bind_rows(dfs_lst)

# Testing the content of the combined dataframe
print("The first 5 rows in th combined_dataframe are :")
head(combined_dataframe, n=5)

print("The last 5 rows in th combined_dataframe are :")
tail(combined_dataframe, n=5)
# Perfect ! But in this case, if we creat a function, each time this function is called R will read all the files in the directory. It will be better to read only the files for the asked IDs and then creat a one dataframe for only these files. In this case we can do :

id <- 1:332 # the full number of IDs
selected_files <- list() # to collect the full path for the selected files
for (n in id) {
  file_number <- sprintf("%03d", n) # this will ceate zero-padded file numbers
  #print(file_number) # it works
  # Now, we will create the full path for each file
  full_path <- file.path(directory, paste0(file_number, ".csv"))
  print(full_path) # it works. Now, we will append each path to the selected_files list
  selected_files <- append(selected_files, list(full_path))
}

# testing the result :
for (file in selected_files) {
  print(file)
}
# It works correctly. Now, we will write a function to do all that
select_some_files <- function(directory, id = 1:332){
  pwd <- getwd()
  sub_directory <- file.path(pwd, directory)
  selected_files <- list()
  for (n in id) {
    file_number <- sprintf("%03d", n)
    full_path <- file.path(sub_directory, paste0(file_number, ".csv"))
    selected_files <- append(selected_files, list(full_path))
  }
  
  print("The path of eache file for the selected IDs are :")
  for (file in selected_files) {
    print(file)
  }
}

# Testing this function now :
select_some_files("specdata", id = 43:47) # Perfect ! Now, we well developed this function so that it reads the selected files and create the dataframe

create_df <- function(directory, id = 1:332){
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
  
  # Testing the content of the combined dataframe
  print("The first 5 rows in the combined_dataframe are :")
  print(head(combined_dataframe, n=5))
  
  print("The last 5 rows in the combined_dataframe are :")
  print(tail(combined_dataframe, n=5))
  
  combined_dataframe
}

# testing this funtion :
df = create_df("specdata", id = 17:35) # working very good :)
df

# Now, passing to manipulating data in the created dataframe
# for the selected dataframe, print the mean for sulfate then for nitrate. Don't forget to remove NA values
print("the mean value for sulfate is :")
print(mean(df$sulfate, na.rm = TRUE))
print("the mean value for nitrate is :")
print(mean(df$nitrate, na.rm = TRUE))

# It works well. Now, we will improve our function so that it will read the created dataframe then give the mean value for the selected pollutant

pollutantmean <- function(directory, pollutant, id = 1:332){
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
  
  print(paste("The mean value for", pollutant, "is:"))
  print(mean(combined_dataframe[[pollutant]], na.rm = TRUE))
   
}

# Testing the function
pollutantmean("specdata", "nitrate", id = 5:15)
# Great ! The job is done :)