# Peer-graded Assignment: Getting and Cleaning Data Course Project

# The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained:
#   http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

# Here are the data for the project:
#   https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

library(data.table)

# First we will set the workin directory
p_wd <- getwd()
n_wd <- setwd(file.path(p_wd, "03_Getting_and_Cleaning_Data", "Module_4"))

# Getting the data from the web
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

# Downloading data
download.file(url, file.path(n_wd, "dataFiles.zip"))

# Extracting the data from the .zip file
unzip(zipfile = "dataFiles.zip")

#==============================================
# Task 1: Merges the training and the test sets to create one data set.
#==============================================
# First, See what is in the downloaded folder
path_data <- file.path(n_wd, "UCI HAR Dataset")
list.files(path = path_data)

# Now, we will explore some .txt files to understand the content of the data sets.
# Starting by activity_labels.txt ('activity_labels.txt': Links the class labels with their activity name.)
activity_labels <- fread(file.path(path_data, "activity_labels.txt"))
head(activity_labels)
colnames(activity_labels) <- c("activityId", "activity") # rename the columns
activity_labels[["activity"]] # "WALKING" "WALKING_UPSTAIRS" "WALKING_DOWNSTAIRS" "SITTING" "STANDING" "LAYING"
ncol(activity_labels) # 2
nrow(activity_labels) # 6

# features.txt ('features.txt': List of all features.)
features <- fread(file.path(path_data, "features.txt"))
head(features)
tail(features)
colnames(features) <- c("featureId", "feature") # rename the columns
length(unique(features[["feature"]])) # Number of unique features. Result: 477
nrow(features) # 561

# now, see the content of the training set
path_train <- file.path(path_data, "train")
list.files(path_train)

# read the X_train.txt ('train/X_train.txt': Training set.)
X_train <- fread(file.path(path_train, "X_train.txt"))
head(X_train)
tail(X_train)
nrow(X_train) # 7352 # the train set
ncol(X_train) # 561 (the names of these columns must be then the list of features)
colnames(X_train) <- c(features[["feature"]]) # rename the columns
length(X_train[[560]]) # 7352

# read the y_train.txt ('train/y_train.txt': Training labels.)
y_train <- fread(file.path(path_train, "y_train.txt"))
head(y_train)
tail(y_train)
nrow(y_train) # 7352
ncol(y_train) # 1
unique(y_train[[1]]) # 5 4 6 1 3 2 (seems to be the activityId)
colnames(y_train) <- "activityId" # rename the column

# read the subject_train.txt ('train/subject_train.txt': Each row identifies the subject who performed the activity for each window sample.)
subject_train <- fread(file.path(path_train, "subject_train.txt"))
head(subject_train)
tail(subject_train)
nrow(subject_train) # 7352
ncol(subject_train) # 1
unique(subject_train[[1]]) # 1 3 5 6 7 8 11 14 15 16 17 19 21 22 23 25 26 27 28 29 30 (seems to be the subject ID)
colnames(subject_train) <- "subjectId" # rename the column

# Grouping now the train data sets
trainSet <- cbind(subject_train, y_train, X_train) # (subjectId, activityId, featuers)
ncol(trainSet) # 563
nrow(trainSet) # 7352
head(colnames(trainSet)) # "subjectId" "activityId" "tBodyAcc-mean()-X" "tBodyAcc-mean()-Y" "tBodyAcc-mean()-Z" "tBodyAcc-std()-X"
head(trainSet[,1:5])
tail(trainSet[,1:5])

# The content of the testing set
path_test <- file.path(path_data, "test")
list.files(path_test)

# read the X_test.txt ('test/X_test.txt': Test set.)
X_test <- fread(file.path(path_test, "X_test.txt"))
head(X_test)
tail(X_test)
nrow(X_test) # 2947
ncol(X_test) # 561 (the names of these columns must be then the list of features)
colnames(X_test) <- c(features[["feature"]]) # rename the columns
length(X_test[[560]]) # 2947

# read the y_test.txt ('test/y_test.txt': Test labels.)
y_test <- fread(file.path(path_test, "y_test.txt"))
head(y_test)
tail(y_test)
nrow(y_test) # 2947
ncol(y_test) # 1
unique(y_test[[1]]) # 5 4 6 1 3 2 (seems also to be the activityId)
colnames(y_test) <- "activityId" # rename the column

# read the subject_test.txt ('test/subject_test.txt': Each row identifies the subject who performed the activity for each window sample.)
subject_test <- fread(file.path(path_test, "subject_test.txt"))
head(subject_test)
colnames(subject_test) # V1
colnames(subject_test) <- "subjectId" # rename the column
tail(subject_test)
nrow(subject_test) # 2947
ncol(subject_test) # 1
unique(subject_test[[1]]) # 2 4 9 10 12 13 18 20 24

# Grouping now the test data sets
testSet <- cbind(subject_test, y_test, X_test) # (subjectId, activityId, featuers)
ncol(testSet) # 563
nrow(testSet) # 2947
head(colnames(testSet)) # "subjectId" "activityId" "tBodyAcc-mean()-X" "tBodyAcc-mean()-Y" "tBodyAcc-mean()-Z" "tBodyAcc-std()-X"
head(testSet[,1:5])
tail(testSet[,1:5])

# Now merge the trainSet and testSet
allTrainTest <- rbind(trainSet, testSet)
ncol(allTrainTest) # 563
nrow(allTrainTest) # 10299 (= 7352 + 2947)
head(allTrainTest[,1:5])
tail(allTrainTest[,1:5])

#==============================================
# Task 2: Extracts only the measurements on the mean and standard deviation for each measurement.
#==============================================
# First, selecting the features that deal with mean and std
allFeatures <- features[["feature"]]
head(allFeatures)
length(allFeatures) # 561
meanStdFeatures <- grep("(mean|std)", allFeatures) # this gives the index of each selected feature
head(meanStdFeatures)
length(meanStdFeatures) # 79
selectedFeatures <- allFeatures[c(meanStdFeatures)] # this gives the name of each selected feature
head(selectedFeatures)
tail(selectedFeatures)
length(selectedFeatures) # 79
# Now, extracting from allTrainTest dataset the measurements with the selected features only
head(colnames(allTrainTest)) # to remember the names of the tow first columns
allTrainTestMeanStd <- allTrainTest[, c("subjectId", "activityId", ..selectedFeatures)]
head(allTrainTestMeanStd)
ncol(allTrainTestMeanStd) # 81
#==============================================
# Task 3: Uses descriptive activity names to name the activities in the data set.
#==============================================
# In this case, we will define the "activityId" column as a factor, where each level corresponds to a specific "cativityId", and then map the levels to the "activity" text labels.
allTrainTestMeanStd$activityId <- factor(allTrainTestMeanStd$activityId, 
                             levels = activity_labels$activityId, 
                             labels = activity_labels$activity)
head(allTrainTestMeanStd)

#==============================================
# Task 4: Appropriately labels the data set with descriptive variable names.
#==============================================
# This is done in task 1 when renaming the columns of train and test data sets.
head(colnames(allTrainTestMeanStd)) # result : "subjectId" "activityId" "tBodyAcc-mean()-X" "tBodyAcc-mean()-Y" "tBodyAcc-mean()-Z" "tBodyAcc-std()-X"

#==============================================
# Task 5: From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
#==============================================
# First, create a copy of allTrainTestMeanStd dataset
secondTidyMean <- copy(allTrainTestMeanStd)
head(secondTidyMean)

# Then group the data by "subjectId" then by "activityId" (which is now "activity name)
library(dplyr)
secondTidyMean_Grouped <- secondTidyMean %>%
  group_by(subjectId, activityId) %>%
  summarise( # to compute the mean values for each variable
    across(all_of(selectedFeatures), mean, .names = "average_{.col}")
  )
head(secondTidyMean_Grouped)
tail(secondTidyMean_Grouped)
ncol(secondTidyMean_Grouped) # 81
nrow(secondTidyMean_Grouped) # 180

# Now, we can write this new dataset to a .txt file
data.table::fwrite(x = secondTidyMean_Grouped, file = file.path(n_wd, "UCI HAR DATASET", "AverageBySubjectByActivity.txt"), quote = FALSE)
