## The working directory should be a folder which contains this file as well as
## containing the folder "UCI HAR Dataset" which includes all the data files 
## provided.

## In order for this script to run, the packages "dplyr" and "tidyr" must be 
## installed.

library(dplyr)
library(tidyr)

## Function to rename the entries in the Activity column so that they're human-
## readable.
## This is part of STEP 3.
rename_activity <- function(x){
  if (x == 1) {output = "WALKING"}
  if (x == 2) {output = "WALKING_UPSTAIRS"}
  if (x == 3) {output = "WALKING_DOWNSTAIRS"}
  if (x == 4) {output = "SITTING"}
  if (x == 5) {output = "STANDING"}
  if (x == 6) {output = "LAYING"}
  output
}

## Read in the data, and arrange it so that the first column is the activity
## being performed (from y_test.txt and y_train.txt), then the next column is the
## subject number, and then finally the columns of the experimental data.
## Do this for both the test data and the training data.  This is the beginning
## of STEP 1.

## The apply(testlables, 1, rename_activity) and apply(trainlabels, 1, 
## rename_activity) commands complete STEP 3 of the assignment, by renaming the
## activity labels from numbers to descriptive names.

testdata <- read.table("./UCI HAR Dataset/test/X_test.txt", 
                       colClasses = "numeric", comment.char = "")
testlabels <- read.table("./UCI HAR Dataset/test/y_test.txt", 
                         colClasses = "integer", comment.char = "")
testlabels <- apply(testlabels, 1, rename_activity)
testsubjects <- read.table("./UCI HAR Dataset/test/subject_test.txt", 
                           colClasses = "integer", comment.char = "")
newtestdata <- cbind(testlabels, testsubjects, testdata)

traindata <- read.table("./UCI HAR Dataset/train/X_train.txt", 
                        colClasses = "numeric", comment.char = "")
trainlabels <- read.table("./UCI HAR Dataset/train/y_train.txt", 
                          colClasses = "integer", comment.char = "")
trainlabels <- apply(trainlabels, 1, rename_activity)
trainsubjects <- read.table("./UCI HAR Dataset/train/subject_train.txt", 
                            colClasses = "integer", comment.char = "")
newtraindata <- cbind(trainlabels, trainsubjects, traindata)

## Read in the names of each measurement type, and add them in as column labels
## for both the test data and the training data.  There are a few duplicate 
## labels for the measurement types, so make them unique.  Adding the "Activity"
## and "Subject" labels is STEP 4.

activitydata <- t(read.table("./UCI HAR Dataset/features.txt"))
collabels <- c("Activity", "Subject", activitydata[2,])
collabels <- make.unique(collabels)
names(newtestdata) <- collabels
names(newtraindata) <- collabels

## Merge the two data sets, and assign the merged data to a dplyr data set.
## This completes STEP 1
fulldata <- tbl_df(rbind(newtestdata, newtraindata))

## Clean up the environment by deleting the intermediate data sets.
rm(activitydata, newtestdata, newtraindata, testdata, testsubjects, traindata,
   trainsubjects, collabels, testlabels, trainlabels)

## Create a data set of just the mean and std values, as well as Activity and 
## Subject.  This is STEP 2.

smalldata <- select(fulldata, Activity, Subject, contains("mean"), 
                    contains("std"))

## Create the tidy data set (STEP 5).  First, group the smalldata table by Activity
## and Subject, then take the mean of each column within each group.  Finally, 
## remove duplicate elements, since this process will create an identical entry
## with the means of each measurement for each original observation of each 
## subject-activity pair.

tidydata <- smalldata %>%
  group_by(Subject, Activity) %>%
  mutate_each(funs(mean)) %>%
  unique() %>%
  arrange(Subject, Activity)
  
## Saves data to tidydata.txt
write.table(tidydata, "./tidydata.txt", sep="\t", row.name = FALSE)