####################################################
## Johns Hopkins' Getting and Cleanning Data project
####################################################

## Clears workspace
rm(list = ls())

## Getting working data file 

fileUrl  <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
destFile <- "UCI HAR Dataset.zip"
dataDir <- "UCI HAR Dataset"

if (!file.exists(dataDir)){
  #dir.create(dataDir) ## unzip already creates the directory
  download.file(fileUrl, destfile = destFile, method = "auto")
  unzip(destFile)
}

## Step 1: To merge the training and the test sets to create one data set

## Gets features and activity labels names
features <- read.table(paste0(dataDir, "\\features.txt"), 
                       col.names=c("id", "featName"))
labels <- read.table(paste0(dataDir, "\\activity_labels.txt"), 
                     col.names=c("id", "labelName"))

## Reads training data sets
inputData_train <- read.table(paste0(dataDir, "\\train\\X_train.txt"),
                              col.names=features$featName)
outputData_train <- read.table(paste0(dataDir, "\\train\\y_train.txt"),
                               col.names=c("idLabel"))
subjectData_train <- read.table(paste0(dataDir, "\\train\\subject_train.txt"),
                                col.names=c("idSuject"))

## Reads test data sets
inputData_test <- read.table(paste0(dataDir, "\\test\\X_test.txt"), 
                             col.names=features$featName)
outputData_test <- read.table(paste0(dataDir, "\\test\\y_test.txt"),
                              col.names=c("idLabel"))
subjectData_test <- read.table(paste0(dataDir, "\\test\\subject_test.txt"),
                               col.names=c("idSuject"))

## Merges training and test data sets into one data set
inputData_merged <- rbind(inputData_train, inputData_test)
outputData_merged <- rbind(outputData_train, outputData_test)
subjectData_merged <- rbind(subjectData_train, subjectData_test)


## Step 2: Extract mean and standard deviation for each measurement

# Gets rid of 'meanFreq()' type features
inputData_merged_meanStd <- inputData_merged[,grep("mean\\(\\)|std\\(\\)", 
                                                   features$featName)]

## Step 3: Use descriptive activity names to name the activities in the data set

# Turns activity labels to a more nice form
labels[,2] <- tolower(labels[,2])
labels[,2] <- gsub("(_.)", "\\U\\1", labels[,2], perl=TRUE)
labels[,2] <- gsub("_", "", labels[,2])

# Replaces numeric index by activity descriptions in output data
outputData_merged[,1] <- labels[outputData_merged[,1], 2]


## Step 4: Appropriately label the data set with descriptive variable names

names(subjectData_merged) <- "subject"
names(outputData_merged) <- "activity"
names(inputData_merged_meanStd) <- gsub("\\(\\)", "", names(inputData_merged_meanStd))
names(inputData_merged_meanStd) <- gsub("mean", "Mean", names(inputData_merged_meanStd))
names(inputData_merged_meanStd) <- gsub("std", "Std", names(inputData_merged_meanStd))
names(inputData_merged_meanStd) <- gsub("-", "", names(inputData_merged_meanStd))

# Creates the tidy merged data set
tidyDataSet <- cbind(subjectData_merged, outputData_merged, inputData_merged_meanStd)
write.table(tidyDataSet, "merged_MeanStd.txt")


## Step 5: Creates a second, independent tidy data set with the average 
##         of each variable for each activity and each subject

library(reshape2)
meltData <- melt(tidyDataSet, id.vars=c("activity", "subject"))
meltMeans <- dcast(meltData, subject + activity ~ variable, fun.aggregate=mean)

write.table(meltMeans, file="tidy_MeltMeans.txt", row.names=FALSE)




