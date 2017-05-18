## Load library
library(dplyr)
library(data.table)

## Read metadata, this data this in function in 3 step.
aLabels <- read.table("UCI HAR Dataset/activity_labels.txt", header = FALSE)
fNames <- read.table("UCI HAR Dataset/features.txt")

## Read training data
feaTrain <- read.table("UCI HAR Dataset/train/X_train.txt", header = FALSE)
actTrain<- read.table("UCI HAR Dataset/train/y_train.txt", header = FALSE)
subTrain <- read.table("UCI HAR Dataset/train/subject_train.txt", header = FALSE)


## Read test data
feaTest <- read.table("UCI HAR Dataset/test/X_test.txt", header = FALSE)
actTest <- read.table("UCI HAR Dataset/test/y_test.txt", header = FALSE)
subTest <- read.table("UCI HAR Dataset/test/subject_test.txt", header = FALSE)

## 1. Merges the training and the test sets to create one data set.
subject <- rbind(subTrain, subTest)
activity <- rbind(activityTrain, actTest)
features <- rbind(feaTrain, feaTest)
colnames(features) <- t(fNames[2])
colnames(activity) <- "Activity"
colnames(subject) <- "Subject"
completeData <- cbind(features,activity,subject)

## 2. Extracts only the measurements on the mean and standard deviation for each measurement.
colWSTD <- grep(".*Mean.*|.*Std.*", names(completeData), ignore.case=TRUE)
reqCols <- c(colWSTD, 562, 563)
masterData <- completeData[,reqCols]

## 3. Uses descriptive activity names to name the activities in the data set, is 1 to 6 cicle for the 6 files reading .
masterData$Activity <- as.character(masterData$Activity)
for (i in 1:6){
  masterData$Activity[masterData$Activity == i] <- as.character(aLabels[i,2])
}
masterData$Activity <- as.factor(masterData$Activity)

## 4. Appropriately labels the data set with descriptive variable names.
names(masterData)<-gsub("^t", "Time", names(masterData), ignore.case = TRUE)
names(masterData)<-gsub("^f", "Frequency", names(masterData), ignore.case = TRUE)
names(masterData)<-gsub("-mean()", "Mean", names(masterData), ignore.case = TRUE)
names(masterData)<-gsub("-std()", "Std", names(masterData), ignore.case = TRUE)
names(masterData)<-gsub("-freq()", "Frequency", names(masterData), ignore.case = TRUE)
names(masterData)<-gsub("[()-]", "", names(masterData), ignore.case = TRUE)

## 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject
tidy <- aggregate(masterData, by=list(activity = masterData$Activity, subject=masterData$Subject), mean)
write.table(tidy, file = "tidy.txt")
