# Getting and Cleaning Data 
# Course Project 
# Najib El Tecle 2015 

# Final exported data can be loaded using read.table("./finalData.txt", header =TRUE, sep=",")

# run_analysis.R

# This code will perform the following tasks: 
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names. 
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


##########################################################################################

# 0.Getting the workspace ready 

#Clean the workspace 
rm(list=ls())

#Load requiered libraries 
library(dplyr)

#Verify/set working directory 
#Please update wd to your local directory 
#Please unquote the 2 lines below if ther is any need to change the working directory
## wd <- "/Users/najibeltecle/Desktop/UCI HAR Dataset" 
## if (getwd() != wd ) setwd(wd)

##########################################################################################
# 1. Merges the training and the test sets to create one data set.

#loading the datasets 
features     = read.table('./features.txt',header=FALSE); #imports features.txt
activityType = read.table('./activity_labels.txt',header=FALSE); #imports activity_labels.txt
subjectTrain = read.table('./train/subject_train.txt',header=FALSE); #imports subject_train.txt
subjectTest = read.table('./test/subject_test.txt',header=FALSE); #imports subject_test.txt
xTrain <- read.table("./train/X_train.txt", header =FALSE) #imports x_train.txt
yTrain <- read.table("./train/y_train.txt", header =FALSE) #imports y_train.txt
xTest <- read.table("./test/X_test.txt", header =FALSE) #imports X_test.txt
yTest <- read.table("./test/y_test.txt", header =FALSE) #imports Y_test.txt

#Assigning Colum names to the imported datasets
colnames(activityType)  <- c('activityId','activityType');
colnames(subjectTrain)  <- "subjectId";
colnames(xTrain)        <- features[,2]; 
colnames(yTrain)        <- "activityId";
colnames(subjectTest) <- "subjectId";
colnames(xTest)       <- features[,2]; 
colnames(yTest)       <- "activityId";

# Getting the test dataset
testData <- cbind(yTest,subjectTest,xTest);

# Getting the training dataset
trainingData <- cbind(yTrain,subjectTrain,xTrain);

#merging datasets 
data <- rbind(trainingData,testData);
data <- tbl_df(data)

##########################################################################################
#2 Extracts only the measurements on the mean and standard deviation for each measurement. 

# Subset the data 
colNames <- colnames(data)

#Subsetting the data based on a vector of booleans generated with grepl
data = data[(grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames))]


##########################################################################################
#3 Uses descriptive activity names to name the activities in the data set

#Merging data on activity type basis 
data <- merge(data, activityType, by ="activityId", all.x= TRUE)

##########################################################################################
#4 Appropriately labels the data set with descriptive variable names. 

colNames <- colnames(data)

# Renaming colNames with descriptive names
colNames <- gsub("\\()","", colNames)
colNames <- gsub("-std","StandardDeviation", colNames)
colNames <- gsub("-mean","Mean", colNames)
colNames <- gsub("Acc","Acceleration", colNames)
colNames <- gsub("Mag","Magnitude", colNames)

# Reassigning colNames

colnames(data) <- colNames

 ##########################################################################################
#5 From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# summazrizing the final data
finalDataNoActivityType  = data[,names(data) != 'activityType'];
finalData    = aggregate(finalDataNoActivityType[,names(finalDataNoActivityType) != c('activityId','subjectId')],by=list(activityId=finalDataNoActivityType$activityId,subjectId = finalDataNoActivityType$subjectId),mean);

#Merging final Data with activity names
finalData    = merge(finalData,activityType,by='activityId',all.x=TRUE);

#Removing activityId
finalData <- select(finalData, -activityId)

# Export the data 
write.table(finalData, "./finalData.txt",row.names = TRUE, sep=",")
