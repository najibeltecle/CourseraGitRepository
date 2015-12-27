**Getting and Cleaning Data Project**

**Description**

Code book for the variables, data and transformations used in the course project for the Getting and Cleaning Data course provided on coursera.

**Source Data**

A full description of the data used in this project can be found at [The UCI Machine Learning Repository](http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones)

**Section 1. Merge the training and the test sets to create one data set.**

After setting the source directory for the files, read into tables the data located in

- ••features.txt
- ••activity\_labels.txt
- ••subject\_train.txt
- ••x\_train.txt
- ••y\_train.txt
- ••subject\_test.txt
- ••x\_test.txt
- ••y\_test.txt

Assign column names and merge to create one data set.

**Section 2. Extract only the measurements on the mean and standard deviation for each measurement.**

Create a logical vector that contains TRUE values for the ID, mean and stdev columns and FALSE values for the others. Subset this data to keep only the necessary columns.

**Section 3. Use descriptive activity names to name the activities in the data set**

Merge data subset with the activityType table to cinlude the descriptive activity names

**Section 4. Appropriately label the data set with descriptive activity names.**

Use gsub function for pattern replacement to clean up the data labels.

**Section 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject.**

Per the project instructions, we need to produce only a data set with the average of each variable for each activity and subject