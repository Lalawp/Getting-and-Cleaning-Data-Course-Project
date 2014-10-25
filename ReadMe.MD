#Getting and Cleaning Data

Jonathan Stone

##Overview 

This project collects and analyses a data set from the UCI Machine Learning Repository. [The source data for this project can be found here.](https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip)

##Modifications

To run this, you will have to set your working directory to the root directory containing the UCI research dataset. The working directory is set on line 4 of run_analysis.R

##The Project
The following are the instructions followed to complete this project:

You should create one R script called run_analysis.R that does the following. 
1. Merges the training and the test sets to create one data set.
2. Extracts only the measurements on the mean and standard deviation for each measurement. 
3. Uses descriptive activity names to name the activities in the data set
4. Appropriately labels the data set with descriptive activity names. 
5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

##The Transformations
Lines 12-37 merge the training and test sets together, and then bind those columns together to form one data set.

Line 43 Removes all variables, excluding the Subject ID, Activity ID, Activity, and all variables which involve either a mean or standard deviation calculation. 

Line 50 merges the dataset with the activity names to provide descriptive names

Lines 57-72 Clean and clarify column variable names. 

Lines 79 removes the alphanumeric key for activity variables.

Line 82 calculate the mean of the variables for the tidy data set.

Line 85 writes the final tidy data set to a csv file. 

Further information about the variables can be found in the CodeBook.MD file. 

