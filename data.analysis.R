    ##sets working directory to appropriate locale
    setwd("C:/Users/Jim/Documents/Data Science/Getting and cleaning Data/UCI HAR Dataset")
    
    ##reads and strips features.txt to a single dataframe
    names <- read.table("features.txt")
    names <- names[,2]
    
    #############################################################################
    ## 1. Merges the training and the test sets to create one data set.         #
    #############################################################################
    
    ##Reads and binds test files together
    subject_test <- read.table("test/subject_test.txt")
    x_test <- read.table("test/x_test.txt")
    y_test <- read.table("test/y_test.txt")
    activityType = read.table('activity_labels.txt',header=FALSE); #imports activity_labels.txt
    
    ##Reads and bind training data together
    subject_train <- read.table("train/subject_train.txt")
    x_train <- read.table("train/x_train.txt")
    y_train <- read.table("train/y_train.txt")
    
    ##binds train and test files together
    subject_test <- rbind(subject_test, subject_train)
    x_test <- rbind(x_test, x_train)
    y_test <- rbind(y_test, y_train)
    
    ##adds column names to x test from features
    colnames(x_test) <- names
    
    ##Column names for other variables
    colnames(subject_test) <- "Subject ID"
    colnames(y_test) <- "Activity ID"
    colnames(activityType) = c("Activity ID", "Activity")
    
    ##Binds data together
    bounddata <- cbind(subject_test, y_test, x_test)
    
    ##############################################################################################
    ## 2. Extracts only the measurements on the mean and standard deviation for each measurement.#
    ##############################################################################################
    
    bounddata_meanstd <- bounddata[, grepl ("mean|std|Subject ID|Activity ID", names(bounddata))]
    
    #############################################################################
    ## 3. Uses descriptive activity names to name the activities in the data set#
    ## merges data with activity type to provide descriptive labels.            #
    #############################################################################
    
    finaldata <- merge(bounddata_meanstd, activityType, by="Activity ID", all.x=TRUE)
    
    #############################################################################
    ## 4. Appropriately labels the data set with descriptive variable names.   ##
    #############################################################################
    
    ##Removes parentheses
    names(finaldata) <- gsub('\\(|\\)', "", names(finaldata))
    
    ##Makes names syntax
    names(finaldata) <- make.names(names(finaldata))
    
    ##Clarifies and Cleans Column Names
    names(finaldata) <- gsub('^t', "TIME.", names(finaldata))
    names(finaldata) <- gsub('^f', "FREQUENCY.", names(finaldata))
    names(finaldata) <- gsub("Acc", "Acceleration", names(finaldata))
    names(finaldata) <- gsub("Freq", "Frequency", names(finaldata))
    names(finaldata) <- gsub("Mag", "Magnitude", names(finaldata))
    names(finaldata) <- gsub("std", "Standard.Deviation", names(finaldata))
    names(finaldata) <- gsub("[Bb]ody", "Body.", names(finaldata))
    names(finaldata) <- gsub("Body.Body", "Body", names(finaldata))
    names(finaldata) <- gsub("Gyro", "Gyroscopic", names(finaldata))
    names(finaldata) <- gsub("mean", "Mean", names(finaldata))
    
    ###########################################################################################################################################################
    ## 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.     ##
    ###########################################################################################################################################################
    
    ##Removes alphanumeric activity ID column
    finaldata <- finaldata[, names(finaldata) !="Activity.ID"]
    
    ##averages relevant columns for activities
    tidydata <- lapply(finaldata[, 2:80], mean)
   
    ##Writes the final csv
    write.csv(tidydata, file="tidydata.csv", row.names=TRUE)
