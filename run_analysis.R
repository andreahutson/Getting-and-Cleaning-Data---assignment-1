# setwd("/Users/andreahutson/Dropbox/Classes/Getting and Cleaning Data Course/assignment1/")

# Note: be sure to install reshape2 into R before running this script, and make sure the 
# working directory is set to the correct location.

# Load the data
    
    # Get the names of each column from features.txt
        features <- read.table("features.txt", sep=" ", comment.char="")
        
      
    # Get the activity labels from activity_labels.txt
        activities <- read.table("activity_labels.txt", sep=" ", comment.char="")

    # Get the test data
        testSubject <- read.table("./test/subject_test.txt", sep=" ", comment.char="")
        testX <-  read.csv("./test/X_test.csv", header=FALSE)
        testY <- read.table("./test/Y_test.txt", sep=" ", comment.char="")

    # Get the training data
        trainSubject <- read.table("./train/subject_train.txt", sep=" ", comment.char="")
        trainX <-  read.csv("./train/X_train.csv", header=FALSE)
        trainY <- read.table("./train/Y_train.txt", sep=" ", comment.char="")

# Label columns
    
    # Label testX and trainX with feature labels
        colnames(testX) <- features$V2
        colnames(trainX) <- features$V2

    # Label subject
        names(testSubject) <- "SubjectID"
        names(trainSubject) <- "SubjectID"

    # Label testy
        names(testY) <- "ActivityCode"
        names(trainY) <- "ActivityCode"

    # Label activity
        names(activities) <- c("ActivityCode", "Activity")


#  Filter data to only include means and standard deviations

    # We only need columns for mean and standard deviation for each measurement in the X dataset.
    # Let's label the columns, and then filter to get only the columns we need.



    # Find columns in the 'features' data frame (e.g., the frame that contains the column names)
    # that contain the string "mean" or "std" for standard deviation. These are the only columns we need of 561.

       meanColumns <- grep("mean()", features$V2, fixed=TRUE) 
       stdColumns <- grep("std()", features$V2, fixed=TRUE)
        
    # Now, append these two arrays together, to make one array with all of the needed values
       meanAndStd <- append(meanColumns, stdColumns)
    
    # Subset the data with mean and standard deviation only; 79 columns remain
        testX2 <- testX[, meanAndStd]
        trainX2 <- trainX[, meanAndStd]

# Bind the columns together, bind the rows together, and then merge activity code and label.

    # First, we'll bind the test sets with cbind
    test <- cbind (testSubject, testY, testX2)
    train <- cbind(trainSubject, trainY, trainX2)

    # Now, we'll add a variable that names each set as "test" or "train" 
    test$type <- "TEST"
    train$type <- "TRAIN"
    
    # now, we'll append the rows together
    testAndTrain <- rbind (test, train)
 
    #Finally, we'll merge the activity code and label together, so the column will be labeled descriptively.

     Final <- merge(activities, testAndTrain, by="ActivityCode")
    
   # Remove the first column (activity code) 
     Final <- Final [, 2:70]

    # Sends this output into a new file
    write.csv(Final, "./product/all-data.csv")


# Create summaries of means for each variable by activity and subject id.

    library(reshape2) # We'll be making a summary by using the melt function in the reshape2 library
    gyroMelt <- melt(Final, id=c("Activity", "SubjectID"), # the id variables are Activity and Subject, 
            measure.vars=names(Final[3:68]))  # the measured variables are all of the variables from 3 to 68

    # Find means for each of the variables by activity/subject, save them to a new data frame
    AllMean <- dcast(gyroMelt, SubjectID + Activity ~ variable, mean)

# Write to CSV
    write.csv(AllMean, "./product/all-means-by-activity-and-subject.csv", row.names=FALSE)

    
