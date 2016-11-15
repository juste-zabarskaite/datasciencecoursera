#
# Getting and Cleaning Data - Week 4 Assignment
#
## The purpose of the script is to clean the data for further analysis
## Input Data source: http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
## Input Data files: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
## Output: data set with averages of (i) selected mean & std variables for (ii) each activity and (iii) each subject

cleandata <- function() {
    
    ## set file directory
    path <- "./UCI HAR Dataset"
    
    ## read & bind Test data
    file <- paste0(path, "/test/X_test.txt") ## 'test/X_test.txt': Test set.
    dataTest <- read.table(file) ## 2947 observations ## 561 variables
    file <- paste0(path, "/test/y_test.txt")  ## 'test/y_test.txt': Test labels.
    labelTest <- read.table(file) ## 2947 observations ## 1 variable
    file <- paste0(path, "/test/subject_test.txt") ## 'test/subject_test.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 
    subjectTest <- read.table(file) ## 2947 observations, 1 variable (Subject)
    library(dplyr)
    labelTest<- rename(labelTest, Activity=V1) ## rename V1 variable as Activity
    subjectTest<- rename(subjectTest, Subject=V1) ## rename V1 variable as Subject
    dataTest <- cbind(labelTest, subjectTest, dataTest) ## bind data with Activity & Subject
    
    ## read & bind Training data
    file <- paste0(path, "/train/X_train.txt") ## 'train/X_train.txt': Training set.
    dataTrain <- read.table(file) ## 7352 observations ## 561 variables
    file <- paste0(path, "/train/y_train.txt") ## 'train/y_train.txt': Training labels.
    labelTrain <- read.table(file) ## 7352 observations, 1 variable (Acitity)
    file <- paste0(path, "/train/subject_train.txt") ## 'train/subject_train.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 
    subjectTrain <- read.table(file) ## 7352 observations, 1 variable (Subject)
    library(dplyr)
    labelTrain<- rename(labelTrain, Activity=V1) ## rename V1 variable as Activity
    subjectTrain<- rename(subjectTrain, Subject=V1) ## rename V1 variable as Subject
    dataTrain <- cbind(labelTrain, subjectTrain, dataTrain) ## bind data with Activity & Subject
    
    ## merge Test & Training data sets
    data <- rbind(dataTest, dataTrain)
    
    ## add descriptive activity names
    file <- paste0(path, "/activity_labels.txt") ## 'activity_labels.txt': Links the class labels with their activity name.
    labelAct <- read.table(file) ## 6 activities
    data$Activity <- factor(data$Activity, levels=labelAct[,1], labels=labelAct[,2]) ## add Activity names
    
    ## add descriptive variable names
    file <- paste0(path, "/features.txt") ## 'features.txt': List of all features.
    labelVar <- read.table(file) ## 561 variables
    colnames(data)[] <- c("Activity", "Subject", as.character(labelVar[,2])) ## add variable names
    
    ## keep only means & standard deviations in the set
    cols <- grep("^Activity$|^Subject$|mean\\(\\)|std\\(\\)",names(data), value=TRUE)   ## find relevant columns + Activity & Subject
    data <- data[,cols] ## drop non-relevant columns
    
    ## create data set with the average of (i) each variable for (ii) each activity and (iii) each subject
    aggdata <-aggregate(data[,!(names(data) %in% c("Activity","Subject"))], by=list(Activity=data$Activity, Subject=data$Subject), FUN=mean, na.rm=TRUE)
    aggdata
}    