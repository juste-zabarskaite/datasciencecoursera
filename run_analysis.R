#
# Getting and Cleaning Data - Week 4 Assignment
#
## The purpose of the script is to clean the data for further analysis
## Input Data source: http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
## Input Data files: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
## Output: data set with averages of (i) selected mean & std variables for (ii) each activity and (iii) each subject
## Code in console: execution()

execution <- function() {
    
    source <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    destfile = "./dataset.zip"
    
    downloadinput() ## download source files
    data <- bindtables() ## read & bind data tables
    data <- fixvariables(data) ## rename activity & other variable names to make them descriptive
    data <- organiseoutput(data) ## return organised output with selected variables
    saveoutput(data) ## save output as csv file
    return(data)
}

## download source files
downloadinput <- function() {
 
    ## download zip 
    if(!file.exists(destfile)) { 
        download.file(source,destfile) 
    }
    
    ## unzip file
    unzip(destfile, files=NULL, list=FALSE, overwrite=TRUE, junkpaths=FALSE, exdir=".", unzip="internal", setTimes=FALSE)
}    

## read & bind data tables
bindtables <- function() {
    
    ## read 'test/X_test.txt': Test set.
    filename <- "UCI HAR Dataset/test/X_test.txt"
    dataTest <- read.table(filename) ## 2947 observations ## 561 variables
    
    ## read 'test/y_test.txt': Test labels.
    filename <- "UCI HAR Dataset/test/y_test.txt"
    labelTest <- read.table(filename) ## 2947 observations ## 1 variable (Activity)
    
    ## read 'test/subject_test.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 
    filename <- "UCI HAR Dataset/test/subject_test.txt"
    subjectTest <- read.table(filename) ## 2947 observations, 1 variable (Subject)
    
    ## bind Test variables together
    library(dplyr)
    labelTest<- rename(labelTest, Activity=V1) ## rename V1 variable as Activity
    subjectTest<- rename(subjectTest, Subject=V1) ## rename V1 variable as Subject
    dataTest <- cbind(labelTest, subjectTest, dataTest) ## bind data with Activity & Subject
    
    ## read 'train/X_train.txt': Training set.
    filename <- "UCI HAR Dataset/train/X_train.txt"
    dataTrain <- read.table(filename) ## 7352 observations ## 561 variables
    
    ## read 'train/y_train.txt': Training labels.
    filename <- "UCI HAR Dataset/train/y_train.txt"
    labelTrain <- read.table(filename) ## 7352 observations, 1 variable (Activity)
    
    ## read 'train/subject_train.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 
    filename <- "UCI HAR Dataset/train/subject_train.txt"
    subjectTrain <- read.table(filename) ## 7352 observations, 1 variable (Subject)
    
    ## bind Training variables together
    library(dplyr)
    labelTrain<- rename(labelTrain, Activity=V1) ## rename V1 variable as Activity
    subjectTrain<- rename(subjectTrain, Subject=V1) ## rename V1 variable as Subject
    dataTrain <- cbind(labelTrain, subjectTrain, dataTrain) ## bind data with Activity & Subject
    
    ## merge Test & Training data sets
    data <- rbind(dataTest, dataTrain)
    
    return(data)   
}    

## rename activity & other variable names to make them descriptive
fixvariables <- function(data) {
    
    ## add descriptive activity names
    file <- paste0(path, "/activity_labels.txt") ## 'activity_labels.txt': Links the class labels with their activity name.
    labelAct <- read.table(file) ## 6 activities
    data$Activity <- factor(data$Activity, levels=labelAct[,1], labels=labelAct[,2]) ## add Activity names
    
    ## add descriptive variable names
    file <- paste0(path, "/features.txt") ## 'features.txt': List of all features.
    labelVar <- read.table(file) ## 561 variables
    colnames(data)[] <- c("Activity", "Subject", as.character(labelVar[,2])) ## add variable names
    
    return(data)
}    

## return organised output with selected variables
organiseoutput <- function(data) {

    ## keep only means & standard deviations in the set
    cols <- grep("^Activity$|^Subject$|mean\\(\\)|std\\(\\)",names(data), value=TRUE)   ## find relevant columns + Activity & Subject
    data <- data[,cols] ## drop non-relevant columns
    
    ## create data set with the average of (i) each variable for (ii) each activity and (iii) each subject
    data <-aggregate(data[,!(names(data) %in% c("Activity","Subject"))], by=list(Activity=data$Activity, Subject=data$Subject), FUN=mean, na.rm=TRUE)
    
    return(data)    
}

## save output as csv file
saveoutput <- function(data) {
    
    ## save file as cvs
    write.csv(data, file="output.csv")
}