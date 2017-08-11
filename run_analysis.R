###clean work directory
rm(list=ls())
#############################################################################
library(dplyr)
library(plyr)
#############################################################################
#check the existense of valid files
#download file and unzip it if not available
path <- getwd()
project.Dir <- "UCI HAR Dataset"
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zipfile <- "project.zip"
if (!file.exists(project.Dir) & (!file.exists(zipfile))) {
        download.file(fileUrl, destfile = zipfile)
        unzip(zipfile)
        
}

#############################################################################
### 1:Merges the training and the test sets to create one data set.
read_data <- function(data_file) {
        data_file <- paste("UCI HAR Dataset/",data_file,sep = "")
        table <-  read.table(data_file)
}
name_lists <- list(
        "activity_labels.txt",
        "features.txt",
        "test/subject_test.txt",
        "test/X_test.txt",
        "test/y_test.txt",
        "train/subject_train.txt",
        "train/X_train.txt",
        "train/y_train.txt"
)

data_all <- lapply(name_lists, read_data)

activity_labels <- data_all[[1]]
features  <- data_all[[2]]
subject_test <- data_all[[3]]
X_test <- data_all[[4]]
y_test <- data_all[[5]]
subject_train <- data_all[[6]]
X_train <- data_all[[7]]
y_train <- data_all[[8]]

dt <- cbind(rbind(y_train, y_test),
            rbind(subject_train, subject_test),
            rbind(X_train, X_test))
dir.create("tidyData_output")
write.table(dt,file = "tidyData_output/data_merged.txt",row.names = FALSE)
write.csv(dt,file = "tidyData_output/data_merged.csv",row.names = FALSE)

###requirement1 answer^^^^
colnames(dt) <- c("activity_ID", "subject_ID", as.character(features$V2))
dt <- dt[,!duplicated(colnames(dt))]

#clear data
rm(X_train, X_test, y_train, y_test, subject_test, subject_train)


#############################################################################
### 2:Extracts only the measurements on the mean and standard deviation for each measurement.

data_mean_std <-select(dt, activity_ID, subject_ID, contains("mean"), contains("std"))
###requirement 2 answer^^^


#############################################################################
### 3.Uses descriptive activity names to name the activities in the data set
### 4.Appropriately labels the data set with descriptive variable names.

colnames(activity_labels) <- c("activity_ID","activity")
dt_activities <- merge(dt,activity_labels,all = T)
dt_activities <- select(dt_activities, activity,everything())

write.table(dt_activities,file = "tidyData_output/tidyData.txt",row.names = FALSE)
write.csv(dt_activities,file = "tidyData_output/tidyData.csv",row.names = FALSE)

### 3,4 answer ^^^

#############################################################################
### 5.From the data set in step 4, creates a second, independent tidy data set 
###with the average of each variable for each activity and each subject.
df_average <- ddply(dt_activities, c("subject_ID","activity"), numcolwise(mean))

write.table(df_average,file = "tidyData_output/average.txt",row.names = FALSE)
write.csv(df_average,file = "tidyData_output/average.csv",row.names = FALSE)

