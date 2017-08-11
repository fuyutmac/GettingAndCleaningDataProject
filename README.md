# README



## Introduction
The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set.


## Review criteria

- The submitted data set is tidy.
- The Github repo contains the required scripts.
- GitHub contains a code book that modifies and updates the available codebooks with the data to indicate all the variables and summaries calculated, along with units, and any other relevant information.
- The README that explains the analysis files is clear and understandable.
- The work submitted for this project is the work of the student who submitted it.

## Getting and Cleaning Data Course Project

The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis. You will be graded by your peers on a series of yes/no questions related to the project. You will be required to submit: 
1) a tidy data set as described below 
2) a link to a Github repository with your script for performing the analysis, and 
3) a code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md. 
4) You should also include a README.md in the repo with your scripts. This repo explains how all of the scripts work and how they are connected.

One of the most exciting areas in all of data science right now is wearable computing - see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained:

[http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones](http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones)

Here are the data for the project:

[https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip](https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip)

You should create one R script called run_analysis.R that does the following.



```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```
## -------------------------------------------------------------------------
```

```
## You have loaded plyr after dplyr - this is likely to cause problems.
## If you need functions from both plyr and dplyr, please load plyr first, then dplyr:
## library(plyr); library(dplyr)
```

```
## -------------------------------------------------------------------------
```

```
## 
## Attaching package: 'plyr'
```

```
## The following objects are masked from 'package:dplyr':
## 
##     arrange, count, desc, failwith, id, mutate, rename, summarise,
##     summarize
```
Following only show the first 6 rows and 10 columns of each data

1. Merges the training and the test sets to create one data set.


```r
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


###requirement1 answer^^^^
colnames(dt) <- c("activity_ID", "subject_ID", as.character(features$V2))
head(dt[,1:10])
```

```
##   activity_ID subject_ID tBodyAcc-mean()-X tBodyAcc-mean()-Y
## 1           5          1         0.2885845       -0.02029417
## 2           5          1         0.2784188       -0.01641057
## 3           5          1         0.2796531       -0.01946716
## 4           5          1         0.2791739       -0.02620065
## 5           5          1         0.2766288       -0.01656965
## 6           5          1         0.2771988       -0.01009785
##   tBodyAcc-mean()-Z tBodyAcc-std()-X tBodyAcc-std()-Y tBodyAcc-std()-Z
## 1        -0.1329051       -0.9952786       -0.9831106       -0.9135264
## 2        -0.1235202       -0.9982453       -0.9753002       -0.9603220
## 3        -0.1134617       -0.9953796       -0.9671870       -0.9789440
## 4        -0.1232826       -0.9960915       -0.9834027       -0.9906751
## 5        -0.1153619       -0.9981386       -0.9808173       -0.9904816
## 6        -0.1051373       -0.9973350       -0.9904868       -0.9954200
##   tBodyAcc-mad()-X tBodyAcc-mad()-Y
## 1       -0.9951121       -0.9831846
## 2       -0.9988072       -0.9749144
## 3       -0.9965199       -0.9636684
## 4       -0.9970995       -0.9827498
## 5       -0.9983211       -0.9796719
## 6       -0.9976274       -0.9902177
```

2. Extracts only the measurements on the mean and standard deviation for each measurement.

```r
dt <- dt[,!duplicated(colnames(dt))]

#clear data
rm(X_train, X_test, y_train, y_test, subject_test, subject_train)


#############################################################################
### 2:Extracts only the measurements on the mean and standard deviation for each measurement.

data_mean_std <-select(dt, activity_ID, subject_ID, contains("mean"), contains("std"))
###requirement 2 answer^^^
head(data_mean_std[,1:10])
```

```
##   activity_ID subject_ID tBodyAcc-mean()-X tBodyAcc-mean()-Y
## 1           5          1         0.2885845       -0.02029417
## 2           5          1         0.2784188       -0.01641057
## 3           5          1         0.2796531       -0.01946716
## 4           5          1         0.2791739       -0.02620065
## 5           5          1         0.2766288       -0.01656965
## 6           5          1         0.2771988       -0.01009785
##   tBodyAcc-mean()-Z tGravityAcc-mean()-X tGravityAcc-mean()-Y
## 1        -0.1329051            0.9633961           -0.1408397
## 2        -0.1235202            0.9665611           -0.1415513
## 3        -0.1134617            0.9668781           -0.1420098
## 4        -0.1232826            0.9676152           -0.1439765
## 5        -0.1153619            0.9682244           -0.1487502
## 6        -0.1051373            0.9679482           -0.1482100
##   tGravityAcc-mean()-Z tBodyAccJerk-mean()-X tBodyAccJerk-mean()-Y
## 1           0.11537494            0.07799634           0.005000803
## 2           0.10937881            0.07400671           0.005771104
## 3           0.10188392            0.07363596           0.003104037
## 4           0.09985014            0.07732061           0.020057642
## 5           0.09448590            0.07344436           0.019121574
## 6           0.09190972            0.07793244           0.018684046
```

3. Uses descriptive activity names to name the activities in the data set
4. Appropriately labels the data set with descriptive variable names.


```r
### 3.Uses descriptive activity names to name the activities in the data set
### 4.Appropriately labels the data set with descriptive variable names.

colnames(activity_labels) <- c("activity_ID","activity")
dt_activities <- merge(dt,activity_labels,all = T)
dt_activities <- select(dt_activities, activity,everything())
head(dt_activities[,1:10])
```

```
##   activity activity_ID subject_ID tBodyAcc-mean()-X tBodyAcc-mean()-Y
## 1  WALKING           1          7         0.3016485      -0.026883636
## 2  WALKING           1          5         0.3433592      -0.003426473
## 3  WALKING           1          6         0.2696745       0.010907280
## 4  WALKING           1         23         0.2681938      -0.012730069
## 5  WALKING           1          7         0.3141912      -0.008695973
## 6  WALKING           1          7         0.2032763      -0.009764083
##   tBodyAcc-mean()-Z tBodyAcc-std()-X tBodyAcc-std()-Y tBodyAcc-std()-Z
## 1       -0.09579580       -0.3801243       -0.1913292       0.34055774
## 2       -0.10154465       -0.2011536        0.1331536      -0.31817123
## 3       -0.07494859       -0.3366399        0.1462498      -0.44557070
## 4       -0.09365263       -0.3836978       -0.2038974       0.14800031
## 5       -0.12456099       -0.3558778       -0.1657995       0.40672936
## 6       -0.15139663       -0.4286661       -0.2610000       0.07675962
##   tBodyAcc-mad()-X
## 1       -0.3747341
## 2       -0.2381466
## 3       -0.4100551
## 4       -0.4296036
## 5       -0.3493229
## 6       -0.4659763
```

5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


```r
### 5.From the data set in step 4, creates a second, independent tidy data set 
###with the average of each variable for each activity and each subject.
df_average <- ddply(dt_activities, c("subject_ID","activity"), numcolwise(mean))
head(df_average[,1:10])
```

```
##   subject_ID           activity activity_ID tBodyAcc-mean()-X
## 1          1             LAYING           6         0.2215982
## 2          1            SITTING           4         0.2612376
## 3          1           STANDING           5         0.2789176
## 4          1            WALKING           1         0.2773308
## 5          1 WALKING_DOWNSTAIRS           3         0.2891883
## 6          1   WALKING_UPSTAIRS           2         0.2554617
##   tBodyAcc-mean()-Y tBodyAcc-mean()-Z tBodyAcc-std()-X tBodyAcc-std()-Y
## 1      -0.040513953        -0.1132036      -0.92805647     -0.836827406
## 2      -0.001308288        -0.1045442      -0.97722901     -0.922618642
## 3      -0.016137590        -0.1106018      -0.99575990     -0.973190056
## 4      -0.017383819        -0.1111481      -0.28374026      0.114461337
## 5      -0.009918505        -0.1075662       0.03003534     -0.031935943
## 6      -0.023953149        -0.0973020      -0.35470803     -0.002320265
##   tBodyAcc-std()-Z tBodyAcc-mad()-X
## 1      -0.82606140      -0.93211117
## 2      -0.93958629      -0.97951458
## 3      -0.97977588      -0.99614135
## 4      -0.26002790      -0.34069902
## 5      -0.23043421      -0.04411722
## 6      -0.01947924      -0.40283278
```





# GettingAndCleaningDataProject
