### Emily Daniels 
### March 5, 2020
### Getting and Cleaning Data: Course Project  


# You should create one R script called run_analysis.R that does the following.

# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of 
#    each variable for each activity and each subject.

######################################################### DATA ACQUISITION #####################################################################

# Get Working Directory 

getwd() ## "C:/Users/emily.e.daniels/Documents/R/Assignments/Programming Assignment 4/UCIHAR/UCI HAR Dataset"

# Load required/potentially required packages 

library(dplyr)
library(readr)
library(utils)
library(reshape2)

# Load FEATURES data

feat <- read.table("C:/Users/emily.e.daniels/Documents/R/Assignments/Programming Assignment 4/UCIHAR/UCI HAR Dataset/features.txt", header = FALSE)
feat_desc <- as.character(feat[,2]) 

# Load ACTIVITY data

activity <- read.table("C:/Users/emily.e.daniels/Documents/R/Assignments/Programming Assignment 4/UCIHAR/UCI HAR Dataset/activity_labels.txt")
activity_labels <- as.character(activity[, 2])  
activity_num <- activity[, 1]

# Load TRAINING data

x_train <- read.table("C:/Users/emily.e.daniels/Documents/R/Assignments/Programming Assignment 4/UCIHAR/UCI HAR Dataset/train/x_train.txt", header = FALSE)
y_train <- read.table("C:/Users/emily.e.daniels/Documents/R/Assignments/Programming Assignment 4/UCIHAR/UCI HAR Dataset/train/y_train.txt", header = FALSE)
sub_train <- read.table("C:/Users/emily.e.daniels/Documents/R/Assignments/Programming Assignment 4/UCIHAR/UCI HAR Dataset/train/subject_train.txt", header = FALSE)
colnames(x_train) = feat_desc
colnames(y_train) = c("Activity")
colnames(sub_train) = c("Subjects")

# Merge TRAINING data

alltrain <- cbind(x_train, y_train, sub_train)

# Load TESTING data 

x_test <- read.table("C:/Users/emily.e.daniels/Documents/R/Assignments/Programming Assignment 4/UCIHAR/UCI HAR Dataset/test/X_test.txt", header = FALSE)
y_test <- read.table("C:/Users/emily.e.daniels/Documents/R/Assignments/Programming Assignment 4/UCIHAR/UCI HAR Dataset/test/y_test.txt", header = FALSE)
sub_test <- read.table("C:/Users/emily.e.daniels/Documents/R/Assignments/Programming Assignment 4/UCIHAR/UCI HAR Dataset/test/subject_test.txt", header = FALSE)
colnames(x_test) = feat_desc
colnames(y_test) = activity_data = c("Activity")
colnames(sub_test) = c("Subjects")

# Merge TESTING data

alltest <- cbind(x_test, y_test, sub_test)

# Merge TRAINING and TESTING data

traintest <- rbind(alltrain, alltest)

############################################################## DATA MANIPULATION #################################################################################

# Remove duplicate rows (NOTE: Some feature labels are duplicated verbatim and aren't necessary for analysis therefore they were removed)

keep1 <- traintest[, 1:304]
keep2 <- traintest[ , 347:383]
keep3 <- traintest[ , 426:462]
keep4 <- traintest[ , 505:563] 

    keepdata <- as.data.frame(cbind(keep1, keep2, keep3, keep4)) 

# Change activity column labels from numbers to text

keepdata$Activity <- factor(keepdata$Activity, levels = activity_num, labels = activity_labels)
keepdata$Subjects <- as.factor(keepdata$Subjects)

# Change remaining columns to more descriptive names

keepcols <- names(keepdata)

    keepcols <- gsub("[-()]", "", keepcols)
    keepcols <- gsub("^f", "FrequencyDomain", keepcols)
    keepcols <- gsub("^t", "TimeDomain", keepcols)
    keepcols <- gsub("Acc", "Acceleration", keepcols)
    keepcols <- gsub("Gyro", "Gyroscope", keepcols)
    keepcols <- gsub("Freq", "Frequency", keepcols)
    keepcols <- gsub("mean", "Mean", keepcols)
    keepcols <- gsub("std", "StandardDeviation", keepcols)
    keepcols <- gsub("BodyBody", "Body", keepcols)

names(keepdata) <- keepcols

# Select Mean and Standard Deviation columns to keep 

wantedfeat <- grep(".*Mean.*|.*StandardDeviation.*|.*Activity.*|.*Subjects.*", names(keepdata), value = TRUE)

# Prepare final data set

finaldata <- keepdata[, wantedfeat]

# Create secondary tidy data set from the final data set

finalmeans <- finaldata %>%
  
  group_by(Subjects, Activity) %>%
  summarize_each(funs(mean))

write.table(finalmeans, "FinalData.txt", row.name = FALSE)

str(finalmeans)

View(finalmeans)









