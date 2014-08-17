## Coursera - Getting and Cleaning Data
## Course Project
##
## You should create one R script called run_analysis.R that does the following.
## 1. Merges the training and the test sets to create one data set.
## 2. Extracts only the measurements on the mean and standard deviation for each measurement.
## 3. Uses descriptive activity names to name the activities in the data set
## 4. Appropriately labels the data set with descriptive variable names.
## 5. Creates a second, independent tidy data set with the average of each variable for each
##    activity and each subject.

## Download files
setwd("~/DS Toolbox/Github/GetData_Project")
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
if (!file.exists("UCI HAR Dataset.zip")) download.file(fileURL, "UCI HAR Dataset.zip", method="curl")
if (!file.exists("UCI HAR Dataset")) unzip("UCI HAR Dataset.zip")

## Read activity labels
activity <- read.table("./UCI HAR Dataset/activity_labels.txt", col.names=c("Activity", "ActivityName"))
## Read feature names
features <- read.table("./UCI HAR Dataset/features.txt", col.names=c("Num", "FeatureName"))

## Steps for #1
## Read test subjects
subjects_test <- read.table("./UCI HAR Dataset/test/subject_test.txt", col.names=c("Subject"))
## Read training subjects
subjects_train <- read.table("./UCI HAR Dataset/train/subject_train.txt", col.names=c("Subject"))
# subjects <- rbind(subjects, subjects_train)  # Combine training and test subjects

test <- read.table("./UCI HAR Dataset/test/X_test.txt")  # read test data
test_a <- read.table("./UCI HAR Dataset/test/Y_test.txt", col.names=c("Activity"))  # test activities
test <- cbind(test_a, test)  # column bind with activity numbers at the beginning
test <- cbind(subjects_test, test)  # column bind with subjects at the beginning

train <- read.table("./UCI HAR Dataset/train/X_train.txt")  # read training data
train_a <- read.table("./UCI HAR Dataset/train/Y_train.txt", col.names=c("Activity"))  # training
train <- cbind(train_a, train)  # column bind with activity numbers at the beginning
train <- cbind(subjects_train, train)  # column bind with subjects at the beginning

# 1. Merge test and training data set
merged <- rbind(test, train)

## Steps for #2
## Create a subset of the features that correspond to mean and standard deviation measurements
f.mean <- features[grep("mean()", fixed = TRUE, features$FeatureName), ]
mean_sd <- rbind(f.mean, features[grep("std()", fixed = TRUE, features$FeatureName), ])

## 2. Extract only mean and standard deviation measurements
## Since my data from #1 already contains subject & activity as 1st and 2nd columns, I have
## to add an offset to the column index for mean & sd
tidy1 <- merged[, c(1,2, mean_sd$Num+2)]

## 3. Use descriptive activity names to name the activities in the data set
## Merging the previously read activity table with the tidy1 dataset will add
## descriptive activity names to each row
tidy1 <- merge(activity, tidy1, by.x="Activity", by.y="Activity", all=FALSE)

## Steps for #4
## Create a new column which will contain R friendly column names for the measurements
mean_sd$ShortName <- gsub("()", "", fixed=TRUE, gsub("-", "", mean_sd$FeatureName))
## Manipulate the new column to create descriptive names for measurement
mean_sd$ShortName <- gsub("mean", ".mean", mean_sd$ShortName)
mean_sd$ShortName <- gsub("meanX", "X.mean", mean_sd$ShortName)
mean_sd$ShortName <- gsub("meanY", "Y.mean", mean_sd$ShortName)
mean_sd$ShortName <- gsub("meanZ", "Z.mean", mean_sd$ShortName)
mean_sd$ShortName <- gsub("std", ".std", mean_sd$ShortName)
mean_sd$ShortName <- gsub("stdX", "X.std", mean_sd$ShortName)
mean_sd$ShortName <- gsub("stdY", "Y.std", mean_sd$ShortName)
mean_sd$ShortName <- gsub("stdZ", "Z.std", mean_sd$ShortName)

## 4. Label the data set with descriptive variable names
names(tidy1)[4:69] <- mean_sd[, 3]


## Steps for #5
## Convert Activity & Subject to factor variables
tidy1$Activity <- factor(tidy1$Activity)
tidy1$Subject <- factor(tidy1$Subject)

## Calculate average of each measurement by data by Activity & Subject
tidydata <- aggregate(x = tidy1[ , 4:69]
                        , by = list(Subject = tidy1$Subject, Activity = tidy1$ActivityName)
                        , FUN = "mean")

## 5. Save the independent tidy data set created above, to a file
## Please note that this file can be read with the following command:
##    read.table("test.txt", header = TRUE)
write.table(tidydata, "GetnCleanDataProject.txt", quote = FALSE, sep = "\t", row.names = FALSE)
