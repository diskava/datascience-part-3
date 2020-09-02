# Obtaining and Cleaning Data
# Authored by: Mwenge Mulenga

# 1. Merges the training and the test data to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# Prepare Packages and get Data
packages <- c("data.table", "reshape2")
sapply(packages, require, character.only=TRUE, quietly=TRUE)

path <- getwd()
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url, file.path(path, "dataFiles.zip"))
unzip(zipfile = "dataFiles.zip")

# Load activity labels as well as features
activity_labels <- fread(file.path(path, "UCI HAR Dataset/activity_labels.txt")
                         , col.names = c("classLabels", "activityName"))
row_features <- fread(file.path(path, "UCI HAR Dataset/features.txt"), col.names = c("index", "featureNames"))
features_filtered <- grep("(mean|std)\\(\\)", row_features[, featureNames])
measurements <- row_features[features_filtered, featureNames]
measurements <- gsub('[()]', '', measurements)

# Load train data
train <- fread(file.path(path, "UCI HAR Dataset/train/X_train.txt"))[, features_filtered, with = FALSE]
data.table::setnames(train, colnames(train), measurements)
trainActivities <- fread(file.path(path, "UCI HAR Dataset/train/Y_train.txt"), col.names = c("Activity"))
trainSubjects <- fread(file.path(path, "UCI HAR Dataset/train/subject_train.txt"), col.names = c("SubjectNum"))
train <- cbind(trainSubjects, trainActivities, train)

# Load test data
test <- fread(file.path(path, "UCI HAR Dataset/test/X_test.txt"))[, features_filtered, with = FALSE]
data.table::setnames(test, colnames(test), measurements)
testActivities <- fread(file.path(path, "UCI HAR Dataset/test/Y_test.txt"), col.names = c("Activity"))
testSubjects <- fread(file.path(path, "UCI HAR Dataset/test/subject_test.txt"), col.names = c("SubjectNum"))
test <- cbind(testSubjects, testActivities, test)

# combine train and test data
merged_data <- rbind(train, test)

# Change classLabels to activityName  
merged_data[["Activity"]] <- factor(merged_data[, Activity], levels = activity_labels[["classLabels"]], labels = activity_labels[["activityName"]])

merged_data[["SubjectNum"]] <- as.factor(merged_data[, SubjectNum])
merged_data <- reshape2::melt(data = merged_data, id = c("SubjectNum", "Activity"))
merged_data <- reshape2::dcast(data = merged_data, SubjectNum + Activity ~ variable, fun.aggregate = mean)


data.table::fwrite(x = merged_data, file = "tidy_data.txt", quote = FALSE)
