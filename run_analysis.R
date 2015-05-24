run_analysis <- function()
  {
#The purpose of this Assignment is to demonstrate the ability to collect, work with, and clean a data set. 
#The goal is to prepare tidy data that can be used for later analysis. 
#This run_analysis.R script does the following: 
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names. 
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# STEP 1. Merges the training and the test sets to create one data set.
file1 <- read.table("./TrainTestDataset/train/X_train.txt")
file2 <- read.table("./TrainTestDataset/test/X_test.txt")
X <- rbind(file1, file2)

file1 <- read.table("./TrainTestDataset/train/subject_train.txt")
file2 <- read.table("./TrainTestDataset/test/subject_test.txt")
S <- rbind(file1, file2)

file1 <- read.table("./TrainTestDataset/train/y_train.txt")
file2 <- read.table("./TrainTestDataset/test/y_test.txt")
Y <- rbind(file1, file2)

# STEP 2. Extracts only the measurements on the mean and standard deviation for each measurement.

features <- read.table("./TrainTestDataset/features.txt")
mean_std_measure <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
X <- X[, mean_std_measure]
names(X) <- features[mean_std_measure, 2]
names(X) <- gsub("\\(|\\)", "", names(X))
names(X) <- tolower(names(X))

# STEP 3. Uses descriptive activity names to name the activities in the data set.

name_activities <- read.table("./TrainTestDataset/activity_labels.txt")
name_activities[, 2] = gsub("_", "", tolower(as.character(name_activities[, 2])))
Y[,1] = name_activities[Y[,1], 2]
names(Y) <- "name_activities"

# STEP 4. Appropriately labels the data set with descriptive activity names.

names(S) <- "subject"
cleaned <- cbind(S, Y, X)
write.table(cleaned, "clean_data_merged.txt")

# STEP 5. Creates a 2nd, independent tidy data set with the average of each variable for each activity and each subject.

uniqueSubjects = unique(S)[,1]
numSubjects = length(unique(S)[,1])
numActivities = length(activities[,1])
numCols = dim(cleaned)[2]
result = cleaned[1:(numSubjects*numActivities), ]

row = 1
for (s in 1:numSubjects) {
  for (a in 1:numActivities) {
    result[row, 1] = uniqueSubjects[s]
    result[row, 2] = activities[a, 2]
    tmp <- cleaned[cleaned$subject==s & cleaned$activity==activities[a, 2], ]
    result[row, 3:numCols] <- colMeans(tmp[, 3:numCols])
    row = row+1
  }
}
write.table(result, "tidy_data_set_with_averages.txt")
}

