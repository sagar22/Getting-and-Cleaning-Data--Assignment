# Step1. Merges the training and the test sets to create one data set.
# setwd("~/Documents/Coursera")
trainData <- read.table("./UCI HAR Dataset/train/X_train.txt")
trainLabel <- read.table("./UCI HAR Dataset/train/y_train.txt")
table(trainLabel)
trainSubject <- read.table("./UCI HAR Dataset/train/subject_train.txt")
testData <- read.table("./UCI HAR Dataset/test/X_test.txt")
testLabel <- read.table("./UCI HAR Dataset/test/y_test.txt") 
table(testLabel) 
testSubject <- read.table("./UCI HAR Dataset/test/subject_test.txt")
joinData <- rbind(trainData, testData)
joinLabel <- rbind(trainLabel, testLabel)
joinSubject <- rbind(trainSubject, testSubject)

# Step2. Extracts only the measurements on the mean and standard 
# deviation for each measurement. 
features <- read.table("./UCI HAR Dataset/features.txt")
meanStdIndices <- grep("mean\\(\\)|std\\(\\)", features[, 2])
joinData <- joinData[, meanStdIndices]
names(joinData) <- gsub("\\(\\)", "", features[meanStdIndices, 2]) # remove "()"
names(joinData) <- gsub("mean", "Mean", names(joinData)) # capitalize M
names(joinData) <- gsub("std", "Std", names(joinData)) # capitalize S
names(joinData) <- gsub("-", "", names(joinData)) # remove "-" in column names 

# Step3. Uses descriptive activity names to name the activities in 
# the data set
activity <- read.table("./UCI HAR Dataset/activity_labels.txt")
activity[, 2] <- tolower(gsub("_", "", activity[, 2]))
substr(activity[2, 2], 8, 8) <- toupper(substr(activity[2, 2], 8, 8))
substr(activity[3, 2], 8, 8) <- toupper(substr(activity[3, 2], 8, 8))
activityLabel <- activity[joinLabel[, 1], 2]
joinLabel[, 1] <- activityLabel
names(joinLabel) <- "activity"

# Step4. Appropriately labels the data set with descriptive activity 
# names. 
names(joinSubject) <- "subject"
cleanedData <- cbind(joinSubject, joinLabel, joinData)
write.table(cleanedData, "merged_data.txt") # write out the 1st dataset

# Step5. Creates a second, independent tidy data set with the average of 
# each variable for each activity and each subject. 
subjectLen <- length(table(joinSubject)) # 30
activityLen <- dim(activity)[1] # 6
columnLen <- dim(cleanedData)[2]
tidy_data <- matrix(NA, nrow=subjectLen*activityLen, ncol=columnLen) 
tidy_data <- as.data.frame(tidy_data)
colnames(tidy_data) <- colnames(cleanedData)
row <- 1
for(i in 1:subjectLen) {
  for(j in 1:activityLen) {
    tidy_data[row, 1] <- sort(unique(joinSubject)[, 1])[i]
    tidy_data[row, 2] <- activity[j, 2]
    b1 <- i == cleanedData$subject
    b2 <- activity[j, 2] == cleanedData$activity
    tidy_data[row, 3:columnLen] <- colMeans(cleanedData[b1&b2, 3:columnLen])
    row <- row + 1
  }
}
head(tidy_data)
write.table(tidy_data, "tidy_data.txt") # write out the 2nd dataset

 data <- read.table("./tidy_data.txt")
 data[1:12, 1:3]
