############################################################################################################################

## Coursera Getting and Cleaning Data Course Assignment
## Rahul Shashwat
## 2017-05-28

# run_analysis.r File Description:

# This script will perform the following steps on the UCI HAR Dataset 
# 1. Merge the training and the test sets to create one data set.
# 2. Extract only the measurements on the mean and standard deviation for each measurement. 
# 3. Use descriptive activity names to name the activities in the data set
# 4. Appropriately label the data set with descriptive activity names. 
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

############################################################################################################################

# 1. Merge the training and the test sets to create one data set.
temp <- tempfile()
setwd('C:/Users/User/Desktop/Coursera Data/Course 3 Week 4')
download.file("http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",temp)
unzip(temp, list = TRUE) 
#set working directory to the location where the UCI HAR Dataset was unzipped
setwd('C:/Users/User/Desktop/Coursera Data/Course 3 Week 4/UCI HAR Dataset')
# Read the data from files
features     <- read.table('./features.txt',header=FALSE)
activityType <- read.table('./activity_labels.txt',header=FALSE)
# Read the Training data from files
subjectTrain <- read.table('./train/subject_train.txt',header=FALSE)
xTrain       <- read.table('./train/x_train.txt',header=FALSE)
yTrain       <- read.table('./train/y_train.txt',header=FALSE)

# Assigin column names to the data imported above
colnames(activityType)  <- c('activityId','activityType')
colnames(subjectTrain)  <- "subjectId"
colnames(xTrain)        <- features[,2]
colnames(yTrain)        <- "activityId"

# Create the final training data set by merging yTrain, subjectTrain, and xTrain
trainingData = cbind(yTrain,subjectTrain,xTrain)

# Read in the test data
subjectTest <- read.table('./test/subject_test.txt',header=FALSE)
xTest       <- read.table('./test/x_test.txt',header=FALSE)
yTest       <- read.table('./test/y_test.txt',header=FALSE)

# Assign column names to the test data imported above
colnames(subjectTest) <- "subjectId"
colnames(xTest)       <- features[,2]
colnames(yTest)       <- "activityId"


# Create the final test set by merging the xTest, yTest and subjectTest data
testData <- cbind(yTest,subjectTest,xTest)


# Combine training and test data to create a final data set to meet the objective 1
finalData <- rbind(trainingData,testData)

# Create a vector for the column names from the finalData, which will be used
# to select the desired mean() & stddev() columns
column_names  <- colnames(finalData)

# 2. Extract only the measurements on the mean and standard deviation for each measurement. 

# Create a logicalVector that contains TRUE values for the ID, mean() & stddev() columns and FALSE for others
logicalVector <- (grepl("activity..",column_names) | grepl("subject..",column_names) | 
                   grepl("-mean..",column_names) & !grepl("-meanFreq..",column_names) & !grepl("mean..-",column_names) | 
                   grepl("-std..",column_names) & !grepl("-std()..-",column_names))

# Subset finalData table based on the logicalVector to keep only desired columns
finalData <- finalData[logicalVector==TRUE]

# 3. Use descriptive activity names to name the activities in the data set

# Merge the finalData set with the acitivityType table to include descriptive activity names
finalData <- merge(finalData,activityType,by='activityId',all.x=TRUE)

# Updating the column_names vector to include the new column names after merge
column_names  <- colnames(finalData)

# 4. Appropriately label the data set with descriptive activity names. 

# Cleaning up the variable names
for (i in 1:length(column_names)) 
{
  column_names[i] <- gsub("\\()","",column_names[i])
  column_names[i] <- gsub("-std$","StdDev",column_names[i])
  column_names[i] <- gsub("-mean","Mean",column_names[i])
  column_names[i] <- gsub("^(t)","time",column_names[i])
  column_names[i] <- gsub("^(f)","freq",column_names[i])
  column_names[i] <- gsub("([Gg]ravity)","Gravity",column_names[i])
  column_names[i] <- gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",column_names[i])
  column_names[i] <- gsub("[Gg]yro","Gyroscope",column_names[i])
  column_names[i] <- gsub("[Aa]cc","Accelerator",column_names[i])
  column_names[i] <- gsub("[Mm]ag","Magnitude",column_names[i])
}

# Reassigning the new descriptive column names to the finalData set
colnames(finalData) <- column_names

# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 

# Create a new table, finalDataNoActivityType without the activityType column
final_dataset <- finalData[,names(finalData) != 'activityType']

# Summarizing the finalDataNoActivityType table to include just the mean of each variable for each activity and each subject
tidy_data <- aggregate(final_dataset [,names(final_dataset ) != c('activityId','subjectId')],by=list(activityId=final_dataset$activityId,subjectId = final_dataset$subjectId),mean)

# Merging the tidyData with activityType to include descriptive acitvity names
tidy_data <- merge(tidy_data,activityType,by='activityId',all.x=TRUE)

# Export the tidyData set 
write.table(tidy_data, file = "Tidy.txt", row.names = FALSE)
# End of code