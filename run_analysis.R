#DOWNLOADING ZIP FILE AND UNZIP IT IN A FOLDER

url<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url,dest="run_analysis.zip",mode="wb")
dir.create("./data_zip")
unzip(zipfile = "./run_analysis.zip",exdir = "./data_zip")

#Reading data from file for training dataset
activityLabels <- read.table("./data_zip/UCI HAR Dataset/activity_labels.txt",header=FALSE)
activityLabels[,2]<-as.character(activityLabels[,2])

features <- read.table("./data_zip/UCI HAR Dataset/features.txt",header=FALSE)
features[,2] <- as.character(features[,2])

subjectTrain = read.table('./data_zip/UCI HAR Dataset/train/subject_train.txt',header=FALSE)
xTrain       = read.table('./data_zip/UCI HAR Dataset/train/x_train.txt',header=FALSE)
yTrain       = read.table('./data_zip/UCI HAR Dataset/train/y_train.txt',header=FALSE)

#Assigning coloumn names to data tables
colnames(activityLabels)  = c('activityId','activityType')
colnames(subjectTrain)  = "subjectId"
colnames(xTrain)        = features[,2] 
colnames(yTrain)        = "activityId"


#Creating training dataset
trainingData = cbind(yTrain,subjectTrain,xTrain)

#Reading data from file for testing dataset
subjectTest = read.table('./data_zip/UCI HAR Dataset/test/subject_test.txt',header=FALSE)
xTest       = read.table('./data_zip/UCI HAR Dataset/test/x_test.txt',header=FALSE)
yTest       = read.table('./data_zip/UCI HAR Dataset/test/y_test.txt',header=FALSE)

#Assigning coloumn names to test data
colnames(subjectTest) = "subjectId"
colnames(xTest)       = features[,2] 
colnames(yTest)       = "activityId"

#Creating test dataset
testData = cbind(yTest,subjectTest,xTest)

#1.Merges the training and the test sets to create one data set
finaldata = rbind(trainingData,testData)



#2.Extracts only the measurements on the mean and standard deviation for each measurement

finalcolumns <- grepl("subject|activity|mean|std", colnames(finaldata))

finaldata <- finaldata[, finalcolumns]


#3.Uses descriptive activity names to name the activities in the data set
finaldata = merge(finaldata,activityLabels,by='activityId',all.x=TRUE)

#4.Appropriately labels the data set with descriptive variable names
finaldataCols <- colnames(finaldata)
# Removing special characters
finaldataCols <- gsub("[\\(\\)-]", "", finaldataCols)

#Cleaning up names
finaldataCols <- gsub("^f", "frequencyDomain", finaldataCols)
finaldataCols <- gsub("^t", "timeDomain", finaldataCols)
finaldataCols <- gsub("Acc", "Accelerometer", finaldataCols)
finaldataCols <- gsub("Gyro", "Gyroscope", finaldataCols)
finaldataCols<- gsub("Mag", "Magnitude", finaldataCols)
finaldataCols <- gsub("Freq", "Frequency", finaldataCols)
finaldataCols <- gsub("mean", "Mean", finaldataCols)
finaldataCols <- gsub("std", "StandardDeviation", finaldataCols)


finaldataCols <- gsub("BodyBody", "Body", finaldataCols)

#5.From the data set in step 4, creates a second,
#independent tidy data set with the average of each variable for each activity and each subject

tidydata <- finaldata %>%
  select(-activityType)%>%
  group_by(activityId,subjectId) %>%
  summarise_all(funs(mean))

write.table(finaldataMeans, "tidy_data.txt", row.names = FALSE, 
            quote = FALSE)

