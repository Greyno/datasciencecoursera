#This R script (run_analysis.R) does the following:
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in step 4, creates a second, independent tidy data set with the #average of each variable for each activity and each subject.

library(plyr)
#Create a directory for the files if it does not exist. The data are assumed to be in the user directory in a folder called "data"
if (!file.exists("data")) {
    dir.create("data")
}

#The dataset are as follows: X_ training and test data; Y_ training and test labels (1-6, representing the 6 activities); subject_ training and test subjects (range 1-30)
getwd()

#Load the test files from the data directory
testData<-read.table("~/data/UCI HAR Dataset/test/X_test.txt")
testLabels<-read.table("~/data/UCI HAR Dataset/test/Y_test.txt")
testSubjects<-read.table("~/data/UCI HAR Dataset/test/subject_test.txt")
#Load the training files
trainingData<-read.table("~/data/UCI HAR Dataset/train/X_train.txt")
trainingLabels<-read.table("~/data/UCI HAR Dataset/train/Y_train.txt")
trainingSubjects<-read.table("~/data/UCI HAR Dataset/train/subject_train.txt")

#Join the training and test data sets
dataSet<-rbind(trainingData, testData)
#dim(dataSet)
#Join the training and test label sets
labelSet<-rbind(trainingLabels, testLabels)
#dim(labelSet)
#Join the training and test subject sets
subjectSet<-rbind(trainingSubjects, testSubjects)

#Get the features data and find the column numbers that correspond to the mean and std
featuresData<-read.table("~/data/UCI HAR Dataset/features.txt")
#dim(featuresData)
#Use grep to get the row numbers that have only mean() or std()
meanAndStd<-grep("mean\\(\\)|std\\(\\)", featuresData[,2])
#View(meanAndStd) 
dataSet<-dataSet[, meanAndStd] #Row of columns number that match the grep selections
#View(dataSet)

#Apply the names to the data columns and remove the ()
names(dataSet)<-gsub("\\(\\)","",featuresData[meanAndStd,2]) 
#Change the column names of the Subject and Label data sets
names(labelSet)<-c("Label") #Change Label column name from V1 to "Label"
names(subjectSet)<-c("Subject") #Change Subject column name from V1 to "Subject"

#Merged the data. Now has only mean and std columns selected
mergedDataSet<-cbind(subjectSet, labelSet, dataSet)
#View(mergedDataSet)

#Clean up the column names by removing _ and converting names to lower case
#Use gsub and operate on the 'names' of the dataset
names(mergedDataSet)<-gsub("-", "", names(mergedDataSet)) #Remove '-' from the names
names(mergedDataSet)<-tolower(names(mergedDataSet)) #Create names in all lower case
#View(mergedDataSet) #Use this to get a view of the final dataset 

#Get the activity labels
activity<-read.table("~/data/UCI HAR Dataset/activity_labels.txt")
activity[,2]<-tolower(activity[,2]) #Change column 2 names to lower case
activity[,2]<-gsub("_", "", activity[,2]) #Remove "_" values from column 2 names
#View(activity)

#Add the activity names to the full label set
#View(labelSet)
activityLabel<-activity[labelSet[,1],2]
labelSet[,1]<-activityLabel
names(labelSet)<-"activity"

#Add the activity to the subject data
names(subjectSet)<-"subject"
#View(subjectSet)

#Create a final data set with the appropriate labels
editedData<-cbind(subjectSet, labelSet, dataSet)
#dim(editedData)

#Clean up the names in the edited data set
names(editedData)<-gsub("-", "", names(editedData)) #Remove '-' from the names
names(editedData)<-tolower(names(editedData)) #Create names in all lower case
#View(editedData)

#The editedData contains the subjects, their activities and the mean and std results. The data have each of the 6 activities completed a number of times by each subject. These data are in a wide format.

#From the editedData set, create a second, independent tidy data set with the average of each variable for each activity and each subject. There are 10,299 observations over 68 variables (including the subject and activity)

#Use ddply to capture the average of the columns by subject and activity
#The resulting tidy data is in a wide format - each measured variable is in one column and each observation of that variable is in one row i.e. each of the 6 activities is reported once for each subject; each measured observation is in its own column. There are 180 observations over 68 variables (including the subject and activity)

#The tidy data is in a wide format
tidyData<-ddply(editedData, .(subject, activity), function(x) colMeans(x[, 3:68]))
#View(tidyData)

#Write the tidy data to the data directory
write.table(tidyData, file="~/data/tidyData.txt", row.names=FALSE)


