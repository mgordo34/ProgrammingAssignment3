#Download zipped file

if(!file.exists("./data"))
{
  dir.create("./data")
  }
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./data/DataSceince3.zip")

#Unzip the file

unzip("DataSceince3.zip")

#Make individual files in for the training and test set into data frames

X_test <- read.table("./test/X_test.txt")
X_train <- read.table("./train/X_train.txt")
Y_test <- read.table("./test/Y_test.txt")
Y_train <- read.table("./train/Y_train.txt")
subject_train <- read.table("./train/subject_train.txt")
subject_test <- read.table("./test/subject_test.txt")
features <- read.table("./features.txt")

#Merge the training and the test sets to create one data set.
set_merge <- merge(X_train, X_test,all=TRUE)

#Appropriately label the data set with descriptive variable names.
features[,2] = gsub('-mean', 'Mean', features[,2])
features[,2] = gsub('-std', 'Std', features[,2])
features[,2] = gsub('[-()]', '', features[,2])
features_transpose <- t(features)
features_merge <- rbind(features_transpose[2,],set_merge, all=TRUE)

#Extract only the measurements on the mean and standard deviation for each measurement.
mean_std <- features_merge[,grepl("Mean|Std", features_merge)]
mean_std <- mean_std[-10301,] #remove the last logical row that gets added after the previous line

#Uses descriptive activity names to name the activities in the data set
label_merge <- rbind(Y_train, Y_test)
activity_labels <- read.table("./activity_labels.txt")
activity <- as.character(activity_labels$V2)

for (i in unique(label_merge$V1)){
  label_merge$V1 <- gsub(i,activity[i], label_merge$V1)
  
}
label_merge <- rbind(c("Activity"),label_merge)

#Merge the training and test subject identifiers
subject_merge <- merge(subject_train,subject_test, all=TRUE)
subject_merge <- rbind(c("Subject"),subject_merge)

#Generate the first "Tidy data" set and turn the first row of the tidy_data dataframe into the column names. 
tidy_data <- cbind(subject_merge,label_merge,mean_std)
colnames(tidy_data) <- as.character(unlist(tidy_data[1,]))
tidy_data = tidy_data[-1, ]

#Generate the second "Tidy data" set with the average of each variable for each activity and each subject.
tidy_data2 <- aggregate(. ~Subject + Activity, tidy_data, FUN = function(x) mean(as.numeric(as.character(x))))
tidy_data2 <- tidy_data2[order(tidy_data2$Subject),] #Order the final dataset by subject number. 

#Writing a .txt file for the submission. 
write.table(tidy_data2, "./HW3.txt", row.names=FALSE)
