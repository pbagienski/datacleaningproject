##First, make sure dataset is present.

url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
datadir <- "./UCI HAR Dataset"

if (!dir.exists(datadir)){
        download.file(url, "zipdata.zip")
        unzip("zipdata.zip")
}

##To make future steps easier, set datadir as working directory

setwd(datadir)

##Read all relevant files

activity       <- read.table("activity_labels.txt")
features       <- read.table("features.txt")

subTest        <- read.table("./test/subject_test.txt")
xTest          <- read.table("./test/X_test.txt")
yTest          <- read.table("./test/y_test.txt")

subTrain       <- read.table("./train/subject_train.txt")
xTrain         <- read.table("./train/X_train.txt")
yTrain         <- read.table("./train/y_train.txt")

## Give above data proper column names

colnames(activity)   <- c('ActivityID', 'ActivityType')

colnames(subTest)    <- "SubjectID"
colnames(xTest)      <- features[,2]
colnames(yTest)       <- "ActivityID"

colnames (subTrain)  <- "SubjectID"
colnames(xTrain)     <- features[,2]
colnames(yTrain)     <- "ActivityID"

## Merge all test sets as one dataset and all training sets as one dataset

test  <- cbind(subTest, xTest, yTest)
train <- cbind(subTrain, xTrain, yTrain)

#Merge test and training sets

all <- rbind(test, train)

##Extract only mean and standard deviation values by first creating a vector
##of the column names

names <- colnames(all)

##Identify only columns with mean or std in name, and the ID columns

meanSD <- grep("SubjectID|ActivityID|mean..|std..", names)

##subset these columns and replace activity ID with corresponding name,
##this descriptively labels the activities

allmeanSD <- all[meanSD]
namedData <- merge(allmeanSD, activity, by="ActivityID", all.x=TRUE)

##Clean up column names

finalcolnames <- colnames(namedData)
for (i in 1:length(finalcolnames)){
        finalcolnames[i] <- gsub("\\()", "", finalcolnames[i])
        finalcolnames[i] <- gsub("^t", "Time-", finalcolnames[i])
        finalcolnames[i] <- gsub("^f", "Frequency-", finalcolnames[i])
        finalcolnames[i] <- gsub("\\()", "", finalcolnames[i])
        finalcolnames[i] <- gsub("mean", "Mean", finalcolnames[i])
        finalcolnames[i] <- gsub("std", "StandardDeviation", finalcolnames[i])
        finalcolnames[i] <- gsub("Acc", "Acceleration", finalcolnames[i])
        finalcolnames[i] <- gsub("Mag", "Magnitude", finalcolnames[i])
        finalcolnames[i] <- gsub("Freq$", "Frequency", finalcolnames[i])
}

colnames(namedData) <- finalcolnames

##Clean up position of columns. In particular, we will move ActivityType
##to the beginning with ActivityID and SubjectID

grep("ActivityType", names(namedData))->index
finalData <- namedData[, c(index, (1:ncol(namedData))[-index])]

##Finally, create another table for the averages of each variable for each
##subject and activity and write into text file

averages<- aggregate(. ~SubjectID + ActivityType, finalData, mean)
write.table(averages, "averages.txt")
