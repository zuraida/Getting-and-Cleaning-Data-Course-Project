#1. Merge the training and test data set into one file
test <- read.table("./data/Porject module 3/UCI HAR Dataset/test/X_test.txt")
train <- read.table("./data/Porject module 3/UCI HAR Dataset/train/X_train.txt")
X <- rbind(test, train)

Stest <- read.table("./data/Porject module 3/UCI HAR Dataset/test/subject_test.txt")
Strain <- read.table("./data/Porject module 3/UCI HAR Dataset/train/subject_train.txt")
S <- rbind(Stest, Strain)

Ltest <- read.table("./data/Porject module 3/UCI HAR Dataset/test/y_test.txt")
Ltrain <- read.table("./data/Porject module 3/UCI HAR Dataset/train/y_train.txt")
L <- rbind(Ltest,Ltrain)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
features <- read.table("./data/Porject module 3/UCI HAR Dataset/features.txt")
indices_of_good_features <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
X1 <- X[, indices_of_good_features]
names(X1) <- features[indices_of_good_features, 2]
names(X1) <- gsub("\\(|\\)", "", names(X1))
names(X1) <- tolower(names(X1))

#3. Uses descriptive activity names to name the activities in the data set.
activities <- read.table("./data/Porject module 3/UCI HAR Dataset/activity_labels.txt")
activities[,2] = gsub("_","",tolower(as.character(activities[, 2])))

L[,1]=activities[L[,1],2]
names(L) <- "activity"

# 4. Appropriately labels the data set with descriptive activity names.
names(S) <- "subject"
cleaned <- cbind(S, L, X1)
write.table(cleaned, "./data/Porject module 3/merged_clean_data.txt")

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

UniqueSubjects = unique(S)[,1]
NumSubjects = length(unique(S)[,1])
NumActivities = length(activities[,1])
NumCol = dim(cleaned)[2]
Result = cleaned[1:(NumSubjects*NumActivities),]

row = 1
for (s in 1:NumSubjects){
  for (a in 1:NumActivities){
    Result[row, 1] = UniqueSubjects[s]
    Result[row, 2] = activities[a,2]
    tmp <- cleaned [cleaned$subject== s  & cleaned$activity == activities[a,2], ]
    Result [row, 3:NumCol] <- colMeans(tmp[ , 3:NumCol])
    row = row+1
  }
    
}
write.table(Result, "./data/Porject module 3/data_set_with_the_averages.txt")
view <- read.table("./data/Porject module 3/data_set_with_the_averages.txt")
