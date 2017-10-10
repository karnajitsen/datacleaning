library(dplyr)
library(plyr)

# read train data
X_train <- read.table("./data/train/X_train.txt")
Y_train <- read.table("./data/train/Y_train.txt")
Sub_train <- read.table("./data/train/subject_train.txt")

# read test data
X_test <- read.table("./data/test/X_test.txt")
Y_test <- read.table("./data/test/Y_test.txt")
Sub_test <- read.table("./data/test/subject_test.txt")

# read data description
var_names <- read.table("./data/features.txt")

# read activity labels
activity_labels <- read.table("./data/activity_labels.txt")

# 1. Merges the training and the test sets to create one data set.
X_total <- rbind(X_train, X_test)
Y_total <- rbind(Y_train, Y_test)
Sub_total <- rbind(Sub_train, Sub_test)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
cols_select <- grep(".*mean.*|.*std.*", var_names[,2])
selected_var <- var_names[cols_select,]
X_total <- X_total[selected_var[,1],]
colnames(X_total) <- gsub("\\(\\)", "", colnames(X_total))
# 3. Uses descriptive activity names to name the activities in the data set
colnames(Y_total) <- "activity"
Y_total$activity <- sapply(Y_total$activity, function(x) {activity_labels[x,2]})


# 4. Appropriately labels the data set with descriptive variable names.
colnames(X_total) <- var_names[selected_var[,1],2]

# 5. From the data set in step 4, creates a second, independent tidy data set with the average
# of each variable for each activity and each subject.

colnames(Sub_total) <- "subject"
total <- as.data.frame(cbind(X_total, Y_total$activity, Sub_total))
rename(total, c("Y_total$activity" = "activity"))
total_mean <- as.data.frame(total %>% group_by(subject,activity) %>% summarise_all(funs(mean)))
write.table(total_mean, file = "./data/tidydata.txt", row.names = FALSE, col.names = TRUE)