run_analysis <- function()
{
    #Reading all the data files
    x_test <- read.table("./test/X_test.txt", header = FALSE)
    y_test <- read.table("./test/y_test.txt", header = FALSE)
    x_train <- read.table("./train/X_train.txt", header = FALSE)
    y_train <- read.table("./train/y_train.txt", header = FALSE)
    subject_test <- read.table("./test/subject_test.txt", header = FALSE)
    subject_train <- read.table("./train/subject_train.txt", header = FALSE)
    
    #Concatenate the data tables by rows
    data_subject <- rbind(subject_train, subject_test)
    data_activity <- rbind(y_train, y_test)
    data_feature <- rbind(x_train, x_test)
    
    #labelling the variables
    names(data_subject) <- c("Subject")
    names(data_activity) <- c("Activity_ID")
    features <- read.table("features.txt", header = FALSE)
    names(data_feature) <- features$V2
    
    #merging columns to get all of the data
    partial_data <- cbind(data_subject, data_activity)
    full_data <- cbind(data_feature, partial_data)
    
    #subset name of features by measurements on the mean and standard deviation
    sub_data <- features$V2[grep("mean\\(\\)|std\\(\\)", features$V2)]
    
    #subset the data frame by selected name of features
    selected_names <- c(as.character(sub_data), "Subject", "Activity_ID")
    full_data <- subset(full_data, select = selected_names)
    
    #reading the activity names from activity_labels.txt
    activity_label <- read.table("activity_labels.txt", header = FALSE, col.names = c("ID", "Activity"))
    
    #mapping the activity names based on the activiy ID
    merge_data <- merge(full_data, activity_label, by.x = "Activity_ID", by.y = "ID")
    
    #labelling the data set with descriptive variable names
    names(merge_data) <- gsub("^t", "Time", names(merge_data))
    names(merge_data) <- gsub("^f", "Frequency", names(merge_data))
    names(merge_data) <- gsub("Acc", "Accelerometer", names(merge_data))
    names(merge_data) <- gsub("Gyro", "Gyroscope", names(merge_data))
    names(merge_data) <- gsub("Mag", "Magnitude", names(merge_data))
    names(merge_data) <- gsub("BodyBody", "Body", names(merge_data))
    names(merge_data)
    
    #creating a tidy data
    library(plyr);
    final_data <- aggregate(. ~Subject + Activity, merge_data, mean)
    final_data <- final_data[order(final_data$Subject, final_data$Activity),]
    
    write.table(final_data, "tidydata.txt", row.name = FALSE)
    
    #produce codebook
    library(knitr)
    knit2html("codebook.Rmd");
}