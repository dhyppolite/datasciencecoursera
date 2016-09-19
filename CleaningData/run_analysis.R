
library(dplyr)
features <- read.table("./UCI HAR Dataset/features.txt",stringsAsFactors = FALSE)
activityLabels <- read.table("./UCI HAR Dataset/activity_labels.txt")
colnames(activityLabels) <- c("id", "label")

#convert features to df for dplyr usage
featuresDf <- tbl_df(features)

#filter features for mean and standard devation aka (mean, std)
mean_std_ind <- grep("mean[()]{2}|std[()]{2}", featuresDf$V2)

prep_data <- function(typeLabel)
{
  x_vals <- read.table(paste("./UCI HAR Dataset/",typeLabel,"/X_", typeLabel,".txt", sep=""))
  names(x_vals) <- c(sub("[()]{2}", "", features[,2]))
  x_vals <- x_vals[, mean_std_ind]
  
  y_vals <- read.table(paste("./UCI HAR Dataset/", typeLabel,"/y_", typeLabel,".txt", sep=""))
  colnames(y_vals) <- c("id")
  combined <- left_join(y_vals, activityLabels)
  
  subject_vals <- read.table(paste("./UCI HAR Dataset/", typeLabel,"/subject_", typeLabel,".txt", sep=""))
  
  
  x_vals$activity_name <- combined[,2]
  x_vals$set_label <- c(typeLabel) 
  x_vals$subject_id <- c(subject_vals[,1])
  
  x_vals
}

test_data <- prep_data("test")
train_data <- prep_data("train")

merged_data <- merge(test_data, train_data, all=TRUE)
column_averages <- data.frame(select(merged_data, -(activity_name:subject_id)) %>% lapply(mean))

#Output final merged training and test data
write.table(merged_data, file="tidy_data.txt")

#Output averaged data
write.table(column_averages, file="tidy_data_averages.txt")
