## Data Description : http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
## Data             : https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
## Data it's about 60Mb

run_analysis <- function(dataPath){

  require(plyr)

  ## Open files

  feature_file <- paste(dataPath, "features.txt", sep = "")
  activity_labels_file <- paste(dataPath, "activity_labels.txt", sep = "")
  x_train_file <- paste(dataPath, "train/X_train.txt", sep = "")
  y_train_file <- paste(dataPath, "train/y_train.txt", sep = "")
  subject_train_file <- paste(dataPath, "train/subject_train.txt", sep = "")
  x_test_file  <- paste(dataPath, "test/X_test.txt", sep = "")
  y_test_file  <- paste(dataPath, "test/y_test.txt", sep = "")
  subject_test_file <- paste(dataPath, "test/subject_test.txt", sep = "")

  ## Load data

  features <- read.table(feature_file, colClasses = c("character"))
  activity_labels <- read.table(activity_labels_file, col.names = c("ActivityId", "Activity"))
  x_train <- read.table(x_train_file)
  y_train <- read.table(y_train_file)
  subject_train <- read.table(subject_train_file)
  x_test <- read.table(x_test_file)
  y_test <- read.table(y_test_file)
  subject_test <- read.table(subject_test_file)

  ## 1 - Merges the training and the test sets to create one data set.

  train_data <- cbind(cbind(x_train, subject_train), y_train)
  test_data <- cbind(cbind(x_test, subject_test), y_test)
  sensor_data <- rbind(train_data, test_data)
  sensor_labels <- rbind(rbind(features, c(562, "Subject")), c(563, "ActivityId"))[,2]
  names(sensor_data) <- sensor_labels

  ## 2 - Extracts only the measurements on the mean and standard deviation for each measurement.

  data <- sensor_data[,grepl("mean|std|Subject|ActivityId", names(sensor_data))]

  ## 3 - Uses descriptive activity names to name the activities in the data set

  data <- join(data, activity_labels, by = "ActivityId", match = "first")
  data <- data[,-1]

  ## 4 - Appropriately labels the data set with descriptive names.

  names(data) <- gsub('\\(|\\)',"",names(data), perl = TRUE)
  names(data) <- make.names(names(data))

  names(data) <- gsub('Acc',"Acceleration",names(data))
  names(data) <- gsub('GyroJerk',"AngularAcceleration",names(data))
  names(data) <- gsub('Gyro',"AngularSpeed",names(data))
  names(data) <- gsub('Mag',"Magnitude",names(data))
  names(data) <- gsub('^t',"TimeDomain.",names(data))
  names(data) <- gsub('^f',"FreqDomain.",names(data))
  names(data) <- gsub('\\.mean',".mean",names(data))
  names(data) <- gsub('\\.std',".stdDev",names(data))
  names(data) <- gsub('Freq\\.',"Frequency.",names(data))
  names(data) <- gsub('Freq$',"Frequency",names(data))

  ## 5 - From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

  tidy = ddply(data, c("Subject","Activity"), numcolwise(mean))
  
  ##write.table(tidy, file = paste(dataPath, "output.txt", sep=""), row.names=FALSE)
}
