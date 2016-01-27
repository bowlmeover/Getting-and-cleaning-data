# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in step 4, creates a second, independent tidy data set with the average 
#    of each variable for each activity and each subject.

# Check to see if data.table is already loaded. If not, install it. 
if (!require("data.table")) {install.packages("data.table")}

# Check to see if data.table is already loaded. If not, install it. 
if (!require("reshape2")) {install.packages("reshape2")}

# Set the working directory for the project
setwd("/Users/Joel/Documents/coursera/Getting and cleaning data/Course Project/")

# Load the data labels from the features file
# Features file contains 2 columns, an integer value for the column number, and a name
features <- read.delim("./UCI HAR Dataset/features.txt",header = FALSE, sep = "")[,2]

# Load the activity labels containing the plain English 
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")[,2]

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# Create a vector of the boolean values that identify variables that represent a  mean or  standard deviation
extract_features <- grepl("mean|std", features)

# Load all the test data (subject_test, X_test, y_test)
# Load the data about the subjects of the test
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")

# Load the data about the activities that were performed
# the data is a single column containing an integer
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt")
# 3. Uses descriptive activity names to name the activities in the data set
# add a second column to y_test that maps the activity integer to the plain text name
y_test[,2] = activity_labels[y_test[,1]]

# Load the test readings from the sensors
x_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
# 4. Appropriately labels the test data set with descriptive variable names.
names(x_test) = features

# subset the test data to only the mean & std values
x_test = x_test[,extract_features]

# Load all the train data (subject_train, x_train, y_train)
# Load the data about the subjects of the test
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")

# Load the data about the activities that were performed
# the data is a single column containing an integer
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt")
# 3. Uses descriptive activity names to name the activities in the data set
# add a second column to y_test that maps the activity integer to the plain text name
y_train[,2] = activity_labels[y_train[,1]]

# Load the test readings from the sensors
x_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
# 4. Appropriately labels the training data set with descriptive variable names.
names(x_train) = features
# subset the training data to only the mean & std values
x_train = x_train[,extract_features]

# 4. Appropriately labels the data set with descriptive variable names.
names(y_test) = c("Activity_ID", "Activity_Label")
names(subject_test) = "subject"

names(y_train) = c("Activity_ID", "Activity_Label")
names(subject_train) = "subject"

# 1. Merges the training and the test sets to create one data set.
#merge test data into a single data_table
combined_test_data <- cbind(as.data.table(subject_test), y_test, x_test)

#merge training data into a single data_table
combined_train_data <- cbind(as.data.table(subject_train), y_train, x_train)

#merge training and test data into a single data_table
combined_data <- rbind (combined_test_data, combined_train_data)

# 5. From the data set in step 4, creates a second, independent tidy data set with the average
#    of each variable for each activity and each subject.
id_labels   = c("subject", "Activity_ID", "Activity_Label")
data_labels = setdiff(colnames(combined_data), id_labels)
melt_data   = melt(combined_data, id = id_labels, measure.vars = data_labels)

# Apply mean function to dataset using dcast function
tidy_data   = dcast(melt_data, subject + Activity_Label ~ variable, mean)

write.table(tidy_data, file = "./tidy_data.txt", row.names = FALSE)