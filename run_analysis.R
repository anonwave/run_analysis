#load dplyr, tidyr, readr
install.packages("dplyr") #if not set up already 
library(readr)
library(dplyr)
library(tidyr) 


#Data Collection 
#Download file url using link: Click on link, download to download file, move to Course 3 Project file.
rundata<- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"


#Data Import & Transformation: Creating a tidy data set in R by merging training and test sets to create one set

#Data import
#Import data into R using 'Base' not 'readr'by clicking: FILE - IMPORT DATA SET - FROM TEXT (BASE) - IMPORT
X_test <- read.table("~/Desktop/Course 3 Project /UCI HAR Dataset/test/X_test.txt", quote="\"", comment.char="")
View(X_test)

X_train <- read.table("~/Desktop/Course 3 Project /UCI HAR Dataset/train/X_train.txt", quote="\"", comment.char="")
View(X_train)

#Import the files subject_test.txt, subject_train.txt to create common ID, to merge train and test data with. 
subject_test <- read.table("~/Desktop/Course 3 Project /UCI HAR Dataset/test/subject_test.txt", quote="\"", comment.char="")
View(subject_test)

subject_train <- read.table("~/Desktop/Course 3 Project /UCI HAR Dataset/train/subject_train.txt", quote="\"", comment.char="")
View(subject_train)

#Import variables in features.txt to use to rename the merged data set
features <- read.table("~/Desktop/Course 3 Project /UCI HAR Dataset/features.txt", quote="\"", comment.char="")
View(features)

#Data transformation
#turn data sets into data frames usable in dplyr
test_df <- tbl_df(X_test)

train_df <- tbl_df(X_train)

Test_ID <- tbl_df(subject_test)

Train_ID <- tbl_df(subject_train)

#rename column of Test_ID and Train_ID before merging them to the data set test_df and train_df
#This variable is how the test and training set will be merged under common variable 'ID'
Test_ID_Renamed<- rename(Test_ID, ID = V1)
Train_ID_Renamed<- rename(Train_ID, ID = V1) 

#Combine data sets test_df and train_df to their respective renamed ID columns 
test_df_id<-bind_cols(Test_ID_Renamed, test_df)

train_df_id <- bind_cols(Train_ID_Renamed, train_df)

#Combine data sets in previous step with their ID's attached to create a merged file of data sets
merged_data<- bind_rows(train_df_id, test_df_id)

#Arrange data by ID number
merged_in_order_of_id<- arrange(merged_data, ID)

#Renames variable names in the data set using the features.txt file for variable names
merged_data_renamed<- rename(merged_in_order_of_id, "tBodyAcc-mean()-X" = V1, "tBodyAcc-mean()-Y" = V2, "tBodyAcc-mean()-Z" = V3,
"tBodyAcc-std()-X" = V4, "tBodyAcc-std()-Y" = V5, "tBodyAcc-std()-Z" = V6, "tBodyAcc-mad()-X" = V7,"tBodyAcc-mad()-Y" = V8,
"tBodyAcc-mad()-Z" = V9,"tBodyAcc-max()-X"= V10 )


#Calculations 
# 2.Extracts only the measurements on the mean and standard deviation for each measurement.
Averages_merged<- summarize(merged_in_order_of_id,  V1 = mean(V1))
Standard_deviations_merged<- summarize(merged_data, V1 = sd(V1))

#AVERAGE of each variable based upon subject(ID).
Individuals <- group_by(merged_in_order_of_id, ID)
Individual_averages <- summarize(Individuals, V1 = mean(V1), V2 = mean(V2), V3 = mean(V3), V4 = mean(V4), V5 = mean(V5),
                               V6= mean(V6), V7 = mean(V7), V8 = mean(V8), V9 = mean(V9), V10 = mean(V10))

#Renames variable names in the data set using the features.txt file for variable names
Individual_averages_renamed<- rename(Individual_averages, "tBodyAcc-mean()-X" = V1, "tBodyAcc-mean()-Y" = V2, "tBodyAcc-mean()-Z" = V3,
                             "tBodyAcc-std()-X" = V4, "tBodyAcc-std()-Y" = V5, "tBodyAcc-std()-Z" = V6, "tBodyAcc-mad()-X" = V7,"tBodyAcc-mad()-Y" = V8,
                             "tBodyAcc-mad()-Z" = V9,"tBodyAcc-max()-X"= V10 )
