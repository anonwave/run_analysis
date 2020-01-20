# run_analysis
README.MD  of Smartphones Tracking Human Activity

This repo explains how all of the scripts work and how they are connected.”


The data set uses the following files from UCI Machine Learning Repository URL

- 'features_info.txt': Shows information about the variables used on the feature file.

- 'features.txt': List of all variables listed and needed for column headers.

- 'train/X_train.txt': Training set.

- 'train/y_train.txt': Training labels.

- 'test/X_test.txt': Test set.

- 'test/y_test.txt': Test labels.


Variables, Data Sets, Transformations, and Study Design

Variables, data sets, and transformations created the in run_analysis.R file to merge the Test and Training data sets together:
•	rundata: a link / URL to the wearable data set provided by UCI Machine Learning Repository
•	X_test: uses read.table() to import the raw test data from X_test.txt 
•	X_train: uses read.table() to import the raw training data from X_train.txt
•	(V1:V561) in X_test and X_ train correspond to the column headers and values found in the features.txt file. 
•	subject_test: uses read.table() to import the test subjects data from subject_test.txt
•	subject_train: uses read.table() to import the training subjects data from subject_train.txt
•	V1 from subject_test and subject_train is a column of subjects
•	features:
•	test_df: turns X_test into a data frame using dplyr function tbl_df
•	train_df: turns X_train into a data frame using dplyr function tbl_df
•	Test_ID: turns subject_test into a data frame using dplyr function tbl_df
•	Train_ID: turns subject_train into a data frame using dplyr function tbl_df
•	Test_ID_Renamed: column V1 is renamed ID in the data set Test_ID 
•	Train_ID_Renamed: column V1 is renamed ID in the data set Train_ID
•	test_df_id: test_df and Test_ID_Renamed are joined using bind_cols()
•	train_df_id: train_df and Train_ID_Renamed are joined using bind_cols()
•	merged_data: Merges the two main data sets, train_df_id and test_df_id on the basis of ‘ID’
•	merged_in_order_of_id: Rearranges the data in order of subject number 
•	merged_data_renamed: this function renames the V1:V561 based of features.txt file
•	Averages_merged: this takes the averages of all columns in merged_data_renamed
•	Standard_deviations_merged: takes standard deviation of all columns merged_data_renamed
•	Individuals: groups data by individuals ID 
•	Individuals_averages: takes average of variables on an individual basis
•	Individuals_averages_renamed: renames Variables in data set Individuals_averages 

