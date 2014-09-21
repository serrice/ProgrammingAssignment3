## README file for Getting and Cleaning Data Course Project

### How run_analysis.R works
The script is separated into five points, one for each step required by the course project:

 * **Step 1:** After importing the package "data.table" I imported the data using the function read.table. The data were then aggregated with the functions cbind separately for training and testing, and finally I connected everything with the function rbind;
 
 * **Step 2:** Through the use of the function grepl I extracted only the measurements on the mean and standard       deviation. I created a vector with the union list of two types of field by adding the fields "Subject" and "Activity", I finally extracted only those fields from the total data set.
 
 * **Step 3:** I imported the names of the "Activity" in a data frame and then crossed with the total data frame.

 * **Step 4:** I have changed the names of the fields in order to ensure readability and comprehension.
 
 * **Step 5:** To aggregate the data for Subject and Activity I imported the package plyr and then used the function ddply using the mean for all variables in the data set.

### Code Book for variables
Contrary to the indications of the file features_inf.txt to improve their reading, I made the following changes to the names of variables:

* f             --> Frequency;
* t             --> Time;
* meanFreq()    --> MeanFreqValue;
* std()         --> StandardDev;
* mean()        --> MeanValue;





