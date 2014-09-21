# STEP 1: Merges the training and the test sets to create one data set.

library(data.table)

x_training   <- read.table("./train/X_train.txt");       
sub_training <- read.table("./train/subject_train.txt"); 
y_training   <- read.table("./train/Y_train.txt");      
all_training <-  cbind(x_training, sub_training, y_training) 

x_test    <- read.table("./test/X_test.txt");          
sub_test   <- read.table("./test/subject_test.txt"); 
y_test    <- read.table("./test/Y_test.txt");          
all_test  <- cbind (x_test, sub_test, y_test);          

all_data  <- rbind(all_training, all_test)               

features     = read.table("features.txt")
names(all_data) <- features[,2]
names(all_data)[562] <- "Subject"
names(all_data)[563] <- "Activity" 
names(all_data)

# STEP 2: Extracts only the measurements on the mean and standard deviation for each measurement. 
strmean <- grepl("mean.*$", as.character(features$V2))
strstd  <- grepl("std.*$" , as.character(features$V2))

colmean   <- subset(as.character(features$V2),strmean)
colstd    <- subset(as.character(features$V2),strstd)

coltot0    <- union(colstd, colmean)
colFin    <- c("Subject","Activity")
coltot    <- union(coltot0, colFin)
step2_data <- all_data[,coltot]
names(step2_data)


# STEP 3: Uses descriptive activity names to name the activities in the data set
label   <- read.table("./activity_labels.txt");       
step3_data <- merge(step2_data, label, by.x = "Activity", by.y = "V1")
step3_data <- step3_data[,!(names(step3_data) %in% "Activity")]
names(step3_data)[81] <- c("Activity")
names(step3_data)


# STEP 4: Appropriately labels the data set with descriptive variable names. 
label0 <- names(step3_data)
label1 <- gsub("\\(\\)", "", gsub( "mean()", "MeanValue",gsub( "std()", "StandardDev",gsub("meanFreq()","MeanFreqValue",gsub( "-", "",gsub( "^t", "Time",gsub( "^f", "Frequency",label0)))))))
names(step3_data) <- label1
step4_data <- step3_data
names(step4_data)

# STEP 5: From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

library(plyr)

step5_data <- ddply(
    step4_data, .(Subject, Activity), summarize,
    Mean_TimeBodyAccStandardDevX                         = round(mean(TimeBodyAccStandardDevX                    ),6)  ,
    Mean_TimeGravityAccStandardDevY                      = round(mean(TimeGravityAccStandardDevY                 ),6),
    Mean_TimeBodyAccJerkStandardDevZ                     = round(mean(TimeBodyAccJerkStandardDevZ                ),6),
    Mean_TimeBodyGyroJerkStandardDevX                    = round(mean(TimeBodyGyroJerkStandardDevX               ),6),
    Mean_TimeGravityAccMagStandardDev                    = round(mean(TimeGravityAccMagStandardDev               ),6),
    Mean_FrequencyBodyAccStandardDevX                    = round(mean(FrequencyBodyAccStandardDevX               ),6),
    Mean_FrequencyBodyAccJerkStandardDevY                = round(mean(FrequencyBodyAccJerkStandardDevY           ),6),
    Mean_FrequencyBodyGyroStandardDevZ                   = round(mean(FrequencyBodyGyroStandardDevZ              ),6),
    Mean_FrequencyBodyBodyGyroJerkMagStandardDev         = round(mean(FrequencyBodyBodyGyroJerkMagStandardDev    ),6),
    Mean_TimeGravityAccMeanValueX                        = round(mean(TimeGravityAccMeanValueX                   ),6),
    Mean_TimeBodyAccJerkMeanValueY                       = round(mean(TimeBodyAccJerkMeanValueY                  ),6),
    Mean_TimeBodyGyroMeanValueZ                          = round(mean(TimeBodyGyroMeanValueZ                     ),6),
    Mean_TimeBodyAccMagMeanValue                         = round(mean(TimeBodyAccMagMeanValue                    ),6),
    Mean_TimeBodyGyroJerkMagMeanValue                    = round(mean(TimeBodyGyroJerkMagMeanValue               ),6),
    Mean_FrequencyBodyAccMeanFreqValueX                  = round(mean(FrequencyBodyAccMeanFreqValueX             ),6),
    Mean_FrequencyBodyAccJerkMeanValueY                  = round(mean(FrequencyBodyAccJerkMeanValueY             ),6),
    Mean_FrequencyBodyAccJerkMeanFreqValueZ              = round(mean(FrequencyBodyAccJerkMeanFreqValueZ         ),6),
    Mean_FrequencyBodyGyroMeanFreqValueX                 = round(mean(FrequencyBodyGyroMeanFreqValueX            ),6),
    Mean_FrequencyBodyAccMagMeanFreqValue                = round(mean(FrequencyBodyAccMagMeanFreqValue           ),6),
    Mean_FrequencyBodyBodyGyroMagMeanFreqValue           = round(mean(FrequencyBodyBodyGyroMagMeanFreqValue      ),6),
    Mean_TimeBodyAccStandardDevY                         = round(mean(TimeBodyAccStandardDevY                   ),6),
    Mean_TimeGravityAccStandardDevZ                      = round(mean(TimeGravityAccStandardDevZ                ),6),
    Mean_TimeBodyGyroStandardDevX                        = round(mean(TimeBodyGyroStandardDevX                  ),6),
    Mean_TimeBodyGyroJerkStandardDevY                    = round(mean(TimeBodyGyroJerkStandardDevY              ),6),
    Mean_TimeBodyAccJerkMagStandardDev                   = round(mean(TimeBodyAccJerkMagStandardDev             ),6),
    Mean_FrequencyBodyAccStandardDevY                    = round(mean(FrequencyBodyAccStandardDevY              ),6),
    Mean_FrequencyBodyAccJerkStandardDevZ                = round(mean(FrequencyBodyAccJerkStandardDevZ          ),6),
    Mean_FrequencyBodyAccMagStandardDev                  = round(mean(FrequencyBodyAccMagStandardDev            ),6),
    Mean_TimeBodyAccMeanValueX                           = round(mean(TimeBodyAccMeanValueX                     ),6),
    Mean_TimeGravityAccMeanValueY                        = round(mean(TimeGravityAccMeanValueY                  ),6),
    Mean_TimeBodyAccJerkMeanValueZ                       = round(mean(TimeBodyAccJerkMeanValueZ                 ),6),
    Mean_TimeBodyGyroJerkMeanValueX                      = round(mean(TimeBodyGyroJerkMeanValueX                ),6),
    Mean_TimeGravityAccMagMeanValue                      = round(mean(TimeGravityAccMagMeanValue                ),6),
    Mean_FrequencyBodyAccMeanValueX                      = round(mean(FrequencyBodyAccMeanValueX                ),6),
    Mean_FrequencyBodyAccMeanFreqValueY                  = round(mean(FrequencyBodyAccMeanFreqValueY            ),6),
    Mean_FrequencyBodyAccJerkMeanValueZ                  = round(mean(FrequencyBodyAccJerkMeanValueZ            ),6),
    Mean_FrequencyBodyGyroMeanValueX                     = round(mean(FrequencyBodyGyroMeanValueX               ),6),
    Mean_FrequencyBodyGyroMeanFreqValueY                 = round(mean(FrequencyBodyGyroMeanFreqValueY           ),6),
    Mean_FrequencyBodyBodyAccJerkMagMeanValue            = round(mean(FrequencyBodyBodyAccJerkMagMeanValue      ),6),
    Mean_FrequencyBodyBodyGyroJerkMagMeanValue           = round(mean(FrequencyBodyBodyGyroJerkMagMeanValue     ),6),
    Mean_TimeBodyAccStandardDevZ                         = round(mean(TimeBodyAccStandardDevZ                        ),6),
    Mean_TimeBodyAccJerkStandardDevX                     = round(mean(TimeBodyAccJerkStandardDevX                    ),6),
    Mean_TimeBodyGyroStandardDevY                        = round(mean(TimeBodyGyroStandardDevY                       ),6),
    Mean_TimeBodyGyroJerkStandardDevZ                    = round(mean(TimeBodyGyroJerkStandardDevZ                   ),6),
    Mean_TimeBodyGyroMagStandardDev                      = round(mean(TimeBodyGyroMagStandardDev                     ),6),
    Mean_FrequencyBodyAccStandardDevZ                    = round(mean(FrequencyBodyAccStandardDevZ                   ),6),
    Mean_FrequencyBodyGyroStandardDevX                   = round(mean(FrequencyBodyGyroStandardDevX                  ),6),
    Mean_FrequencyBodyBodyAccJerkMagStandardDev          = round(mean(FrequencyBodyBodyAccJerkMagStandardDev         ),6),
    Mean_TimeBodyAccMeanValueY                           = round(mean(TimeBodyAccMeanValueY                          ),6),
    Mean_TimeGravityAccMeanValueZ                        = round(mean(TimeGravityAccMeanValueZ                       ),6),
    Mean_TimeBodyGyroMeanValueX                          = round(mean(TimeBodyGyroMeanValueX                         ),6),
    Mean_TimeBodyGyroJerkMeanValueY                      = round(mean(TimeBodyGyroJerkMeanValueY                     ),6),
    Mean_TimeBodyAccJerkMagMeanValue                     = round(mean(TimeBodyAccJerkMagMeanValue                    ),6),
    Mean_FrequencyBodyAccMeanValueY                      = round(mean(FrequencyBodyAccMeanValueY                     ),6),
    Mean_FrequencyBodyAccMeanFreqValueZ                  = round(mean(FrequencyBodyAccMeanFreqValueZ                 ),6),
    Mean_FrequencyBodyAccJerkMeanFreqValueX              = round(mean(FrequencyBodyAccJerkMeanFreqValueX             ),6),
    Mean_FrequencyBodyGyroMeanValueY                     = round(mean(FrequencyBodyGyroMeanValueY                    ),6),
    Mean_FrequencyBodyGyroMeanFreqValueZ                 = round(mean(FrequencyBodyGyroMeanFreqValueZ                ),6),
    Mean_FrequencyBodyBodyAccJerkMagMeanFreqValue        = round(mean(FrequencyBodyBodyAccJerkMagMeanFreqValue       ),6),
    Mean_FrequencyBodyBodyGyroJerkMagMeanFreqValue       = round(mean(FrequencyBodyBodyGyroJerkMagMeanFreqValue ),6),
    Mean_TimeGravityAccStandardDevX                      = round(mean(TimeGravityAccStandardDevX         ),6),
    Mean_TimeBodyAccJerkStandardDevY                     = round(mean(TimeBodyAccJerkStandardDevY        ),6),
    Mean_TimeBodyGyroStandardDevZ                        = round(mean(TimeBodyGyroStandardDevZ           ),6),
    Mean_TimeBodyAccMagStandardDev                       = round(mean(TimeBodyAccMagStandardDev          ),6),
    Mean_TimeBodyGyroJerkMagStandardDev                  = round(mean(TimeBodyGyroJerkMagStandardDev     ),6),
    Mean_FrequencyBodyAccJerkStandardDevX                = round(mean(FrequencyBodyAccJerkStandardDevX   ),6),
    Mean_FrequencyBodyGyroStandardDevY                   = round(mean(FrequencyBodyGyroStandardDevY      ),6),
    Mean_FrequencyBodyBodyGyroMagStandardDev             = round(mean(FrequencyBodyBodyGyroMagStandardDev),6),
    Mean_TimeBodyAccMeanValueZ                           = round(mean(TimeBodyAccMeanValueZ              ),6),
    Mean_TimeBodyAccJerkMeanValueX                       = round(mean(TimeBodyAccJerkMeanValueX          ),6),
    Mean_TimeBodyGyroMeanValueY                          = round(mean(TimeBodyGyroMeanValueY             ),6),
    Mean_TimeBodyGyroJerkMeanValueZ                      = round(mean(TimeBodyGyroJerkMeanValueZ         ),6),
    Mean_TimeBodyGyroMagMeanValue                        = round(mean(TimeBodyGyroMagMeanValue           ),6),
    Mean_FrequencyBodyAccMeanValueZ                      = round(mean(FrequencyBodyAccMeanValueZ         ),6),
    Mean_FrequencyBodyAccJerkMeanValueX                  = round(mean(FrequencyBodyAccJerkMeanValueX     ),6),
    Mean_FrequencyBodyAccJerkMeanFreqValueY              = round(mean(FrequencyBodyAccJerkMeanFreqValueY ),6),
    Mean_FrequencyBodyGyroMeanValueZ                     = round(mean(FrequencyBodyGyroMeanValueZ        ),6),
    Mean_FrequencyBodyAccMagMeanValue                    = round(mean(FrequencyBodyAccMagMeanValue       ),6),
    Mean_FrequencyBodyBodyGyroMagMeanValue               = round(mean(FrequencyBodyBodyGyroMagMeanValue  ),6)       
)

write.table(step5_data, "step5_data.txt", row.name=FALSE) 

