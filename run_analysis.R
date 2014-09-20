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
    Main_TimeBodyAccStandardDevX                         = round(mean(TimeBodyAccStandardDevX                    ),6)  ,
    Main_TimeGravityAccStandardDevY                      = round(mean(TimeGravityAccStandardDevY                 ),6),
    Main_TimeBodyAccJerkStandardDevZ                     = round(mean(TimeBodyAccJerkStandardDevZ                ),6),
    Main_TimeBodyGyroJerkStandardDevX                    = round(mean(TimeBodyGyroJerkStandardDevX               ),6),
    Main_TimeGravityAccMagStandardDev                    = round(mean(TimeGravityAccMagStandardDev               ),6),
    Main_FrequencyBodyAccStandardDevX                    = round(mean(FrequencyBodyAccStandardDevX               ),6),
    Main_FrequencyBodyAccJerkStandardDevY                = round(mean(FrequencyBodyAccJerkStandardDevY           ),6),
    Main_FrequencyBodyGyroStandardDevZ                   = round(mean(FrequencyBodyGyroStandardDevZ              ),6),
    Main_FrequencyBodyBodyGyroJerkMagStandardDev         = round(mean(FrequencyBodyBodyGyroJerkMagStandardDev    ),6),
    Main_TimeGravityAccMeanValueX                        = round(mean(TimeGravityAccMeanValueX                   ),6),
    Main_TimeBodyAccJerkMeanValueY                       = round(mean(TimeBodyAccJerkMeanValueY                  ),6),
    Main_TimeBodyGyroMeanValueZ                          = round(mean(TimeBodyGyroMeanValueZ                     ),6),
    Main_TimeBodyAccMagMeanValue                         = round(mean(TimeBodyAccMagMeanValue                    ),6),
    Main_TimeBodyGyroJerkMagMeanValue                    = round(mean(TimeBodyGyroJerkMagMeanValue               ),6),
    Main_FrequencyBodyAccMeanFreqValueX                  = round(mean(FrequencyBodyAccMeanFreqValueX             ),6),
    Main_FrequencyBodyAccJerkMeanValueY                  = round(mean(FrequencyBodyAccJerkMeanValueY             ),6),
    Main_FrequencyBodyAccJerkMeanFreqValueZ              = round(mean(FrequencyBodyAccJerkMeanFreqValueZ         ),6),
    Main_FrequencyBodyGyroMeanFreqValueX                 = round(mean(FrequencyBodyGyroMeanFreqValueX            ),6),
    Main_FrequencyBodyAccMagMeanFreqValue                = round(mean(FrequencyBodyAccMagMeanFreqValue           ),6),
    Main_FrequencyBodyBodyGyroMagMeanFreqValue           = round(mean(FrequencyBodyBodyGyroMagMeanFreqValue      ),6),
    Main_TimeBodyAccStandardDevY                         = round(mean(TimeBodyAccStandardDevY                   ),6),
    Main_TimeGravityAccStandardDevZ                      = round(mean(TimeGravityAccStandardDevZ                ),6),
    Main_TimeBodyGyroStandardDevX                        = round(mean(TimeBodyGyroStandardDevX                  ),6),
    Main_TimeBodyGyroJerkStandardDevY                    = round(mean(TimeBodyGyroJerkStandardDevY              ),6),
    Main_TimeBodyAccJerkMagStandardDev                   = round(mean(TimeBodyAccJerkMagStandardDev             ),6),
    Main_FrequencyBodyAccStandardDevY                    = round(mean(FrequencyBodyAccStandardDevY              ),6),
    Main_FrequencyBodyAccJerkStandardDevZ                = round(mean(FrequencyBodyAccJerkStandardDevZ          ),6),
    Main_FrequencyBodyAccMagStandardDev                  = round(mean(FrequencyBodyAccMagStandardDev            ),6),
    Main_TimeBodyAccMeanValueX                           = round(mean(TimeBodyAccMeanValueX                     ),6),
    Main_TimeGravityAccMeanValueY                        = round(mean(TimeGravityAccMeanValueY                  ),6),
    Main_TimeBodyAccJerkMeanValueZ                       = round(mean(TimeBodyAccJerkMeanValueZ                 ),6),
    Main_TimeBodyGyroJerkMeanValueX                      = round(mean(TimeBodyGyroJerkMeanValueX                ),6),
    Main_TimeGravityAccMagMeanValue                      = round(mean(TimeGravityAccMagMeanValue                ),6),
    Main_FrequencyBodyAccMeanValueX                      = round(mean(FrequencyBodyAccMeanValueX                ),6),
    Main_FrequencyBodyAccMeanFreqValueY                  = round(mean(FrequencyBodyAccMeanFreqValueY            ),6),
    Main_FrequencyBodyAccJerkMeanValueZ                  = round(mean(FrequencyBodyAccJerkMeanValueZ            ),6),
    Main_FrequencyBodyGyroMeanValueX                     = round(mean(FrequencyBodyGyroMeanValueX               ),6),
    Main_FrequencyBodyGyroMeanFreqValueY                 = round(mean(FrequencyBodyGyroMeanFreqValueY           ),6),
    Main_FrequencyBodyBodyAccJerkMagMeanValue            = round(mean(FrequencyBodyBodyAccJerkMagMeanValue      ),6),
    Main_FrequencyBodyBodyGyroJerkMagMeanValue           = round(mean(FrequencyBodyBodyGyroJerkMagMeanValue     ),6),
    Main_TimeBodyAccStandardDevZ                         = round(mean(TimeBodyAccStandardDevZ                        ),6),
    Main_TimeBodyAccJerkStandardDevX                     = round(mean(TimeBodyAccJerkStandardDevX                    ),6),
    Main_TimeBodyGyroStandardDevY                        = round(mean(TimeBodyGyroStandardDevY                       ),6),
    Main_TimeBodyGyroJerkStandardDevZ                    = round(mean(TimeBodyGyroJerkStandardDevZ                   ),6),
    Main_TimeBodyGyroMagStandardDev                      = round(mean(TimeBodyGyroMagStandardDev                     ),6),
    Main_FrequencyBodyAccStandardDevZ                    = round(mean(FrequencyBodyAccStandardDevZ                   ),6),
    Main_FrequencyBodyGyroStandardDevX                   = round(mean(FrequencyBodyGyroStandardDevX                  ),6),
    Main_FrequencyBodyBodyAccJerkMagStandardDev          = round(mean(FrequencyBodyBodyAccJerkMagStandardDev         ),6),
    Main_TimeBodyAccMeanValueY                           = round(mean(TimeBodyAccMeanValueY                          ),6),
    Main_TimeGravityAccMeanValueZ                        = round(mean(TimeGravityAccMeanValueZ                       ),6),
    Main_TimeBodyGyroMeanValueX                          = round(mean(TimeBodyGyroMeanValueX                         ),6),
    Main_TimeBodyGyroJerkMeanValueY                      = round(mean(TimeBodyGyroJerkMeanValueY                     ),6),
    Main_TimeBodyAccJerkMagMeanValue                     = round(mean(TimeBodyAccJerkMagMeanValue                    ),6),
    Main_FrequencyBodyAccMeanValueY                      = round(mean(FrequencyBodyAccMeanValueY                     ),6),
    Main_FrequencyBodyAccMeanFreqValueZ                  = round(mean(FrequencyBodyAccMeanFreqValueZ                 ),6),
    Main_FrequencyBodyAccJerkMeanFreqValueX              = round(mean(FrequencyBodyAccJerkMeanFreqValueX             ),6),
    Main_FrequencyBodyGyroMeanValueY                     = round(mean(FrequencyBodyGyroMeanValueY                    ),6),
    Main_FrequencyBodyGyroMeanFreqValueZ                 = round(mean(FrequencyBodyGyroMeanFreqValueZ                ),6),
    Main_FrequencyBodyBodyAccJerkMagMeanFreqValue        = round(mean(FrequencyBodyBodyAccJerkMagMeanFreqValue       ),6),
    Main_FrequencyBodyBodyGyroJerkMagMeanFreqValue       = round(mean(FrequencyBodyBodyGyroJerkMagMeanFreqValue ),6),
    Main_TimeGravityAccStandardDevX                      = round(mean(TimeGravityAccStandardDevX         ),6),
    Main_TimeBodyAccJerkStandardDevY                     = round(mean(TimeBodyAccJerkStandardDevY        ),6),
    Main_TimeBodyGyroStandardDevZ                        = round(mean(TimeBodyGyroStandardDevZ           ),6),
    Main_TimeBodyAccMagStandardDev                       = round(mean(TimeBodyAccMagStandardDev          ),6),
    Main_TimeBodyGyroJerkMagStandardDev                  = round(mean(TimeBodyGyroJerkMagStandardDev     ),6),
    Main_FrequencyBodyAccJerkStandardDevX                = round(mean(FrequencyBodyAccJerkStandardDevX   ),6),
    Main_FrequencyBodyGyroStandardDevY                   = round(mean(FrequencyBodyGyroStandardDevY      ),6),
    Main_FrequencyBodyBodyGyroMagStandardDev             = round(mean(FrequencyBodyBodyGyroMagStandardDev),6),
    Main_TimeBodyAccMeanValueZ                           = round(mean(TimeBodyAccMeanValueZ              ),6),
    Main_TimeBodyAccJerkMeanValueX                       = round(mean(TimeBodyAccJerkMeanValueX          ),6),
    Main_TimeBodyGyroMeanValueY                          = round(mean(TimeBodyGyroMeanValueY             ),6),
    Main_TimeBodyGyroJerkMeanValueZ                      = round(mean(TimeBodyGyroJerkMeanValueZ         ),6),
    Main_TimeBodyGyroMagMeanValue                        = round(mean(TimeBodyGyroMagMeanValue           ),6),
    Main_FrequencyBodyAccMeanValueZ                      = round(mean(FrequencyBodyAccMeanValueZ         ),6),
    Main_FrequencyBodyAccJerkMeanValueX                  = round(mean(FrequencyBodyAccJerkMeanValueX     ),6),
    Main_FrequencyBodyAccJerkMeanFreqValueY              = round(mean(FrequencyBodyAccJerkMeanFreqValueY ),6),
    Main_FrequencyBodyGyroMeanValueZ                     = round(mean(FrequencyBodyGyroMeanValueZ        ),6),
    Main_FrequencyBodyAccMagMeanValue                    = round(mean(FrequencyBodyAccMagMeanValue       ),6),
    Main_FrequencyBodyBodyGyroMagMeanValue               = round(mean(FrequencyBodyBodyGyroMagMeanValue  ),6)       
)

write.table(step5_data, "step5_data.txt", row.name=FALSE) 

