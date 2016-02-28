#set the appropriate path
setwd("C:/Users/lycnb/Specilization in data science/getting and cleaning/wearable computing")
#load packages
packages<-c("data.table","reshape2");
sapply(packages,require,character.only=TRUE,quietly= TRUE)

#1) read the tables
subject_test<-read.table("subject_test.txt")
subject_train<-read.table("subject_train.txt")
x_test<-read.table("X_test.txt")
x_train<-read.table("X_train.txt")
y_test<-read.table("y_test.txt")
y_train<-read.table("y_train.txt")

#2 merge the data set
x_combine<-rbind(x_train,x_test)
y_combine<-rbind(y_train,y_test)
subject_combine<-rbind(subject_train,subject_test)
#renmae subject and activities
setnames(subject_combine,"V1","subject")
setnames(y_combine,"V1","activity")
data<-cbind(subject_combine,y_combine,x_combine)
#3 extract the mean and std from feature
#read feature
feature<-read.table("features.txt")
setnames(feature,names(feature),c("featureNum","featureName"))
#subset feature by mean() and sd()
feature<-feature[grepl("mean|std",feature$featureName),]
#subset the data by feature including mean() and sd()
feature$select<-paste("V",feature$featureNum,sep="")
data1<-data[,c("subject","activity",feature$select)]
#4 rename the activities
replace1<-function(x){
  if (x==1){y<-"WALKING"}
  else if (x==2){y<-"WALKING_UPSTAIRS"}
  else if (x==3){y<-"WALKING_UPSTAIRS"}
  else if (x==4){y<-"WALKING_DOWNSTAIRS"}
  else if (x==5){y<-"SITTING"}
  else {y<-"LAYING"}
  y
}?
data1$activity<-sapply(data1$activity,replace1)
#5 rename the variables
names(data1)[3:ncol(data1)]<-as.character(feature$featureName)
#6 convert it to data table, take the average
data1<-data.table(data1)
setkey(data1,subject,activity)
#.SD include all columns except the key columns
data2 <- data1[,lapply(.SD,mean),by = key(data1)]
#8 create tidy data set
write.table(data2, "tidy.txt", row.names = FALSE, quote = FALSE) 



