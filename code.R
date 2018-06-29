library("dplyr")
set.seed(1)
data<-read.csv("C:\\R-Programming\\PASSNYC\\data.csv",stringsAsFactors = FALSE,na.strings = c("N/A","NA",""))
##Let's add a missing tag in Ratings which are NA
colIndex<-grep("Rating",names(data))
temp<-data[,colIndex]
temp<-as.data.frame(apply(data[,colIndex],2,function(x) ifelse(is.na(x),"Missing",x)))
df<-cbind(temp,data$Rigorous.Instruction..)

colIndex<-grep("Percent",names(data))
temp<-data[,colIndex]
temp<-as.data.frame(apply(data[,colIndex],2,function(x) gsub("%", "", x)))
df<-cbind(df,temp)

df$Rigorous.Instruction..<-NULL

colIndex<-grep("\\.\\.",names(data))
temp<-data[,colIndex]
temp<-as.data.frame(apply(data[,colIndex],2,function(x) gsub("%", "", x)))
df<-cbind(df,temp)

colIndex<-grep("Average",names(data))
temp<-data[,colIndex]
df<-cbind(df,temp)

##Let's try to Engineer some new variables

quantiliesELA<-quantile(data$Average.ELA.Proficiency,na.rm = TRUE,probs=seq(0,1,0.1))
bad.ELA<-ifelse(df$Average.ELA.Proficiency<quantiliesELA[[2]],1,0)

quantilesMath<-quantile(data$Average.Math.Proficiency,na.rm = TRUE,probs=seq(0,1,0.1))
bad.Math<-ifelse(df$Average.Math.Proficiency<quantilesMath[[2]],1,0)
df<-cbind(df,bad.ELA=bad.ELA)
df<-cbind(df,bad.Math=bad.Math)

colIndex<-grep("Percent",names(df))
for(i in colIndex){
  df[,i]<-as.numeric(df[,i])
}

colIndex<-grep("\\.\\.",names(df))
for(i in colIndex){
  df[,i]<-as.numeric(df[,i])
}

colIndex<-grep("Average",names(df))
for(i in colIndex){
  df[,i]<-as.numeric(df[,i])
}

df$direNeed<-ifelse(df$bad.Math+df$bad.ELA==2,1,0)
df$bad.Math<-as.factor(df$bad.Math)
df$bad.ELA<-as.factor(df$bad.ELA)
df$direNeed<-as.factor(df$direNeed)
df$Economic.Need.Index<-data$Economic.Need.Index
df$Community.School<-data$Community.School.
df$Latitude<-data$Latitude
df$Longitude<-data$Longitude
df$City<-data$City
df$School.Income.Estimate<-data$School.Income.Estimate

##I have engineered a total of 3 factors here
##1.If the ELA scores have been below 10 percentile value
##2.If the Math scores have been below 10 percentile value
##3.If both(Math and ELA)scores are below 10 percentile values(Very bad case)


df$Percent.Black...Hispanic<-NULL

library(xgboost)
library(caTools)
library(Matrix)
library(magrittr)
library(data.table)
library(caret)
library(rpart)
library(rpart.plot)
library(mice)
library(missForest)
library(cluster)
library(flexclust)
library(ggplot2)
df1<-as.data.frame(lapply(df,unlist))

data<-df1

colIndex<-grep("Rating",names(data))
for(i in colIndex){
  levels(data[,i])<-(c("Missing","Not Meeting Target","Approaching Target","Meeting Target","Exceeding Target"))
  data[,i]<-as.numeric(data[,i])
}

data$Student.Achievement.Rating<-as.numeric(data[,i])
##exploratory analysis on columns with "Ratings"
for(i in colIndex){
  col.name<-names(data)[i]
  title<-paste("Student Achievement Rating Vs",col.name)
  explore<-data %>% group_by_(col.name) %>% summarise(point=median(Student.Achievement.Rating))
  explore<-as.data.frame(explore)
  g<-ggplot(data=explore, aes(x=explore[,1], y=point)) +
    geom_bar(stat="identity") +xlab(col.name)+ylab("Student Achievement Rating") + labs(title=title)
  suppressMessages(print(g))
}

colIndex<-seq(9,13,1)
for(i in colIndex){
  ApplyQuintiles <- function(x) {
    cut(x, breaks=c(quantile(data[,i], probs = seq(0, 1, by = 0.25))), 
        labels=c("Small","Medium","Large","Very Large"),include.lowest = TRUE)
  }
  data[,i] <- sapply(data[,i], ApplyQuintiles)
  data[,i]<-as.numeric(data[,i])
}
for(i in colIndex){
  col.name<-names(data)[i]
  title<-paste("Student Achievement Rating Vs",col.name)
  explore<-data %>% group_by_(col.name) %>% summarise(point=mean(Student.Achievement.Rating))
  explore<-as.data.frame(explore)
  g<-ggplot(data=explore, aes(x=explore[,1], y=point)) +
    geom_bar(stat="identity") +xlab(col.name)+ylab("Student Achievement Rating") + labs(title=title)
  suppressMessages(print(g))
}

##Let's see how are the schools with different Economic Index perform 
explore<-data %>% group_by(Student.Achievement.Rating) %>% summarise(point=median(Economic.Need.Index,na.rm = TRUE))
explore<-as.data.frame(explore)
g<-ggplot(data=explore, aes(x=explore[,1], y=point)) +
  geom_bar(stat="identity") +xlab("Student.Achievement.Rating")+ylab("Mean Economic Need Index") + labs(title="Student.Achievement.Rating Vs Mean Economic Need Index")
suppressMessages(print(g))


##Lets see geographic impact on Student Ratings
g<-ggplot(data=data, aes(x=Latitude, y=Longitude)) + geom_point(aes(color=Student.Achievement.Rating))+ xlab("Latitude")+ylab("Longitude") + labs(title="Effect of Latitude and Longitude on Student Achievement Rating")
suppressMessages(print(g))

##Lets see geographic impact on Economic Need Index
g<-ggplot(data=data, aes(x=Latitude, y=Longitude)) + geom_point(aes(color=Economic.Need.Index))+ xlab("Latitude")+ylab("Longitude") + labs(title="Effect of Latitude and Longitude on Student Achievement Rating")
suppressMessages(print(g))

data$School.Income.Estimate<-gsub("\\$","",data$School.Income.Estimate)
data$School.Income.Estimate<-gsub(",","",data$School.Income.Estimate)
data$School.Income.Estimate<-as.numeric(data$School.Income.Estimate)

##Let's see how are the schools with different School Income perform 
explore<-data %>% group_by(Student.Achievement.Rating) %>% summarise(point=median(School.Income.Estimate,na.rm = TRUE))
explore<-as.data.frame(explore)
g<-ggplot(data=explore, aes(x=explore[,1], y=point)) +
  geom_bar(stat="identity") +xlab("Student.Achievement.Rating")+ylab("Median School Income") + labs(title="Student.Achievement.Rating Vs Median School Income")
suppressMessages(print(g))

##More Money into the school does not necessarily makes student's results good
##Clearly there is a trend with location and Economic Need Index and Student Ratings(Data points are getting clustered around different spots)

preproc<-preProcess(data)
preprocesseddata<-predict(preproc,data)
dist<-dist(preprocesseddata)
hclust<-hclust(dist,method="ward.D")
plot(hclust)

##Going with 5 clusters as I want more than 1 factor to decide whether the school needs help or not
##Could have gone with 2/3 clusters as well(but that would have given very few segregation within the data)
clusters<-cutree(hclust,k=5)


data1<-subset(data,clusters==1)
data2<-subset(data,clusters==2)
data3<-subset(data,clusters==3)
data4<-subset(data,clusters==4)
data5<-subset(data,clusters==5)

explore<-data1 %>% group_by(Student.Achievement.Rating) %>% summarise(median(Economic.Need.Index,na.rm = TRUE)) 
print(explore)
explore<-data2 %>% group_by(Student.Achievement.Rating) %>% summarise(median(Economic.Need.Index,na.rm = TRUE)) 
print(explore)
explore<-data3 %>% group_by(Student.Achievement.Rating) %>% summarise(median(Economic.Need.Index,na.rm = TRUE)) 
print(explore)
explore<-data4 %>% group_by(Student.Achievement.Rating) %>% summarise(median(Economic.Need.Index,na.rm = TRUE)) 
print(explore)
explore<-data5 %>% group_by(Student.Achievement.Rating) %>% summarise(median(Economic.Need.Index,na.rm = TRUE)) 
print(explore)

table(data1$Student.Achievement.Rating)
table(data2$Student.Achievement.Rating)
table(data3$Student.Achievement.Rating)
table(data4$Student.Achievement.Rating)
table(data5$Student.Achievement.Rating)