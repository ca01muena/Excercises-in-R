#Read data
setwd('/Users/claudiomuena/Desktop/Data Science R')
train<-read.csv("train.csv",header = TRUE)
test<-read.csv("test.csv",header= TRUE)

#Data functions
head(train) #show first 6 rows
head(as.factor(train$name))
test.survived<-data.frame(survived=rep("Zero",nrow(test)),test[,])
#--------Create a datafile(--Row name--Replicate(Row values, Row length)--,Old)

L3 <- LETTERS[1:20] # An array with the first 20 letters
char <- sample(L3, 15, replace = TRUE) #An array with n random elements from L3
demo <- data.frame(First_column = 1:5, second_column = 1:15, Third_column=char)

data.combined<-rbind(train,test.survived) #Combined data
str(data.combined) #Types of data of...
str(demo)
demo$Third_column<-as.factor(demo$Third_column) #Change types
data.combined$pclass<-as.factor(data.combined$pclass)
data.combined$survived<-as.factor(data.combined$survived)
train$survived<-as.factor(train$survived)
str(demo)
str(data.combined)


#Take a look at the data values
table(data.combined$survived)
table(data.combined$pclass)

#Graphics
library(ggplot2)
ggplot(train,aes(x=pclass,fill=factor(survived)))+
  geom_bar(width=0.5)+
  xlab("Pclass")+
  ylab("Total count")+
  labs(fill="Survived")
#This analysis reveals that it is likely to die for 3th class tickets.

#Let's see for Mr and Miss statistics
library(stringr)
misses<-data.combined[which(str_detect(data.combined$name,"Miss.")),]
misses[1:5,]
mrses<-data.combined[which(str_detect(data.combined$name,"Mrs.")),]
mrses[1:5,]
males<-data.combined[which(train$sex=="male"),]

#Create a function
extractTitle<-function(name){
  name<-as.character(name)
  
  if (length(grep("Miss.",name))>0){
    return("Miss.")
  } else if (length(grep("Master.",name))>0){
    return("Master.")
  } else if (length(grep("Mrs.",name))>0){
    return("Mrs.")
  } else if (length(grep("Mr.",name))>0){
    return("Mr.")
  } else {
    return("Other")
  }
}

titles<-NULL
for (i in 1:nrow(data.combined)) {
  titles<-c(titles,extractTitle(data.combined[i,"name"]))
} 
data.combined$title<-as.factor(titles)

#Plot to compare and analysis 
ggplot(data.combined[1:891,],aes(x=title,fill=survived))+
  geom_bar(width=0.5)+
  facet_wrap(~pclass)+
  ggtitle("Pclass")+
  xlab("Title")+
  ylab("Total Count")+
  labs(fill="Survived")


