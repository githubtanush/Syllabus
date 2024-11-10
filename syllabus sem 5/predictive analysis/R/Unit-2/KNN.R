#K-NN-> K nearest neighbour
#Used for classification and non-parameterized
#Supervised learning algorithm
#Lazy learner algorithm->It never learns anything it always stores everything
#Computation cost is high
#Euclidean distance formula we will used
#good for large dataset

#-------------------------------------------------------------------------------------------------------------------------
#FIRST EXAMPLE

#Data Collection
#Create a dataframe
x<-data.frame(CA1=c(25,20,15,20,30,10),CA2=c(30,15,15,20,10,20),GRADE=c('A','B','B','A','B',"B"))
x$GRADE=factor(x$GRADE)
#Data Exploration
View(x)
str(x)
summary(x)

#Train and Test variable
install.packages("caTools")
library(caTools)
set.seed(123)
split=sample.split(x,SplitRatio=0.70)
train_split=subset(x,split==TRUE)
test_split=subset(x,split==FALSE)
View(train_split)
View(test_split)


train_v=train_split[,1:2]
test_v=test_split[,1:2]
train_n=train_split[,3]
test_n=test_split[,3]
install.packages("caret")
library(class)
library(caret)
y_pred=knn(train_v,test_v,train_n,k=3)
cm=table(test_n,y_pred)

cm1=confusionMatrix(test_n,y_pred)

View(cm)
#------------------------------------------------------------------------------------------------------------------------------------
#SECOND EXAMPLE
#CANCER DATASET

wdbc=read.table(file.choose(),sep=',',header=TRUE)
wdbc=wdbc[,-1]         
View(wdbc)
summary(wdbc)
str(wdbc)
data_norm=function(x) {  (x-min(x))/((max(x)-min(x))) }        
wdbc_norm=as.data.frame(lapply(wdbc[2:31],data_norm))
wdbc_train=wdbc_norm[1:450,]
wdbc_test=wdbc_norm[451:569,]
library(class)
wdbc_pred=knn(wdbc_train,wdbc_test,wdbc[1:450,1],k=21)
cm=table(wdbc_pred,wdbc[451:569,1])
a=sum(cm[1],cm[4])
acc=a/sum(cm)
acc
cm


#-----------------------------------------------------------------------------------------------------------------------------------------------
#Feature Scaling

#1)
#Standardization method using scale function
set.seed(123)
data<-data.frame(Age=rnorm(5,50,8),Weight=rnorm(5,80,10))
data<-as.data.frame(scale(data))
data

a=mean(data$Age)
b=sd(data$Age)
c=data$Age[1]-a
d=c/b
d


#2)
#Manual Method
data<-as.data.frame(sapply(data,function(x)(x-mean(x))/sd(x)))
data 

#normalization method
set.seed(123)
data<-data.frame(Age=rnorm(5,50,8),Weight=rnorm(5,80,10))
data<-as.data.frame(sapply(data,function(x)(x-min(x))/(max(x)-min(x))))
data


#3)
#Using Caret Package
library(caret)
set.seed(123)
data<-data.frame(Age=rnorm(500,50,8),Weight=rnorm(500,80,10))
data.pre<-preProcess(data,method=c("center","scale"))
data

#normalization
data<-data.frame(Age=rnorm(500,50,8),Weight=rnorm(500,80,10))
data.pre<-preProcess(data,method="range")
data<-predict(data.pre,data)
data

#------------------------------------------------------------------------------------------------------------------------
#ACTIVITY
dataset=read.csv(file.choose())
View(dataset)
dataset=dataset[3:5]
dataset$Purchased=factor(dataset$Purchased,levels=c(0,1))
library(caTools)
set.seed(123)
split=sample.split(dataset$Purchased,SplitRatio = 0.75)
training_set=subset(dataset,split==TRUE)
test_set=subset(dataset,split==FALSE)
#Feature Scaling
training_set[-3]=scale(training_set[-3])
View(training_set)
test_set[-3]=scale(test_set[-3])
View(test_set)
library(class)
y_pred=knn(train=training_set[,-3],test=test_set[,-3],cl=training_set[,3],k=5)
cm=table(test_set[,3],y_pred)
View(cm)

#-----------------------------------------------------------------------------------------------------------------------------------
#CASE STUDY
#CANCER DATASET
#k=3,4,5 srroot(record) sqoot(record-trainvalue) accuracy=?
#train2test = 60:40,70:30,80:20  65:35,75:25,85:15   accuracy=?
data=read.csv(file.choose())
data[is.na(data)] <- 0
View(data)
data=data[-1]
data=data[-10]
data$diagnosis_result=factor(data$diagnosis_result,c("M","B"))
fun=function(x){(x-min(x))/(max(x)-min(x))}
data[-1]=as.data.frame(lapply(data[-1],fun))
View(data)
library(caTools)
library(class)
split=sample.split(data$diagnosis_result,SplitRatio = 0.70)
View(split)
train_split=subset(data,split==TRUE)
test_split=subset(data,split==FALSE)
View(train_split)
View(test_split)
y_pred=knn(train_split[,-1],test_split[,-1],train_split[,1],k=4)
cm=table(test_split[,1],y_pred)
View(cm)
s=sum(cm[1],cm[4])
acc=s/sum(cm)
acc
#-------------------------------------------------------------------------------------------------------------
data=read.csv(file.choose())
data[is.na(data)] <- 0
View(data)
data=data[-1]
data=data[-10]
data$diagnosis_result=factor(data$diagnosis_result,c("M","B"))
fun=function(x){(x-min(x))/(max(x)-min(x))}
data[-1]=as.data.frame(lapply(data[-1],fun))
View(data)
library(caTools)
library(class)
split=sample.split(data$diagnosis_result,SplitRatio = 0.70)
View(split)
train_split=subset(data,split==TRUE)
test_split=subset(data,split==FALSE)
View(train_split)
View(test_split)
y_pred=knn(train_split[,-1],test_split[,-1],train_split[,1],k=4)
cm=table(test_split[,1],y_pred)
cm
View(cm)
s=sum(cm[1],cm[4])
acc=s/sum(cm)
acc


