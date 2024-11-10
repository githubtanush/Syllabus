#data preprocessing
#importing the dataset
dataset = read.csv(file.choose())
str(dataset)
summary(dataset)
View(dataset)
#labelling the variables
names(dataset)<-c("Country","Age","Salary","Purchased")
str(dataset)

#Checking missing values
sum(is.na(dataset))
View(dataset)

colSums(is.na(dataset))
missingdata<-dataset[!complete.cases(dataset), ]
sum(is.na(missingdata))
View(dataset)

#taking care of missing values
dataset$Age=ifelse(is.na(dataset$Age),
                   ave(dataset$Age, FUN= function(x) mean(x,na.rm=TRUE)),
                   dataset$Age)
dataset$Salary=ifelse(is.na(dataset$Salary),
                   ave(dataset$Salary, FUN= function(x) mean(x,na.rm=TRUE)),
                   dataset$Salary)
sum(is.na(dataset))
View(dataset)

str(dataset)

#handling the categorical values
dataset$Country= factor(dataset$Country, 
                        levels=c('France','Spain','Germany'),
                        labels= c(1,2,3))
dataset$Purchased= factor(dataset$Purchased, 
                        levels=c('No','Yes'),
                        labels= c(0,1))
str(dataset)
View(dataset)

