library('class') # loading Package 'class' for KNN algorithm
library('caret')# loading Package 'caret' for confusion matrix for validation of the knn model
library('catools')#feature Scaling

diabetes = read.csv(file.choose()) # loading the data 
View(diabetes)
sum(is.na(diabetes))#0 NA values
class(diabetes$mass) # class() to know the datatype of the feature mass in diabetes dataset
str(diabetes) # str() to know the structure of the data
summary(diabetes)# gives the info about mean, median and other descriptive measures
diabetes$class=factor(diabetes$class,c(0,1))# converting the 'class' feature into factor type
str(diabetes) # checking the changes made to 'class' feature
mean(diabetes$age) # finding the mean age of patients
train=diabetes[1:500,] # building training data to train knn
test=diabetes[501:768,] # test data to test the performance
split=sample.split(diabetes$class,SplitRatio = 0.70)
View(split)
train_split=subset(diabetes,split==TRUE)# building training data to train knn
test_split=subset(diabetes,split==FALSE)# test data to test the performance
View(train_split)
View(test_split)
pred_test=knn(train_split[,-9],test_split[,-9],train_split$class,k=2) # After training on the train data we calculating the output labels for the test data for k=2.
pred_test # to see the output labels
confusion=table(pred_test,test_split$class) # table() gives us the correct and incorrect predictions
View(confusion)
sum(diag(confusion))/nrow(test_split) # this gives us the accuracy of the model on the test data
confusionMatrix(pred_test,test_split$class) # Confusion matrix gives us the accuracy, sensitivity and other measures which helps us to interpret the model.
CrossTable()
