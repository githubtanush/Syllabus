# Implementation of KNN in R
# ___________________METADATA____________________

# The data is pima-indians-diabetes.csv
# This data describes the medical records for Pima Indians
# and whether or not each patient will have an onset of diabetes within some years.
# Fields description follow:
# preg = Number of times pregnant
# plas = Plasma glucose concentration a 2 hours in an oral glucose tolerance test
# pres = Diastolic blood pressure (mm Hg)
# skin = Triceps skin fold thickness (mm)
# test = 2-Hour serum insulin (mu U/ml)
# mass = Body mass index (weight in kg/(height in m)^2)
# pedi = Diabetes pedigree function
# age = Age (years)
# class = Class variable (1:tested positive for diabetes, 0: tested negative for diabetes)

#  ______________Questions__________________

# Q.1. What is the data type of column mass?
# Q.2. What is the mean age of patients?
# Now, Divide the Data into train and test part where, 
# train part contains first 500 observations and test part contains last 268 observations
# and fit a KNN with K=2 and answer the following questions:
# Q.3. Plot a confusion matrix and find the number of patients 
# correctly predicted as 'negative' for diabetes
# Q.4. What is the accuracy of the model for the test data ?
# Q.5. Total number of patients predicted as positive for diabetes?
# ________________CODE____________________________

rm(list=ls()) # to clear all variables
setwd('D:/') # setting working directory to 'D' drive where data is present
library('class') # loading Package 'class' for KNN algorithm
library('caret') # loading Package 'caret' for confusion matrix for validation of the knn model
# install.packages('caret', dependencies = TRUE) You can install packages in case they are not present.
diabetes = read.csv('pima-indians-diabetes.csv') # loading the data 
class(diabetes$mass) # class() to know the datatype of the feature mass in diabetes dataset
str(diabetes) # str() to know the structure of the data
diabetes[,'class']=factor(diabetes[,'class']) # converting the 'class' feature into factor type
str(diabetes) # checking the changes made to 'class' feature
mean(diabetes$age) # finding the mean age of patients
summary(diabetes) # gives the info about mean, median and other descriptive measures
train=diabetes[1:500,] # building training data to train knn
test=diabetes[501:768,] # test data to test the performance
pred_test=knn(train[,-9],test[,-9],train$class,k=2) # After training on the train data we calculating the output labels for the test data for k=2.
pred_test # to see the output labels
confusion=table(pred_test,test$class) # table() gives us the correct and incorrect predictions
sum(diag(confusion))/nrow(test) # this gives us the accuracy of the model on the test data
confusionMatrix(pred_test,test$class) # Confusion matrix gives us the accuracy, sensitivity and other measures which helps us to interpret the model.
# ____________________END______________________
