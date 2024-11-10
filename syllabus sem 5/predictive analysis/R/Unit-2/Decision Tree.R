#Topic->Decision Tree
# USED FOR-> CLASSIFICATION AND REGRESSION
View(iris)

# ctree()
data=iris
indexes=sample(150,110)
iris_train=iris[indexes,]
iris_test=iris[indexes,]
iris_train_label=iris[indexes,5]
iris_test_label=iris[indexes,5]
target=Species ~ Sepal.Length+Sepal.Width+Petal.Length+Petal.Width
install.packages('party')
library(party)
tree=ctree(Species ~ .,data)
plot(tree,main="Tree for iris")
table(predict(tree),iris$Species)

# rpart()
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)
set.seed(678)
s=sample(nrow(iris),100)
iris_train=iris[s,]
iris_test=iris[-s,]
iris_decision_tree_model=rpart(Species ~ .,data=iris_train,method="class")
plot(iris_decision_tree_model)
text(iris_decision_tree_model)
rpart.plot(iris_decision_tree_model)
rpart.plot(iris_decision_tree_model,type=4,extra=103)
iris_predict=predict(iris_decision_tree_model,iris_test,type="class")
iris_predict_table=table(iris_test[,5],iris_predict)
iris_predict_table
(iris_performance=sum(diag(iris_predict_table))/sum(iris_predict_table))*100

#c5.0
install.packages("ISLR")
library(ISLR)
attach(Carseats)
str(Carseats)
install.packages("C50")
library(C50)
library(rpart)
library(rpart.plot)
tree=C5.0(US ~ .,data=Carseats,trials=10)
#make predictions
table(predict(tree,newdata=Carseats),Carseats$US)
tree_ms3=rpart(US ~ .,Carseats,control=rpart.control(minsplit=3))
rpart.plot(tree_ms3,main="minsplit=3")


#--------------------------------------------------------------------------------
#case study
#prostate cancer
library(caTools)
library(rpart)
library(rpart.plot)

data = read.csv(file.choose())
View(data)

data = data[,-11]  
data = data[,-1]   

# Normalizing the dataset
fun = function(x){ (x - min(x)) / (max(x) - min(x)) }
data[,2:9] = as.data.frame(lapply(data[,2:9], fun))

# Splitting the dataset into training and test sets
set.seed(10)  # Setting seed for reproducibility
split = sample.split(data, SplitRatio = 0.7)
train_c1 = subset(data, split == TRUE)
test_c1 = subset(data, split == FALSE)

# Building the decision tree classifier
classifier_tree = rpart(diagnosis_result ~ ., data = train_c1, method = "class")

# Plot the decision tree
rpart.plot(classifier_tree)

# Predicting on the test set
y_pres = predict(classifier_tree, newdata = test_c1, type = "class")

# Confusion matrix
cm = table(test_c1$diagnosis_result, y_pres)

# Calculating accuracy
acc = sum(diag(cm)) / sum(cm)

# Displaying confusion matrix and accuracy
cm
acc
