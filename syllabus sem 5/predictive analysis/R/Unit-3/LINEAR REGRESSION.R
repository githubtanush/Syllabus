#Correlation values ranges between - 1 to + 1.
#With +1 shows highest positive correlation,
#while -1 represents highest negative correlation.
#-1 indicates a perfectly negative linear correlation between two variables
#0 indicates no linear correlation between two variables
#1 indicates a perfectly positive linear correlation between two variables
bank=read.csv(file.choose(),header=TRUE,sep=",",stringsAsFactors=FALSE)
my_data=bank[,c(1,3,4,5,6,7,8)]
head(my_data)
cor_mat=cor(my_data)#the cor() function takes the data frame(dataset) as the input and returns the correlation matrix as the result
print("Correlation matrix:")
print(cor_mat)
install.packages("corrplot")
library(corrplot)
corrplot(cor_mat,method="circle")
corrplot(cor_mat,method="number")
corrplot(cor_mat,method="pie")
corrplot(cor_mat,method="square")
corrplot(cor_mat,method="shade")

#--------------------------------------------------------------------------------------------------------------------------------------------------------------
#height predictor vector
x<-c(5.1,5.5,5.8,6.1,6.4,6.7,6.4,6.1,5.10,5.7)
#weight response vector
y<-c(63,66,69,72,75,78,75,72,69,66)
#lm()
relation<-lm(y~x)  #lm=linear model
summary(relation)
#find weight of a person with given height
a<-data.frame(x=6.3)
result=predict(relation,a)
print(result)
# Output and Interpretation
# The output of lm is an object of class "lm"
# This object contains several components, including:
# coefficients: A named vector of coefficients from the model.
# residuals: The residuals of the model, which are the differences
# between observed and fitted values.
# fitted.values: The predicted values based on the model.
# rank: The numeric rank of the fitted linear model.
# df.residual: The residual degrees of freedom.
z<-data.frame(x,y)
cor_mat=cor(z)
library(corrplot)
corrplot(cor_mat,method="circle")

#-------------------------------------------------------------------------------------------------------------------------
#insurance case study
insurance=read.csv(file.choose(),stringsAsFactors = TRUE)
str(insurance)
summary(insurance$charges)
hist(insurance$charges)
table(insurance$region)
cor_mat=cor(insurance[c("age","bmi","children","charges")])
corrplot(cor_mat,method="circle")
#cor is used to find the correlation between the columns of x and y
#regression model works on numeric data. rest of the columns are non numeric
#There is also a moderate positive correlation between age and charges, bmi
#and charges,and children
#and charges. These associations imply that as age, body mass, and
#number of children increase, the
#expected cost of insurance goes up.
x=insurance$age
y=insurance$charges
relation=lm(y~x)     #lm()->LINEAR MODEL
summary(relation)
a=data.frame(x=80)
result=predict(relation,a)
print(result)

#Pair and pair.panels
insurance=read.csv(file.choose(),stringsAsFactors = TRUE)
str(insurance)
summary(insurance$charges)
hist(insurance$charges)
table(insurance$region)
cor(insurance[c("age","bmi","children","charges")])
pairs(insurance[c("age","bmi","children","charges")])
install.packages("psych")
library(psych)
pairs.panels(insurance[c("age","bmi","children","charges")])
ins_model<-lm(charges~age+children+bmi+sex+smoker+region,data=insurance)
ins_pred<-predict(ins_model,data=insurance)
#same as above ins_model <- lm(charges ~ .,data=insurance)
ins_model
#to see the estimated beta coefficients
summary(ins_model)

#--------------------------------------------------------------------------------------------------------------------------------------------
#prostate cancer
ps=read.csv(file.choose(),stringsAsFactors = TRUE)
ps=ps[3:9]
View(ps)
str(ps)
summary(ps)
cor_mat=cor(ps)
corrplot(cor_mat,method="circle")
y=ps$area
x=ps$perimeter
relation=lm(y~x)     #lm()->LINEAR MODEL
summary(relation)
a=data.frame(x=70)
result=predict(relation,a)
print(result)
#Apply multilinear regression 
df=data.frame(radius=23,texture=12,perimeter=151,area=954,smoothness=0.143,compactness=0.278+symmetry=0.242+fractal_dimension=0.079)
ps_model<-lm(area~df,data=ps)
ps_pred<-predict(ps_model,data=ps)
ps_pred

#------------------------------------------------------------------------------------------------------------------------------------------------------
#Multilinear regression
#Startup dataset
dataset = read.csv(file.choose())
str(dataset)
View(dataset)
#checking for null values 
sum(is.na(dataset))
#handling categorical data
dataset$State = factor(dataset$State,levels = c('New York','Florida','California'),labels = c(1,2,3))
View(dataset)
library(caTools)
#splitting the dataset
split = sample.split(dataset$Profit,SplitRatio = 0.80)
training_set = subset(dataset,split = TRUE)
test_set = subset(dataset, split = FALSE)

#fitting multiple linear regression model
regressor = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend,data = training_set)
y_pred = predict(regressor,newdata = test_set)
y_pred

#prediction
df<-data.frame(R.D.Spend = 165349.2, Administration = 136897.80, Marketing.Spend = 471784.1)
result<-predict(regressor,df)
library(ggplot2)
ggplot(training_set,aes(Administration,Profit))+
  geom_smooth(method="lm")+
  geom_point(size=3)+
  theme_bw() + 
  xlab("R.D.Spend")+
  ylab("Profit") + 
  ggtitle("R&D Spend vs Profit")

#-------------------------------------------------------------------------------------------------------------------------------------------
#Polynomial Regression
#STEPS
 #read.csv
 #lm()
 #ggplot
 #data$a^2
 #lm
 #ggplot
 #data$a^3
 #lm
 #ggplot
 #continue till we find best fit
#POLYNOMIAL REGRESSION
dataset=read.csv(file.choose())
dataset=dataset[2:3]
View(dataset)
str(dataset)
lin_reg=lm(formula=Salary ~ .,data=dataset)
lin_reg
dataset$Level2=dataset$Level^2
dataset$Level3=dataset$Level^3
dataset$Level4=dataset$Level^4
dataset$Level5=dataset$Level^5
poly_reg=lm(formula=Salary ~ .,data=dataset)
library(ggplot2)
ggplot() + 
  geom_point(aes(x = dataset$Level, y = dataset$Salary), colour = 'red') + 
  geom_line(aes(x = dataset$Level, y = predict(lin_reg, newdata = dataset)), colour = 'blue') + 
  ggtitle('Truth or Bluff (Linear Regression)') + 
  xlab('Level') + 
  ylab('Salary')

ggplot() + 
  geom_point(aes(x = dataset$Level, y = dataset$Salary), colour = 'red') + 
  geom_line(aes(x = dataset$Level, y = predict(poly_reg, newdata = dataset)), colour = 'blue') + 
  ggtitle('Truth or Bluff (Linear Regression)') + 
  xlab('Level') + 
  ylab('Salary')
#Predicting a new result with Linear regression
predict(lin_reg,data.frame(Level=6.5))
#Predicting a new result with Polynomial regression
predict(poly_reg,data.frame(Level=6.5,Level2=6.5^2,Level3=6.5^3,Level4=6.5^4,Level5=6.5^5))

