a=c(1,2,NA,3,NA,4,5)

is.na(a)

a=c(1,2,NA,3,NA,4,5,0/0)
is.nan(a)#A NaN value is also NA but the converse is not true
#NaN means "not a number" and it means there is a result, 
#but it cannot be represented in the computer. 
#The second, NA , explains that the data is just missing for unknown reasons.
#extract elements except NA
x <- c(100, 200, NA, 300,NA, 400)
b <- is.na(x)
c=x[!b]

#complete the cases
x <- c(100, 200, NA, 400, NA, 500)
y <- c("x", "y", NA, "z", NA, "w")
c<-complete.cases(x,y)
x[c]
y[c]

#complete the data frame
x <- c(100, 200, NA, 400, NA, 500)
y <- c("x", "y", NA, "z", NA, "w")
z<- c("a","b","c","d","e","f")
d<-data.frame(x,y,z)
d[complete.cases(d),]
