a=data.frame(s.no=seq(1:10),marks=seq(1:10))
# 1) apply
apply(a,1,sum)# row wise sum
apply(a,2,sum)# column wise sum
# 2) lapply : output is "LIST" type
lapply(a,sum)
b=list(1:10)
lapply(b,mean)
# 3) sapply : output is "VECTOR" type
c=c("This","is","a","vector")
sapply(c,nchar)
sapply(a,sum)
# 4) mapply : multivariante version of sapply
mapply(sum,a)
mapply(mean,b)
mapply(sum,1:3,2:4,3:5)
# 5) tapply : I/P: Vectors only
attach(iris)
tapply(iris$Sepal.Length,iris$Species,mean)
attach(cars)
tapply(cars$speed,cars$dist,mean)
attach(orange)
tapply(Orange$age,Orange$circumference,mean)
# 6) rapply : Recursive apply, apply a function to all elements of a list recusively
x=list(1,2,3,4)
rapply(x,function(x){x^2},class=c("numeric"))
rapply(b,function(x){x^2})
# 7) vapply : similar to "sapply", has pre specified type of retuen type
vapply(1:5,sqrt,1i)
vapply(1:5,sqrt,0)
vapply(1:5,sqrt,33)

