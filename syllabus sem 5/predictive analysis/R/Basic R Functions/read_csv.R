#print your working directory
print(getwd())

data <- read.csv("Office city.csv")
print(data)
students = read.csv(file.choose(),header = TRUE)
a=read.csv(file.choose(),header = TRUE)

write.csv("office_city.csv")

print(is.data.frame(data))
print(ncol(data))
print(nrow(data))
sal <- max(data$Profit)
print(sal)
print(min(data$Profit))
print(sum(data$Profit))
print(mean(data$Profit))
print(median(data$Profit))
retval <- subset(data, Profit == max(Profit))

print(retval)
retval1 <- subset(data, Profit == min(Profit))
print(retval1$Client.ID)

