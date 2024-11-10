# Load necessary libraries
library('class')    # For k-NN algorithm
library('caret')    # For confusion matrix
library('caTools')  # For data splitting

# Load the mtcars dataset
data(mtcars)
View(mtcars)

# Create a binary target variable based on median mpg
median_mpg = median(mtcars$mpg)
mtcars$mpg_class = factor(ifelse(mtcars$mpg > median_mpg, "High MPG", "Low MPG"))

# Drop the original mpg column
mtcars = mtcars[ , !(names(mtcars) %in% c("mpg"))]

# Normalize features
normalize = function(x) {
  (x - min(x)) / (max(x) - min(x))
}
mtcars[1:10] = as.data.frame(lapply(mtcars[1:10], normalize))

# Split the dataset into training and testing sets
set.seed(123)  # For reproducibility
split = sample.split(mtcars$mpg_class, SplitRatio = 0.70)
train_split = subset(mtcars, split == TRUE)
test_split = subset(mtcars, split == FALSE)

# Train the k-NN model and make predictions
k = 3  # You can adjust the value of k as needed
pred_test = knn(train_split[,-11], test_split[,-11], train_split$mpg_class, k = k)

# Create and view the confusion matrix
confusion = table(pred_test, test_split$mpg_class)
View(confusion)

# Calculate and print accuracy
accuracy = sum(diag(confusion)) / sum(confusion)
print(paste("Accuracy:", accuracy))

# Detailed confusion matrix
conf_matrix = confusionMatrix(pred_test, test_split$mpg_class)
print(conf_matrix)
