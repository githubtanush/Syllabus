# Load necessary libraries
library('class')    # For k-NN algorithm
library('caret')    # For confusion matrix
library('caTools')  # For data splitting

# Load the Iris dataset
data(iris)
View(iris)

# Convert Species to a factor (it already is, but let's ensure)
iris$Species = factor(iris$Species)

# Normalize features (optional but recommended for consistency)
normalize = function(x) {
  (x - min(x)) / (max(x) - min(x))
}
iris[1:4] = as.data.frame(lapply(iris[1:4], normalize))

# Split the dataset into training and testing sets
set.seed(123)  # For reproducibility
split = sample.split(iris$Species, SplitRatio = 0.70)
train_split = subset(iris, split == TRUE)
test_split = subset(iris, split == FALSE)

# Train the k-NN model and make predictions
k = 3  # You can adjust the value of k as needed
pred_test = knn(train_split[,-5], test_split[,-5], train_split$Species, k = k)

# Create and view the confusion matrix
confusion = table(pred_test, test_split$Species)
View(confusion)

# Calculate and print accuracy
accuracy = sum(diag(confusion)) / sum(confusion)
print(paste("Accuracy:", accuracy))

# Detailed confusion matrix
conf_matrix = confusionMatrix(pred_test, test_split$Species)
print(conf_matrix)