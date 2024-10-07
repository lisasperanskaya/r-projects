# Load the dataset
library(data.table)
data <- fread("/cardio_data.csv")
head(data)

# Split the dataset into 70% training data and 30% testing data
sample <- sample(c(TRUE, FALSE), nrow(data), replace=TRUE, prob=c(0.7,0.3))
train  <- data[sample, ]
test   <- data[!sample, ]

# Exclude "cardio" and "id" columns and scale the remaining feature columns
train_scaled <- scale(subset(train, select = -c(cardio,id)))
test_scaled <- scale(subset(test, select = -c(cardio,id)))

# Apply KNN with k = 10
library(class)
test_pred <- knn( train = train_scaled, test = test_scaled,cl = train$cardio, k=10 )

# Extract actual target values (cardio) from the test set
actual <- test$cardio

# Create the confusion matrix to compare actual vs predicted values
cmatrix <- table(actual,test_pred)
cmatrix

# Initialize variables to store the best k and the corresponding sum of correctly classified instances
best_k <- 50
highest_sum <- 0

# Loop through different values of k from 50 to 100
for (k in 50:100) {
  # Run the KNN algorithm
  test_pred <- knn(train = train_scaled, test = test_scaled, cl = train$cardio, k = k)
  actual <- test$cardio
  cmatrix <- table(actual, test_pred)
  
  # Calculate the sum of (0,0) and (1,1)
  correct_classification_sum <- cmatrix[1, 1] + cmatrix[2, 2]
  
  # Check if this sum is the highest we've seen so far
  if (correct_classification_sum > highest_sum) {
    highest_sum <- correct_classification_sum
    best_k <- k
  }
}

# Print the best k value and the highest sum of correctly classified instances
cat("Best k value:", best_k, "\n")
cat("Highest sum of correctly classified instances (0,0 and 1,1):", highest_sum, "\n")

#Best k value is equal to 55
test_pred <- knn( train = train_scaled, test = test_scaled,cl = train$cardio, k=10 )
actual <- test$cardio
cmatrix <- table(actual,test_pred)
cmatrix

#Calculate accuracy
accuracy <- sum(diag(cm))/length(actual)
sprintf("Accuracy: %.2f%%", accuracy*100)