# Q8

# Q8.1

train$spam <- ifelse(train$spam == "spam", 1, 0)
test$spam <- ifelse(test$spam == "spam", 1, 0)

normalized_train <- as.data.frame(apply(train[2:16], 2, normalize_vec))
normalized_test <- as.data.frame(apply(test[2:16], 2, normalize_vec))

y_train <- train$spam
y_test <- test$spam

knn_manual <- function(X_train, X_test, y_train, k) {
  
  distances <- cdist(X_train, X_test, metric = "euclidean")
  y_pred <- c()
  
  for (i in 1:ncol(distances)) {
    pred <- distances[,i]
    
    ordered_pred <- order(pred)
    
    k_nearest_indices <- ordered_pred[1:k]
    
    k_nearest_labels <- y_train[k_nearest_indices]
    
    prediction <- as.numeric(names(which.max(table(k_nearest_labels))))
    
    y_pred[i] <- prediction
    
  }
  
  return(y_pred)
  
}

error_rate <- c()

for (k in seq(1, 25, by = 2)) {
  
  y_pred <- knn_manual(normalized_train, normalized_test, y_train, k)
  error_rate[k] <- mean(y_pred != y_test)
  
}

plot(1:25, error_rate, type = "b", col = "cornflowerblue", xlab = "Value of K", ylab = "Missclassification Rate", main = "Performance as a Function of K")

optimal_k <- which.min(error_rate)


# 8
y_pred_knn = knn_manual(normalized_train, normalized_test, y_train, optimal_k)  

# Calculate misclassification rates
missclassification_rate_knn = mean(y_pred_knn != y_test)
missclassification_rate_logistic = 1 - classification_accuracy
missclassification_rate_tree = 1 - tree_result$overall["Accuracy"]

# Print the results
cat("Misclassification Rate - KNN:", missclassification_rate_knn, "\n")
cat("Misclassification Rate - Logistic:", missclassification_rate_logistic, "\n")
cat("Misclassification Rate - Decision Tree:", missclassification_rate_tree, "\n")