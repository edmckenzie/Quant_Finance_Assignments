# Q5

#Q5.1

df <- as.data.frame(X_train)
df_test <- as.data.frame(X_test)

train_tree_df <- setNames(df, c("Intercept", paste0("Var", 1:34)))
test_tree_df <- setNames(df_test, c("Intercept", paste0("Var", 1:34)))

tree_model <- tree(y_train_lagged ~ ., train_tree_df)

tree_predict <- predict(tree_model, newdata = test_tree_df)

tree_RMSE <- sqrt(sum((y_test_lagged - tree_predict)^2)/length(y_test_lagged))

# Q5.2

plot(tree_model)
text(tree_model, pretty = 0)

# Q5.3

tree_values <- tail(tree_predict, length(response))

plot(response, corresponding_values,  xlab = "Actual Counts", ylab = "Fitted Values", 
     main = "Actual vs. Fitted Values", col = "cornflowerblue", pch = 16)
points(response, lagged_values, col = "lightcoral", pch = 16)
points(response, tree_values, col = "green", pch = 16)
legend(x = "bottomright", pch = c(16, 16, 16), col = c("cornflowerblue", "lightcoral", "green"), legend=c("Original Fit", "Lagged Fit", "Tree Fit"))
