# Q4

# Q4.1

response <- design_data_test[design_data_test$dteday >= as.Date("2012-12-25") & design_data_test$dteday <= as.Date("2012-12-31"),]
response <- response$hr

corresponding_values <- tail(lasso_test, length(response))

plot(response, corresponding_values,  xlab = "Actual Counts",
     ylab = "Fitted Values",
     main = "Actual vs. Fitted Values",
     col = "cornflowerblue",
     pch = 16)

# Q4.2


lagged_training <- design_data_train %>%
  tk_augment_lags(contains("log_cnt"), .lags = 1:4)

lag_24 <- design_data_train %>%
  tk_augment_lags(contains("log_cnt"), .lags = 24)

lagged_training <- cbind(lagged_training, lag_24["log_cnt_lag24"])

lagged_test <- design_data_test %>%
  tk_augment_lags(contains("log_cnt"), .lags = 1:4)

lag_24_test <- design_data_test %>%
  tk_augment_lags(contains("log_cnt"), .lags = 24)

lagged_test <- cbind(lagged_test, lag_24_test["log_cnt_lag24"])

features <- c("hour", "yr", "holiday", "workingday", "temp", "atemp", "hum", "windspeed","cloudy","light rain","heavy rain","Mon","Tue","Wed","Thu","Fri","Sat","Spring","Summer","Fall", "log_cnt_lag1", "log_cnt_lag2", "log_cnt_lag3", "log_cnt_lag4", "log_cnt_lag24")

lagged_train_matrix <- lagged_training[features]
lagged_test_matrix <- lagged_test[features]

lagged_train_matrix <- na.omit(lagged_train_matrix)
lagged_test_matrix <- na.omit(lagged_test_matrix)

X_train <- model.matrix(~ ., data = lagged_train_matrix)
y_train <- design_data_train$log_cnt
y_train_lagged <- y_train[25:length(y_train)]

lagged_lasso_model <- glmnet(X_train, y_train_lagged, alpha = 1)

cv_lagged_lasso <- cv.glmnet(X_train, y_train_lagged, alpha = 1)

optimal_lagged_lasso <- cv_lagged_lasso$lambda.1se

X_test <- model.matrix(~ ., data = lagged_test_matrix)
y_test <- design_data_test$log_cnt
y_test_lagged <- y_test[25:length(y_test)]

lagged_lasso_train <- predict(lagged_lasso_model, s = optimal_lagged_lasso, newx = X_train)
lagged_lasso_test <- predict(lagged_lasso_model, s = optimal_lagged_lasso, newx = X_test)

lagged_lasso_RMSE_train <- sqrt(sum((y_train_lagged - lagged_lasso_train)^2)/length(y_train_lagged))
lagged_lasso_RMSE_predict <- sqrt(sum((y_test_lagged - lagged_lasso_test)^2)/length(y_test_lagged))

# Q4.3

lagged_values <- tail(lagged_lasso_test, length(response))

plot(response, corresponding_values,  xlab = "Actual Counts", ylab = "Fitted Values", 
     main = "Actual vs. Fitted Values", col = "cornflowerblue", pch = 16)
points(response, lagged_values, col = "lightcoral", pch = 16)
legend(x = "bottomright", pch = c(16, 16), col = c("cornflowerblue", "lightcoral"), legend=c("Original Fit", "Lagged Fit"))


