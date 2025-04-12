set.seed(123)
# Q2

suppressMessages(library(splines))
knots <- seq(0.05, 0.95, length.out = 25)
X_train <- ns(bike_data_train$hour, knots = knots, intercept = TRUE)
X_test <- ns(bike_data_test$hour, knots = knots, intercept = TRUE)
beta_hat <- solve(t(X_train)%*%X_train)%*%t(X_train)%*%y_train

# Plot training data, test data, and spline fit on a fine grid.
plot(log_cnt ~ hour, data = bike_data_train, col = "cornflowerblue", ylim = c(0, 8))
lines(bike_data_test$hour, bike_data_test$log_cnt, type = "p", col = "lightcoral")
hours_grid <- seq(0, 1, length.out = 1000)
X_grid <- ns(hours_grid, knots = knots, intercept = TRUE) # cbind(1, ns(hours_grid, knots = knots))
y_hat_spline_grid <- X_grid%*%beta_hat
lines(hours_grid, y_hat_spline_grid, lty = 1, col = "lightcoral")
legend(x = "topleft", pch = c(1, 1, NA), lty = c(NA, NA, 1), col = c("cornflowerblue", "lightcoral",  "lightcoral"), legend=c("Train", "Test", "Spline"))

# Q2.1 

I_mat <- diag(ncol(X_train))

objective_func <- function(lambda) sqrt(mean((y_test - X_test %*% (solve(t(X_train) %*% X_train + lambda * I_mat) %*% t(X_train) %*% y_train))^2))

optimal_lam <- optim(par = 0.5, fn = objective_func, method = "L-BFGS-B", lower = 0, upper = 1)

beta_hat_new = solve(t(X_train) %*% X_train + optimal_lam$par * I_mat) %*% t(X_train) %*% y_train

plot(log_cnt ~ hour, data = bike_data_train, col = "cornflowerblue", ylim = c(0, 8), main = "Spline Fit with Optimal Lambda")
lines(bike_data_test$hour, bike_data_test$log_cnt, type = "p", col = "lightcoral")
hours_grid <- seq(0, 1, length.out = 1000)
X_grid <- ns(hours_grid, knots = knots, intercept = TRUE) # cbind(1, ns(hours_grid, knots = knots))
y_hat_spline_grid <- X_grid%*%beta_hat_new
lines(hours_grid, y_hat_spline_grid, lty = 1, col = "lightcoral")
legend(x = "topleft", pch = c(1, 1, NA), lty = c(NA, NA, 1), col = c("cornflowerblue", "lightcoral",  "lightcoral"), legend=c("Train", "Test", "Spline"))

# Q2.2

ridge_model <- glmnet(X_train, y_train, alpha = 0)

cv <- cv.glmnet(X_train, y_train, alpha = 0)
plot(cv, main = "ridge penalty\n\n")

optimal_lambda_ridge <- cv$lambda.1se

ridge_train <- predict(ridge_model, s = optimal_lambda_ridge, newx = X_train)
ridge_test <- predict(ridge_model, s = optimal_lambda_ridge, newx = X_test)

ridge_train_plot <- predict(ridge_model, s = optimal_lambda_ridge, newx = X_grid)

ridge_RMSE_train <- sqrt(sum((y_train - ridge_train)^2)/length(y_train))
ridge_RMSE_predict <- sqrt(sum((y_test - ridge_test)^2)/length(y_test))

plot(log_cnt ~ hour, data = bike_data_train, col = "cornflowerblue", ylim = c(0, 8), main = "Spline Fit vs Ridge Fit with Optimal Lambdas")
lines(bike_data_test$hour, bike_data_test$log_cnt, type = "p", col = "lightcoral")
hours_grid <- seq(0, 1, length.out = 1000)
X_grid <- ns(hours_grid, knots = knots, intercept = TRUE) # cbind(1, ns(hours_grid, knots = knots))
y_hat_spline_grid <- X_grid%*%beta_hat_new
lines(hours_grid, y_hat_spline_grid, lty = 1, col = "lightcoral")
lines(hours_grid, ridge_train_plot, type = "l", col = "green")
legend(x = "topleft", pch = c(1, 1, NA, NA), lty = c(NA, NA, 1, 1), col = c("cornflowerblue", "lightcoral", "lightcoral", "green"), legend=c("Train", "Test","Spline Fit" ,"Ridge Fit"))


# Q2.3

min_optimal_lambda <- cv$lambda.min

ridge_train_min <- predict(ridge_model, s = min_optimal_lambda, newx = X_train)
ridge_test_min <- predict(ridge_model, s = min_optimal_lambda, newx = X_test)

ridge_RMSE_train_min <- sqrt(sum((y_train - ridge_train_min)^2)/length(y_train))
ridge_RMSE_predict_min <- sqrt(sum((y_test - ridge_test_min)^2)/length(y_test))

# Q2.4

lasso_model <- glmnet(X_train, y_train, alpha = 1)

cv_lasso <- cv.glmnet(X_train, y_train, alpha = 1)
plot(cv_lasso, main = "Lasso penalty\n\n")

optimal_lambda_lasso <- cv_lasso$lambda.1se

lasso_train <- predict(lasso_model, s = optimal_lambda_lasso, newx = X_train)
lasso_test <- predict(lasso_model, s = optimal_lambda_lasso, newx = X_test)

lasso_RMSE_train <- sqrt(sum((y_train - lasso_train)^2)/length(y_train))
lasso_RMSE_predict <- sqrt(sum((y_test - lasso_test)^2)/length(y_test))
