rm(list=ls()) # Remove variables 
cat("\014") # Clean workspace

library(tidyverse)
library(dplyr)
library(splines)
library(glmnet)
library(timetk)
library(caret)
library(tree)
library(rdist)

# Q1 
# Load Data

bike_data <- read.csv('bike_rental_hourly.csv')
head(bike_data)

bike_data$log_cnt <- log(bike_data$cnt)
bike_data$hour <- bike_data$hr/23
plot(log_cnt ~ hour, data = bike_data, col="cornflowerblue")

bike_data_train <- bike_data[bike_data$dteday >= as.Date("2011-02-01") & bike_data$dteday <= as.Date("2011-03-31"), ]
bike_data_test <- bike_data[bike_data$dteday >= as.Date("2011-04-01") & bike_data$dteday <=  as.Date("2011-05-31"), ]

y_test <- bike_data_test$log_cnt
y_train <- bike_data_train$log_cnt
p <- 2
X_train <- cbind(1, poly(bike_data_train$hour, 2, raw = TRUE, simple = TRUE))
beta_hat <- solve(t(X_train)%*%X_train)%*%t(X_train)%*%y_train

# Predict in-sample and compute RMSE
y_hat_train <- X_train%*%beta_hat 
RMSE_train <- sqrt(sum((y_train - y_hat_train)^2)/length(y_train))

# Predict out-of-sample and compute RMSE
X_test <- cbind(1, poly(bike_data_test$hour, p, raw = TRUE, simple = TRUE))
y_hat_test <- X_test%*%beta_hat
RMSE_test <- sqrt(sum((y_test - y_hat_test)^2)/length(y_test))

# Plot training data, test data, and fit on a fine grid.
plot(log_cnt ~ hour, data = bike_data_train, col = "cornflowerblue", ylim = c(0, 8))
lines(bike_data_test$hour, bike_data_test$log_cnt, type = "p", col = "lightcoral")
hours_grid <- seq(0, 1, length.out = 1000)
X_grid <- cbind(1, poly(hours_grid, p, raw = TRUE, simple = TRUE))
y_hat_grid <- X_grid%*%beta_hat
lines(hours_grid, y_hat_grid, lty = 1, col = "lightcoral")
legend(x = "topleft", pch = c(1, 1, NA), lty = c(NA, NA, 1), col = c("cornflowerblue", "lightcoral",  "lightcoral"), legend=c("Train", "Test", "Fitted curve"))

# Q1.1
y_test <- bike_data_test$log_cnt
y_train <- bike_data_train$log_cnt
p <- 8
X_train <- cbind(1, poly(bike_data_train$hour, p, raw = TRUE, simple = TRUE))
beta_hat <- solve(t(X_train)%*%X_train)%*%t(X_train)%*%y_train

# Predict in-sample and compute RMSE
y_hat_train <- X_train%*%beta_hat 
RMSE_train <- sqrt(sum((y_train - y_hat_train)^2)/length(y_train))

# Predict out-of-sample and compute RMSE
X_test <- cbind(1, poly(bike_data_test$hour, p, raw = TRUE, simple = TRUE))
y_hat_test <- X_test%*%beta_hat
RMSE_test <- sqrt(sum((y_test - y_hat_test)^2)/length(y_test))

# Plot training data, test data, and fit on a fine grid.
plot(log_cnt ~ hour, data = bike_data_train, col = "cornflowerblue", ylim = c(0, 8))
lines(bike_data_test$hour, bike_data_test$log_cnt, type = "p", col = "lightcoral")
hours_grid <- seq(0, 1, length.out = 1000)
X_grid <- cbind(1, poly(hours_grid, p, raw = TRUE, simple = TRUE))
y_hat_grid <- X_grid%*%beta_hat
lines(hours_grid, y_hat_grid, lty = 1, col = "lightcoral")
legend(x = "topleft", pch = c(1, 1, NA), lty = c(NA, NA, 1), col = c("cornflowerblue", "lightcoral",  "lightcoral"), legend=c("Train", "Test", "Fitted curve"))

# Q1.2

RMSE_train <- numeric(length = 10)
RMSE_test <- numeric(length = 10)

for (i in 1:10) {
  p <- i
  X_train <- cbind(1, poly(bike_data_train$hour, p, raw = TRUE, simple = TRUE))
  beta_hat <- solve(t(X_train)%*%X_train)%*%t(X_train)%*%y_train
  
  # Predict in-sample and compute RMSE
  y_hat_train <- X_train%*%beta_hat 
  RMSE_train[i] <- sqrt(sum((y_train - y_hat_train)^2)/length(y_train))
  
  # Predict out-of-sample and compute RMSE
  X_test <- cbind(1, poly(bike_data_test$hour, p, raw = TRUE, simple = TRUE))
  y_hat_test <- X_test%*%beta_hat
  RMSE_test[i] <- sqrt(sum((y_test - y_hat_test)^2)/length(y_test))
  
}

plot(1:10, RMSE_train, type = "b", col = "cornflowerblue", xlab = "Polynomial Order", ylab = "RMSE",
     main = "RMSE vs Polynomial Order", ylim=c(0,2))
lines(1:10, RMSE_test, type = "b", col = "lightcoral")
legend("topright", legend = c("Train RMSE", "Test RMSE"), col = c("cornflowerblue", "lightcoral"), lty = 1)

#Q 1.3

y_test <- bike_data_test$log_cnt
y_train <- bike_data_train$log_cnt
p <- 8
X_train <- cbind(1, poly(bike_data_train$hour, p, raw = TRUE, simple = TRUE))
beta_hat <- solve(t(X_train)%*%X_train)%*%t(X_train)%*%y_train

# Predict in-sample and compute RMSE
y_hat_train <- X_train%*%beta_hat 
RMSE_train <- sqrt(sum((y_train - y_hat_train)^2)/length(y_train))

# Predict out-of-sample and compute RMSE
X_test <- cbind(1, poly(bike_data_test$hour, p, raw = TRUE, simple = TRUE))
y_hat_test <- X_test%*%beta_hat
RMSE_test <- sqrt(sum((y_test - y_hat_test)^2)/length(y_test))

#Nonparametric local regression

#loess_train <- loess(log_cnt ~ hour, data = bike_data_train)
loess_model <- loess(log_cnt ~ hour, data = bike_data_train)
loess_test <- predict(loess_model, bike_data_test)
loess_RMSE <- sqrt(sum((y_test - loess_test)^2)/length(y_test))

local_fit <- predict(loess_model, data.frame(hour = seq(0, 1, length.out=1000), se = TRUE))

# Plot training data, test data, and fit on a fine grid.
plot(log_cnt ~ hour, data = bike_data_train, col = "cornflowerblue", ylim = c(0, 8))
lines(bike_data_test$hour, bike_data_test$log_cnt, type = "p", col = "lightcoral")
hours_grid <- seq(0, 1, length.out = 1000)
X_grid <- cbind(1, poly(hours_grid, p, raw = TRUE, simple = TRUE))
y_hat_grid <- X_grid%*%beta_hat
lines(hours_grid, y_hat_grid, lty = 1, col = "lightcoral")
lines(hours_grid, local_fit, lty = 1, col = "green")
legend(x = "topleft", pch = c(1, 1, NA, NA), lty = c(NA, NA, 1, 1), col = c("cornflowerblue", "lightcoral",  "lightcoral", "green"), legend=c("Train", "Test", "Poly Fitted curve", "Smoothed Fit"))




