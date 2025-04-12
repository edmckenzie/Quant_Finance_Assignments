#Lab 2 Solution

#____________________Set Up______________________#

#Preparing workspace
rm(list=ls()) # Remove variables 
cat("\014") # Clean workspace
graphics.off()

suppressMessages(library(dplyr))
suppressMessages(library(splines))
suppressMessages(library(randomForest))
suppressMessages(library(xgboost))


bike_data = read.csv("bike_rental_hourly-1.csv")
head(bike_data)

bike_data$log_cnt <- log(bike_data$cnt)
bike_data$hour <- bike_data$hr/23 # transform [0, 23] to [0, 1]. 0 is midnight, 1 is 11 PM

# One hot for weathersit
one_hot_encode_weathersit <- model.matrix(~ as.factor(weathersit) - 1,data = bike_data)
one_hot_encode_weathersit  <- one_hot_encode_weathersit[, -1] # Remove reference category
colnames(one_hot_encode_weathersit) <- c('cloudy', 'light rain', 'heavy rain')
bike_data <- cbind(bike_data, one_hot_encode_weathersit)

# One hot for weekday
one_hot_encode_weekday <- model.matrix(~ as.factor(weekday) - 1,data = bike_data)
one_hot_encode_weekday  <- one_hot_encode_weekday[, -1] # Remove reference category
colnames(one_hot_encode_weekday) <- c('Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat')
bike_data <- cbind(bike_data, one_hot_encode_weekday)

# One hot for weekday
one_hot_encode_season <- model.matrix(~ as.factor(season) - 1,data = bike_data)
one_hot_encode_season  <- one_hot_encode_season[, -1] # Remove reference category
colnames(one_hot_encode_season) <- c('Spring', 'Summer', 'Fall')
bike_data <- cbind(bike_data, one_hot_encode_season)

# Create lags
bike_data_new <- mutate(bike_data, lag1 = lag(log_cnt, 1), lag2 = lag(log_cnt, 2), 
                        lag3 = lag(log_cnt, 3), lag4 = lag(log_cnt, 4), lag24 = lag(log_cnt, 24))

bike_data_new <- bike_data_new[-c(1:24),] # Lost 24 obs because of lagging

# Create training and test data
bike_all_data_train <- bike_data_new[bike_data_new$dteday >= as.Date("2011-01-01") & bike_data_new$dteday <=  as.Date("2012-05-31"), ]
bike_all_data_test <- bike_data_new[bike_data_new$dteday >= as.Date("2012-06-01") & bike_data_new$dteday <=  as.Date("2012-12-31"), ]
X_train <- cbind(1, bike_all_data_train[, c("lag1", "lag2",  "lag3", "lag4", "lag24")])
spline_basis <- ns(bike_all_data_train$hour, df = 10, intercept = FALSE)
X_train <- cbind(X_train, spline_basis)
colnames(X_train)[1] <- "intercept"
knots <- attr(spline_basis, "knots")
variables_to_keep_in_X <- c("yr", "holiday", "workingday", "temp", "atemp", "hum", "windspeed") 
variables_to_keep_in_X <- c(variables_to_keep_in_X, colnames(one_hot_encode_weathersit), colnames(one_hot_encode_weekday), colnames(one_hot_encode_season))
X_train <- cbind(X_train, bike_all_data_train[, variables_to_keep_in_X])
head(X_train)

# Training data
X_train <- as.matrix(X_train)
y_train <- bike_all_data_train$log_cnt
# Test data
y_test <- bike_all_data_test$log_cnt
X_test <- cbind(1, bike_all_data_test[, c("lag1", "lag2",  "lag3", "lag4", "lag24")])
spline_basis_test <- ns(bike_all_data_test$hour, df=10, knots=knots, intercept = FALSE)
X_test <- cbind(X_test, spline_basis_test)
colnames(X_test)[1] <- "intercept"
X_test <- cbind(X_test, bike_all_data_test[, variables_to_keep_in_X])
X_test <- as.matrix(X_test)



#____________________Problem 1______________________#

#____________________Problem 1.1____________________#

#RMSE Function 
RMSE = function(y_pred, y_actual){
    sqrt(mean((y_pred - y_actual)^2))
}
#TODO:
  #probably make this a FUNCTION

rf_50 = randomForest(X_train, y_train, ntree=50)
rf_100 = randomForest(X_train, y_train, ntree=100)
rf_150 = randomForest(X_train, y_train, ntree=150)

yhat_train_50 = predict(rf_50, newdata=X_train)
yhat_train_100 = predict(rf_100, newdata=X_train)
yhat_train_150 = predict(rf_150, newdata=X_train)

yhat_test_50 = predict(rf_50, newdata=X_test)
yhat_test_100 = predict(rf_100, newdata=X_test)
yhat_test_150 = predict(rf_150, newdata=X_test)

rmse_y_hat_train_50 = RMSE(yhat_train_50, y_train)
rmse_y_hat_train_100 = RMSE(yhat_train_100, y_train)
rmse_y_hat_train_150 = RMSE(yhat_train_150, y_train)

rmse_y_hat_test_50 = RMSE(yhat_test_50, y_test)
rmse_y_hat_test_100 = RMSE(yhat_test_100, y_test)
rmse_y_hat_test_150 = RMSE(yhat_test_150, y_test)

#RMSE For Training
cat("\nRMSE for training 50 trees is ", rmse_y_hat_train_50)
cat("\nRMSE for training 100 trees is ", rmse_y_hat_train_100)
cat("\nRMSE for training 150 trees is ", rmse_y_hat_train_150)

#RMSE for Test
cat("\nRMSE for test 50 trees is ", rmse_y_hat_test_50)
cat("\nRMSE for test 100 trees is ", rmse_y_hat_test_100)
cat("\nRMSE for test 150 trees is ", rmse_y_hat_test_150)


#____________________Problem 1.2____________________#
#extract the last week of test data
last_week_test = bike_all_data_test[bike_all_data_test$dteday >= as.Date("2012-12-25") & bike_all_data_test$dteday <= as.Date("2012-12-31"), ]

#converting it to a data frame
yhat_test_50 = as.data.frame(yhat_test_50)
yhat_test_100 = as.data.frame(yhat_test_100)

#use row names from last_week_test to filter y_hat_test
fitted_values_last_week_50 = yhat_test_50[row.names(last_week_test),]
fitted_values_last_week_100 = yhat_test_100[row.names(last_week_test),]

n = 1:(24*7)[-c(168)] #creating numbers ranging from 1 to 167
n = n[-c(168)]

#Plot
plot(n, last_week_test$cnt,  xlab = "1:(24*7)",ylab = "Fitted Values",
     main = "Prob 4.1: Actual vs. Fitted Values",
     col = "cornflowerblue",
     pch = 16)
points(n, exp(fitted_values_last_week_50), col = "lightcoral", pch = 16)
points(n, exp(fitted_values_last_week_100), col = "lightcoral", pch = 16)
legend(x = "topleft", pch = c(16, 16), col = c("cornflowerblue", "lightcoral"), legend=c("Actual Count", "Fitted Values"))


#____________________Problem 1.3____________________#

#TODO:
#probably make this a FUNCTION


fit_boost_10 = xgboost(data = as.matrix(X_train), label = y_train, nrounds = 10)  
fit_boost_25 = xgboost(data = as.matrix(X_train), label = y_train, nrounds = 25)  
fit_boost_50 = xgboost(data = as.matrix(X_train), label = y_train, nrounds = 50)  

yhat_train_10 = predict(fit_boost_10, newdata = as.matrix(X_train))
yhat_train_25 = predict(fit_boost_25, newdata = as.matrix(X_train))
yhat_train_50 = predict(fit_boost_50, newdata = as.matrix(X_train))

yhat_test_10 = predict(fit_boost_10, newdata = as.matrix(X_test))
yhat_test_25 = predict(fit_boost_10, newdata = as.matrix(X_test))
yhat_test_50 = predict(fit_boost_10, newdata = as.matrix(X_test))

rmse_y_hat_train_10 = RMSE(yhat_train_10, y_train)
rmse_y_hat_train_25 = RMSE(yhat_train_25, y_train)
rmse_y_hat_train_50 = RMSE(yhat_train_50, y_train)

rmse_y_hat_test_10 = RMSE(yhat_test_10, y_test)
rmse_y_hat_test_25 = RMSE(yhat_test_25, y_test)
rmse_y_hat_test_50 = RMSE(yhat_test_50, y_test)

#RMSE For Training
cat("\nRMSE for training 10 boost is ", rmse_y_hat_train_10)
cat("\nRMSE for training 25 boost is ", rmse_y_hat_train_25)
cat("\nRMSE for training 50 boost is ", rmse_y_hat_train_50)

#RMSE for Test
cat("\nRMSE for test 10 boost is ", rmse_y_hat_test_10)
cat("\nRMSE for test 25 boost is ", rmse_y_hat_test_25)
cat("\nRMSE for test 50 boost is ", rmse_y_hat_test_50)


#____________________Problem 1.4____________________#
#NEED TO DETERMINE BEST BAGGING AND BOOSTING MODEL VIA LOWEST RMSE

#converting it to a data frame
yhat_test_50 = as.data.frame(yhat_test_50)
yhat_test_100 = as.data.frame(yhat_test_100)

#use row names from last_week_test to filter y_hat_test
fitted_values_last_week_50 = yhat_test_50[row.names(last_week_test),]
fitted_values_last_week_100 = yhat_test_100[row.names(last_week_test),]

n = 1:(24*7)[-c(168)] #creating numbers ranging from 1 to 167
n = n[-c(168)]

#Plot
plot(n, last_week_test$cnt,  xlab = "1:(24*7)",ylab = "Fitted Values",
     main = "Prob 4.1: Actual vs. Fitted Values",
     col = "cornflowerblue",
     pch = 16)
points(n, exp(fitted_values_last_week_50), col = "lightcoral", pch = 16)
points(n, exp(fitted_values_last_week_100), col = "lightcoral", pch = 16)
legend(x = "topleft", pch = c(16, 16), col = c("cornflowerblue", "lightcoral"), legend=c("Actual Count", "Fitted Values"))

#____________________Problem 2______________________#

#____________________Set Up_________________________#

rm(list=ls()) # Remove variables 
cat("\014") # Clean workspace
load(file = "spam_ham_emails-1.RData")
library(randomForest)
library(pROC)
library(caret)
library(xgboost)

Spam_ham_emails[, -1] <- scale(Spam_ham_emails[, -1])
Spam_ham_emails['spam'] <- as.factor(Spam_ham_emails['spam'] == 1) # Changing from 1->TRUE, 0->FALSE
levels(Spam_ham_emails$spam) <- c("not spam", "spam")
head(Spam_ham_emails)
set.seed(1234)
suppressMessages(library(caret))

train_obs <- createDataPartition(y = Spam_ham_emails$spam, p = .75, list = FALSE)
train <- Spam_ham_emails[train_obs, ]
y_train <- train$spam
X_train <- train[, -1]
test <- Spam_ham_emails[-train_obs, ]
y_test <- test$spam
X_test <- test[, -1]

# Confirm both training and test are balanced with respect to spam emails
print(paste("Percentage of training data consisting of spam emails:", 
            100*mean(train$spam == "spam")))

print(paste("Percentage of test data consisting of spam emails:", 
            100*mean(test$spam == "spam")))

#____________________Problem 2.1____________________#


fit_rf_50 = randomForest(y_train ~., data=X_train, ntree=50)
fit_rf_100 = randomForest(y_train ~., data=X_train, ntree=100)
fit_rf_500 = randomForest(y_train ~., data=X_train, ntree=500)

yhat_test_50 = predict(fit_rf_50, newdata=X_test, type="prob")[,2]
yhat_test_100 = predict(fit_rf_100, newdata=X_test, type="prob")[,2]
yhat_test_500 = predict(fit_rf_500, newdata=X_test, type="prob")[,2]

roc50 = roc(test$spam, yhat_test_50)
roc100 = roc(test$spam, yhat_test_100)
roc500 = roc(test$spam, yhat_test_500)

plot(roc50, print.auc = TRUE, auc.polygon = FALSE, auc.polygon.col = "cornflowerblue", print.thres = "best")
lines(roc100, col="blue", lwd=2)
lines(roc500, col="green", lwd=2)

auc50 = auc(roc50)
auc100 = auc(roc100)
auc500 = auc(roc500)

cat("\nAUC for Random Forest with 50 trees ", auc50)
cat("\nAUC for Random Forest with 100 trees ", auc100)
cat("\nAUC for Random Forest with 500 trees ", auc500)

#____________________Problem 2.2____________________#

threshold = 0.5 # Predict spam if probability > threshold
yhat_test_100_prob = as.factor(yhat_test_100 > threshold)
levels(yhat_test_100_prob)  = c("not spam", "spam")
confusionMatrix(data = yhat_test_100_prob, test$spam, positive = "spam")

#____________________Problem 2.3____________________#
X_train_xgb <- as.matrix(X_train)
X_test_xgb <- as.matrix(X_test)
y_train_xgb <- as.integer(y_train) - 1
y_test_xgb <- as.integer(y_test) - 1

fit_boost_10 = xgboost(data = X_train_xgb, label = y_train_xgb, nrounds = 10, objective = "binary:logistic")  
fit_boost_25 = xgboost(data = X_train_xgb, label = y_train_xgb, nrounds = 25, objective = "binary:logistic")  
fit_boost_50 = xgboost(data = X_train_xgb, label = y_train_xgb, nrounds = 50, objective = "binary:logistic")  

yhat_test_10 = predict(fit_boost_10, newdata = X_test_xgb)
yhat_test_25 = predict(fit_boost_25, newdata = X_test_xgb)
yhat_test_50 = predict(fit_boost_50, newdata = X_test_xgb)

roc_boost_10 = roc(y_test_xgb, yhat_test_10)
roc_boost_25 = roc(y_test_xgb, yhat_test_25)
roc_boost_50 = roc(y_test_xgb, yhat_test_50)

plot(roc_boost_10, print.auc = TRUE, auc.polygon = FALSE, auc.polygon.col = "cornflowerblue", print.thres = "best")
lines(roc_boost_25, col="blue", lwd=2)
lines(roc_boost_50, col="blue", lwd=2)
lines(roc100, col="blue", lwd=2)

auc_boost_10 = auc(roc_boost_10)
auc_boost_25 = auc(roc_boost_25)
auc_boost_50 = auc(roc_boost_50)

cat("\nAUC for xgboost with 10 rounds ", auc_boost_10)
cat("\nAUC for xgboost with 25 rounds ", auc_boost_25)
cat("\nAUC for xgboost with 50 rounds ", auc_boost_50)

#____________________Problem 2.4____________________#

threshold = 0.5 # Predict spam if probability > threshold
yhat_test_100_prob = as.factor(yhat_test_25 > threshold)
levels(yhat_test_100_prob)  = c("not spam", "spam")
confusionMatrix(data = yhat_test_100_prob, test$spam, positive = "spam")

#____________________Problem 3______________________#

#____________________Problem 3.1____________________#

rm(list=ls()) # Remove variables 
cat("\014") # Clean workspace
gen_x <- function(seed=123){
  set.seed(seed)
  return(runif(0, 1, n = 1000))
}
# Test
x <- gen_x()
length(x)
head(x)

beta_0 = 0.3
beta_1 = 2.5

lambda = exp(beta_0 + beta_1 * x)

y = rpois(1000, lambda)



#____________________Problem 3.2____________________#

x_grid = seq(0, 1, length.out=10000)

conditional_expected = exp(beta_0 + beta_1 * x_grid)

plot(x, y,  xlab = "x",ylab = "y",
     main = "Prob 3.2: Poisson Regression Model",
     col = "cornflowerblue",
     pch = 16)
points(x_grid, conditional_expected, col = "lightcoral", pch = 16)
legend(x = "topleft", pch = c(16, 16), col = c("cornflowerblue", "lightcoral"), legend=c("Observations", "Conditional Expected"))


#____________________Problem 3.5____________________#

loss_func = function (y, x, beta_0, beta_1) {
  sum(y * (beta_0 + beta_1 * x) - exp(beta_0 + beta_1 * x) - log(factorial(y)))
} 

grads = function (y, x, beta_0, beta_1) {
  dB0 <- sum(y - exp(beta_0 + beta_1 * x))
  dB1 <- sum(x * (y - exp(beta_0 + beta_1 * x)))
  output <- c(dB0, dB1)
  return(output)
}

#____________________Problem 3.6____________________#

cost_func = function(n, log_loss) {
  -(1/n) * log_loss
}

gradient_func = function(n, gradients) {
  -(1/n) * gradients
}

gradient_descent = function (y, x, learning_rate, iterations) {
  
  beta_0 <- 1.0
  beta_1 <- 1.0
  betas <- c(beta_0, beta_1)
  n <- length(y)
  
  beta_track <- matrix(0, nrow = n, ncol = length(betas))
  cost_track <- c()
  
  for(i in 1:iterations){
    
    loss <- loss_func(y, x, betas[1], betas[2])
    cost <- cost_func(n, loss)
    cost_track[i] <- cost
    
    gradients <- grads(y, x, betas[1], betas[2])
    update <- gradient_func(n, gradients)
    
    betas <- betas - learning_rate * update
    beta_track[i,] = betas
    
  }
  
  return(
    list(
      Params = betas,
      Beta_hist = beta_track,
      Loss_hist = cost_track))
    
  
}

params_0.01 <- gradient_descent(y, x, 0.01, 200)
params_0.1 <- gradient_descent(y, x, 0.1, 200)
params_0.25 <- gradient_descent(y, x, 0.25, 200)

#____________________Problem 3.7____________________#


stochastic_descent = function (y, x, epochs, batch_size) {
  
  beta_0 <- 1.0
  beta_1 <- 1.0
  beta <- c(beta_0, beta_1)
  n <- length(y)
  nb <- batch_size
  num_of_batches <- n/nb
  
  for(i in 1:epochs){
    
    x <- sample(x)
    y <- sample(y)
    
    for(j in 1:num_of_batches){
      
      loss <- loss_func(y[((j-1)*nb+1) : (j*nb)], x[((j-1)*nb+1) : (j*nb)], beta[1], beta[2])
      cost <- cost_func(nb, loss)
      
      gradients <- grads(y[((j-1)*nb+1) : (j*nb)], x[((j-1)*nb+1) : (j*nb)], beta[1], beta[2])
      update <- gradient_func(nb, gradients)
      
      beta <- beta - (0.5 / (j^0.6)) * update
      
    }
    
  }
  
  return(beta)
  
}

sgd_10 <- stochastic_descent(y, x, 20, 10)
sgd_50 <- stochastic_descent(y, x, 20, 50)
sgd_100 <- stochastic_descent(y, x, 20, 100)

#____________________Problem 4______________________#

#____________________Problem 4.1____________________#



#____________________Problem 4.2____________________#
#I am confused as to where y_i comes into this? There is no y_i in second 
#order derivative of the log likelihood of poisson regression

# Note: Fill in the NAs!
Hess_log_dens_single_obs <- function(beta, y_i, X_i) {
  phi11 <- -exp(beta[1] + beta[2] * X_i) 
  phi12 <- -exp(beta[1] + beta[2] * X_i) * X_i 
  phi21 <- phi12
  phi22 <-  -exp(beta[1] + beta[2] * X_i) * X_i^2
  return(matrix(c(phi11, phi21, phi12, phi22), nrow = 2, ncol = 2))
}

Hess_log_like <- function(beta, y, X) {
  n <- length(y)
  sum_Hess_log_like <- matrix(rep(0, 4), nrow = 2, ncol = 2)
  for(i in 1:n) {
    sum_Hess_log_like <- sum_Hess_log_like + Hess_log_dens_single_obs(beta, y[i], X[i ])
  }
  return(sum_Hess_log_like)
}


#____________________Problem 4.3____________________#

newton_descent = function (y, x, iterations, trust_region) {
  
  beta_0 <- 0.0
  beta_1 <- 0.0
  beta <- c(beta_0, beta_1)
  n <- length(y)
  
  
  cost_track <- c()
  
  for(i in 1:iterations){
    
    loss <- loss_func(y, x, beta[1], beta[2])
    cost <- cost_func(n, loss)
    cost_track[i] <- cost
    
    gradients <- grads(y, x, beta[1], beta[2])
    hessian <- -(1/n) * Hess_log_like(beta, y, x)
    v <- solve(hessian) %*% gradient_func(n, gradients)
    learning_rate <- (trust_region / max(norm(v, type="2"), trust_region))
    
    beta <- beta - learning_rate * v
    
    
  }
  
  return(
    list(
      Params = beta,
      Loss_hist = cost_track))
  
  
}

test_newton <- newton_descent(y, x, 200, 1)

#____________________Problem 5______________________#

#____________________Problem 5.1____________________#

#GETTING FINITIE PROBLEMS 
#DO WE UE X AND Y FROM PROBLEM 3.1?

log_dens_Poisson <- function(beta, y, X) {
  # log-density for each y_i, X_i
    return(dpois(y, lambda = exp(X%*%beta),log = TRUE))
  
}

log_like_Poisson <- function(beta, y, X) {
  # log-likelihood (for all observations)
  return(sum(log_dens_Poisson(beta, y, X)))
}

cost_function = function(beta, y, X){
   
    return(-(1/n) * log_like_Poisson(beta, y, X))
  
}
  
#____________________Problem 5.2____________________#
#TODO:
  #GETTING ERROR MESSAGE OF FINITIE ERROR VALUES
  #what about the intercept? # i am guessing the true parameter values needs to be close to 0.3 and 2.5 as in problem 3.1

beta_start = c(0,2)
n <- 1000
#beta_start = c(0.3, 2.5)

X = as.matrix(cbind(1, x), ncol = 2)


Y = as.matrix(y)

obj = optim(par = beta_start, fn=cost_function, y=y, X=X, method = "L-BFGS-B")

obj$par #these are the true parameter values 

#____________________Problem 5.3____________________#

load(file = "eBay_coins.RData")

ebay_response = as.matrix(eBay_coins$nBids) #this is y
ebay_features = as.matrix(select(eBay_coins, -nBids))#this is X
beta_start = rep(1, ncol(ebay_features))

betas = optim(par = beta_start, fn=cost_function, y=ebay_response,X=ebay_features)$par

#PRINT OUT NAME AND OPTIMISED VALUE
betas

#____________________Problem 5.4____________________#
#also need 1 for the intercept
#do we ned to take the exponential of it so its poisson regression?
#am i doing this right? - my point estimate seems wrong

new_data = c(1, 1, 1, 1, 0, 0 ,0 ,1, 0.5)

point_estimate = exp(betas %*% new_data)

cat("\nThe point estimate is ", point_estimate)



