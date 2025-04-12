
# Q3

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

head(bike_data)

# Q3.1

design_data <- select(bike_data, -weathersit, -weekday, -season)
  
head(design_data)

design_data_train = design_data[design_data$dteday >= as.Date("2011-01-01") & design_data$dteday <=  as.Date("2012-05-31"), ]
design_data_test = design_data[design_data$dteday >= as.Date("2012-06-01") & design_data$dteday <=  as.Date("2012-12-31"), ]

spline_basis <- ns(design_data_train$hour, df = 10, intercept = FALSE)
knots <- attr(spline_basis, "knots")
spline_basis_test <- ns(design_data_test$hour, df = 10, knots = knots, intercept = FALSE)

design_data_train$hour <- spline_basis
design_data_test$hour <- spline_basis_test

features <- c("hour", "yr", "holiday", "workingday", "temp", "atemp", "hum", "windspeed","cloudy","light rain","heavy rain","Mon","Tue","Wed","Thu","Fri","Sat","Spring","Summer","Fall")

design_matrix_train <- design_data_train[features]
design_matrix_test <- design_data_test[features]

X_train <- model.matrix(~ ., data = design_matrix_train)
y_train <- design_data_train$log_cnt

lasso_model <- glmnet(X_train, y_train, alpha = 1)

cv_lasso <- cv.glmnet(X_train, y_train, alpha = 1)

optimal_lambda_lasso <- cv_lasso$lambda.1se

X_test <- model.matrix(~ ., data = design_matrix_test)
y_test <- design_data_test$log_cnt

lasso_train <- predict(lasso_model, s = optimal_lambda_lasso, newx = X_train)
lasso_test <- predict(lasso_model, s = optimal_lambda_lasso, newx = X_test)

lasso_RMSE_train <- sqrt(sum((y_train - lasso_train)^2)/length(y_train))
lasso_RMSE_predict <- sqrt(sum((y_test - lasso_test)^2)/length(y_test))

# Q3.2

plot(lasso_model, xvar = "lambda", main = "Lasso penalty\n\n", label = TRUE, ylim= c(-1,1.5))
#abline(v = log(cv_lasso$lambda.min), col = "red", lty = "dashed")
#abline(v = log(cv_lasso$lambda.1se), col = "blue", lty = "dashed")

coef(lasso_model, s = 0.5)

# feature 2 = hour 1, 10 = hour 9, 16 = atemp 

# Q3.3

residuals_train <- y_train - lasso_train
acf_residuals_train <- acf(residuals_train)
qqnorm(residuals_train)
qqline(residuals_train)

residuals_test <- y_test - lasso_test
acf_residuals_test <- acf(residuals_test)
qqnorm(residuals_test)
qqline(residuals_test)