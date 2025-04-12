load(file = 'penguins.RData')
y <- penguins$dive_heart_rate
n <- length(y)
X <- penguins$duration/max(penguins$duration) # Scale duration [0, 1]
plot(X, y, main="DHR vs scaled duration", col = "cornflowerblue", xlab = "Scaled duration", ylab = "Dive heart rate (DHR)")
sigma_f <- 100
ell <- 0.6
sigma_eps <- sqrt(150)
X_star <- X # For smoothing
# Compute means and kernel matrices (smoothing case can reuse computations)
# Prior means
m_X <- rep(0, n)
m_Xstar <- m_X
# Prior covariances
K_X_X <- kernel_matrix_squared_exp(X, X, sigma_f, ell)
K_X_Xstar <- K_X_X
K_Xstar_X <- K_X_Xstar
K_Xstar_Xstar <- K_X_X
# Conditional distribution of f given y is normal. 
fbar_star <- m_Xstar + K_Xstar_X%*%solve(K_X_X + sigma_eps^2*diag(n), y - m_X)
lines(X, fbar_star, col = "lightcoral", type = "p")
legend(x = "topright", pch = c(1, 1), col = c("cornflowerblue", "lightcoral"), legend=c("Data", "Smoothed (fitted) values"))

# Q5.2 #

x_grid <- seq(0,1,length.out=1000)

K <- kernel_matrix_squared_exp(X, X, sigma_f, ell) + sigma_eps^2 * diag(n)
K_x <- kernel_matrix_squared_exp(X, x_grid, sigma_f, ell)
K_xx <- kernel_matrix_squared_exp(x_grid, x_grid, sigma_f, ell)

mu_x <- t(K_x) %*% solve(K) %*% y
cov_x <- K_xx - t(K_x) %*% solve(K) %*% K_x

std_x <- sqrt(diag(cov_x))

upper_bound <- mu_x + 1.96*std_x
lower_bound <- mu_x - 1.96*std_x

plot(X, y, main = "DHR with GP Predictions", col = "cornflowerblue", xlab = "Scaled duration", ylab = "Dive heart rate (DHR)")
lines(x_grid, mu_x, col = "lightcoral")
lines(x_grid, lower_bound, col = "lightcoral", lty = 4)
lines(x_grid, upper_bound, col = "lightcoral", lty = 4)
legend(x = "topright", pch = c(1, 1, -1, -1), col = c("cornflowerblue", "lightcoral", "lightcoral", "lightcoral"), legend=c("Data", "Posterior mean", "Lower 95%", "Upper 95%"), lty=c(NA, NA, 2, 2))


# Q5.3 #

fn <- function(l, X_train, y_train, noise) {
  n <- length(X_train)
  K <- kernel_matrix_squared_exp(X_train, X_train, 100, l) + noise^2 * diag(n)
  
  log_marginal_likelihood <- - 0.5 * t(y_train) %*% solve(K) %*% y_train - 0.5 * log(det(K)) - 0.5 * n * log(2*pi)
  
  return(-log_marginal_likelihood)
  
}

optimal_length <- optim(par = ell, fn=fn, X_train = X, y_train = y, noise = sigma_eps, lower = 0, upper = Inf, method = "L-BFGS-B")

# Q5.4 #

ell_grid<-seq(0,1,length.out=50)

folds <- cut(seq(1, n),breaks=5,labels=FALSE)

posterior <- function(X_s, X_train, y_train, l, variance, noise) {
  n <- length(X_train)
  K <- kernel_matrix_squared_exp(X_train, X_train, variance, l) + noise^2 * diag(n)
  K_x <- kernel_matrix_squared_exp(X_train, X_s, variance, l)
  K_xx <- kernel_matrix_squared_exp(X_s, X_s, variance, l)
  
  mu_x <- t(K_x) %*% solve(K) %*% y_train
  
  return(mu_x)
  
}

data <- cbind(X, y)

RMSE <- matrix(NA, 5, 50)

for(i in 1:5){
   
  testIndexes <- which(folds==i, arr.ind=TRUE)
  testData <- data[testIndexes, ]
  trainData <- data[-testIndexes, ]
  
  rmse <- c()
  
  for(l in 1:length(ell_grid)) {
    
    pred <- posterior(testData[, 1], trainData[, 1], trainData[, 2], ell_grid[l], sigma_f, sigma_eps)
    
    rmse[l] <- sqrt(mean(pred - testData[, 2])^2)
    
    
  }
  
  RMSE[i, ] <- rmse
  
}

average_RMSE <- apply(RMSE, 2, mean)

optimal_ell_ind <- which.min(average_RMSE) #index 8

optimal_cv_ell <- ell_grid[optimal_ell_ind] # ell value of 0.142857


# Q5.5 #

fn2 <- function(theta, X_train, y_train) {
  n <- length(X_train)
  l <- theta[1]
  variance <- theta[2]
  noise <- theta[3]
  
  K <- kernel_matrix_squared_exp(X_train, X_train, variance, l) + noise^2 * diag(n)
  
  log_marginal_likelihood <- - 0.5 * t(y_train) %*% solve(K) %*% y_train - 0.5 * log(det(K)) - 0.5 * n * log(2*pi)
  
  return(-log_marginal_likelihood)
  
}

optimal_params <- optim(par = c(ell, sigma_f, sigma_eps), fn=fn2, X_train = X, y_train = y, lower = c(0,0,0) , upper = c(Inf, Inf, Inf), method = "L-BFGS-B")
