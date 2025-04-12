# Q4.1 #

pairwise_cov_squared_exp <- function(x, x_prime, sigma_f, ell) {
  return(sigma_f^2*exp(-1/(2*ell^2)*(x - x_prime)^2))
}

covariance1 <- pairwise_cov_squared_exp(0.3, 0.7, 1.5, 0.5)
covariance2 <- pairwise_cov_squared_exp(0.1, 0.5, 1.5, 0.5)
correlation3 <- pairwise_cov_squared_exp(-0.2, -0.5, 1.5, 0.5) / sqrt(pairwise_cov_squared_exp(-0.2, -0.2, 1.5, 0.5) * 
                                                                        pairwise_cov_squared_exp(-0.5, -0.5, 1.5, 0.5))

# Q4.2 #

X <- seq(-1, 1, length.out = 21)

covariance_matrix <- function(X) {

Kernel_Mat <- matrix(0, length(X), length(X))

for (i in 1:length(X)) {
  for (j in 1:length(X)) {
    Kernel_Mat[i, j] = pairwise_cov_squared_exp(X[i], X[j], 1.5, 0.5)
  }
}
return(Kernel_Mat)
}

result <- covariance_matrix(X)

print(result[2,5])

# Q4.3 #

kernel_matrix_squared_exp <- function(X, Xstar, sigma_f, ell) {
  # Computes the kernel matrix for the squared exponential kernel model
  # Compute the pairwise squared Euclidean distances
  pairwise_squared_distances <- outer(X, Xstar, FUN = "-")^2
  # Compute the kernel matrix element-wise
  kernel_matrix <- sigma_f^2*exp(-1/(2*ell^2)*pairwise_squared_distances)
  return(kernel_matrix)
}

start_time1 <- Sys.time()
covariance_matrix(X)
end_time1 <- Sys.time()

print(end_time1 - start_time1)

start_time2 <- Sys.time()
kernel_matrix_squared_exp(X, X, 1.5, 0.5)
end_time2 <- Sys.time()

print(end_time2 - start_time2)

# Q4.4 #

suppressMessages(library(mvtnorm)) # for multivariate normal
n_grid <- 200
X_grid <- seq(-1, 1, length.out = n_grid)
sigma_f <- 1.5
ell <- 1.5
m_X <- rep(0, n_grid) # Create zero vector
K_X_X <- kernel_matrix_squared_exp(X_grid, X_grid, sigma_f, ell)
GP_realisations <- rmvnorm(n = 5, mean = m_X, sigma = K_X_X)
matplot(X_grid, t(GP_realisations), type = "l", lty = 1, col = c("cornflowerblue", "lightcoral", "green", "black", "purple"), xlab = "x", ylab = "f(x)", main = "Simulations from the GP prior", xlim=c(-1, 1.5), ylim=c(-3*sigma_f, 3*sigma_f))
legend("topright", legend = c("Sim 1", "Sim 2", "Sim 3", "Sim 4", "Sim 5"), col = c("cornflowerblue", "lightcoral", "green", "black", "purple"), lty = 1)


matplot(X_grid, t(GP_realisations), type = "l", lty = 1, col = c("cornflowerblue", "lightcoral", "green", "black", "purple"), xlab = "x", ylab = "f(x)", main = "Simulations from the GP prior", xlim=c(-1, 1.5), ylim=c(-3*sigma_f, 3*sigma_f))
legend("topright", legend = c("Sim 1", "Sim 2", "Sim 3", "Sim 4", "Sim 5"), col = c("cornflowerblue", "lightcoral", "green", "black", "purple"), lty = 1)

# Define x values for shading (X_grid for this example)
x_shade <- X_grid
# Lower and upper interval (prior mean is zero)
lower_interval <- -1.96*sigma_f*rep(1, n_grid)
upper_interval <- 1.96*sigma_f*rep(1, n_grid)

# Create a polygon to shade the prediction interval (alpha controls transparency)
polygon(c(x_shade, rev(x_shade)), c(lower_interval, rev(upper_interval)), col = rgb(0, 0, 1, alpha = 0.05), border = NA)

# The length-scale hyperparameter controls the correlation length between datapoints. As we decrease (increase) the value of l, two neighbouring points have 
# to be explained by a sharper (smoother) variation in the value of the underlying function due to this correlation distance being smaller (larger). This has the effect of
# creating a wigglier (smoother) function by increasing (decreasing) the variance and lowering (increasing) the bias of the model. 

