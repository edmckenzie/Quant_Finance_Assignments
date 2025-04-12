# Q5 #
rm(list=ls()) # Remove variables
cat("\014") # Clean workspace

# 5.1 #

load(file = 'palmer_penguins_missing.RData')
# Get indices to the classes
ind_gentoo <- (palmer_penguins_missing$species == "Gentoo")
ind_adelie <- (palmer_penguins_missing$species == "Adelie")
ind_missing <- is.na(palmer_penguins_missing$species)

x1 <- palmer_penguins_missing$flipper_length_mm
x2 <- palmer_penguins_missing$body_mass_g
plot(x1[ind_gentoo], x2[ind_gentoo], type = "p", col = "cornflowerblue", xlim = c(170, 250), ylim = c(2500, 7000), xlab = "Flipper length", ylab = "Body mass") 
lines(x1[ind_adelie], x2[ind_adelie], type = "p", col = "lightcoral") 
lines(x1[ind_missing], x2[ind_missing], type = "p", pch = "?", cex = 0.8, col = "black")
legend("topright", legend = c("Gentoo", "Adelie", "Missing"), col = c("cornflowerblue", "lightcoral", "black"), pch = c("o", "o", "?"))

df_no_missing<-na.omit(palmer_penguins_missing)

library(dplyr)

x_labeled <- df_no_missing$flipper_length_mm
class_labels <- df_no_missing$species

mu_supervised <- c(mean(x_labeled[class_labels == "Gentoo"]), mean(x_labeled[class_labels == "Adelie"]))
sigma_supervised <- c(var(x_labeled[class_labels == "Gentoo"]), var(x_labeled[class_labels == "Adelie"]))
pi_supervised <- c(sum(class_labels == "Gentoo") / length(class_labels), sum(class_labels == "Adelie") / length(class_labels))

# QDA discriminant function
qda_score <- function(x, mu, sigma2, marginal) {
  -0.5 * log(sigma2) - 0.5 * (x - mu)^2 / sigma2 + log(marginal)
}

# Prediction
predictions <- sapply(x_labeled, function(point) {
  score1 <- qda_score(point, mu_supervised[1], sigma_supervised[1], pi_supervised[1])
  score2 <- qda_score(point, mu_supervised[2], sigma_supervised[2], pi_supervised[2])
  ifelse(score1 > score2, "Gentoo", "Adelie")
})



# 5.2 #

df_missing <- palmer_penguins_missing

mu <- c(mean(na.omit(df_missing$flipper_length_mm[df_missing$species == "Gentoo"])),
        mean(na.omit(df_missing$flipper_length_mm[df_missing$species == "Adelie"])))

sigma <- c(sd(na.omit(df_missing$flipper_length_mm[df_missing$species == "Gentoo"])),
           sd(na.omit(df_missing$flipper_length_mm[df_missing$species == "Adelie"])))

pis <- c(0.5, 0.5) # could still use QDA but the EM algorithm will optimise these very quickly anyways

x <- df_missing$flipper_length_mm
class_labels <- df_missing$species


EM_GMM_Penguins <- function(x, class_labels, mu_start, sigma_start, pis_start, n_iter = 100) {
  stopifnot(sum(pis_start) == 1) # this can be removed with QDA
  
  pis_all <- matrix(NA, nrow = n_iter, ncol = 2)
  mu_all <- matrix(NA, nrow = n_iter, ncol = 2)
  sigma_all <- matrix(NA, nrow = n_iter, ncol = 2)
  Q_all <- rep(NA, n_iter)
  log_like_all <- rep(NA, n_iter)

  # Initialise
  mu <- mu_start
  sigma <- sigma_start
  pis <- pis_start
  n <- length(x)
  W <- matrix(0, nrow = n, ncol = 2)
  log_pdf_class <- matrix(0, nrow = n, ncol = 2)
  
  for(j in 1:n_iter){
    # E-step
    for(m in 1:2){
      log_pdf_class[, m] <- dnorm(x, mean = mu[m], sd = sqrt(sigma2[m]), log = TRUE) + log(pis[m])
      if (m == 1) {
        W[class_labels == "Gentoo", m] <- 1
      }
      else if (m == 2) {
        W[class_labels == "Adelie", m] <- 1
      }
      W[is.na(class_labels), m] <- pis[m] * dnorm(x[is.na(class_labels)], mean = mu[m], sd = sigma[m])
    }
    w <- W / rowSums(W)
    n_hat <- colSums(w)
    Q <- sum(rowSums(w*log_pdf_class)) # Expected log-likelihood
    
    # M-step
    for(m in 1:2){
      pis[m] <- n_hat[m] / n
      mu[m] <- sum(w[, m] * x) / n_hat[m]
      sigma[m] <- sum(w[, m] * ( - mu[m])^2) / n_hat[m]
    }
    pis_all[j, ] <- pis
    mu_all[j, ] <- mu
    sigma_all[j, ] <- sigma
    Q_all[j] <- Q
    
    # Compute log-likelihood at current parameter values
    for (m in 1:2) {
      if (m == 1) {
        W[class_labels == "Gentoo", m] <- 1
      }
      else if (m == 2) {
        W[class_labels == "Adelie", m] <- 1
      }
      W[is.na(class_labels), m] <- pis[m] * dnorm(x[is.na(class_labels)], mean = mu[m], sd = sigma[m])
    }
    log_like_all[j] <- sum(log(rowSums(W)))
  }
  # Return final estimates
  return(list(pi_hat = pis, mu_hat = mu, sigma2_hat = sigma,
              weights = W/rowSums(W), pis_all = pis_all, mu_all = mu_all, sigma_all = sigma_all,
              Q_all = Q_all, log_like_all = log_like_all))
}

# Example of how to call the function:
# result <- EM_GMM_Penguins_with_QDA(df)

result <- EM_GMM_Penguins_with_QDA(df_missing, n_iter = 1000)

x <- df_missing$flipper_length_mm
mu_semi <- result$mu_hat
sigma_semi <- result$sigma2_hat
pi_semi <- result$pi_hat


# Final predictions on all data
predictions_semi <- sapply(x, function(point) {
  score1 <- qda_score(point, mu_semi[1], sigma_semi[1], pi_semi[1])
  score2 <- qda_score(point, mu_semi[2], sigma_semi[2], pi_semi[2])
  ifelse(score1 > score2, "Gentoo", "Adelie")
})

df_missing$species <- predictions_semi
x1_semi <- df_missing$flipper_length_mm
x2_semi <- df_missing$body_mass_g
ind_semi_gentoo <- (df_missing$species == "Gentoo")
ind_semi_adelie <- (df_missing$species == "Adelie")

x1 <- palmer_penguins_missing$flipper_length_mm
x2 <- palmer_penguins_missing$body_mass_g
plot(x1_semi[ind_semi_gentoo], x2[ind_semi_gentoo], type = "p", col = "cornflowerblue", xlim = c(170, 250), ylim = c(2500, 7000), xlab = "Flipper length", ylab = "Body mass") 
lines(x1_semi[ind_semi_adelie], x2[ind_semi_adelie], type = "p", col = "lightcoral") 
lines(x1[ind_missing], x2[ind_missing], type = "p", pch = "?", cex = 0.8, col = "black")
legend("topright", legend = c("Gentoo Pred", "Adelie Pred", "Missing"), col = c("cornflowerblue", "lightcoral", "black"), pch = c("o", "o", "?"))


# 5.3 #
load(file = 'palmer_penguins.RData')

x_test <- palmer_penguins$flipper_length_mm[ind_missing]
y_test <- palmer_penguins$species[ind_missing]

supervised_preds <- sapply(x_test, function(point) {
  score1 <- qda_score(point, mu_supervised[1], sigma_supervised[1], pi_supervised[1])
  score2 <- qda_score(point, mu_supervised[2], sigma_supervised[2], pi_supervised[2])
  ifelse(score1 > score2, "Gentoo", "Adelie")
})

# Final predictions on all data
semi_preds <- sapply(x_test, function(point) {
  score1 <- qda_score(point, mu_semi[1], sigma_semi[1], pi_semi[1])
  score2 <- qda_score(point, mu_semi[2], sigma_semi[2], pi_semi[2])
  ifelse(score1 > score2, "Gentoo", "Adelie")
})

supervised_accuracy <- 1 - mean(y_test != supervised_preds)

semi_accuracy <- 1 - mean(y_test != semi_preds)

