# Q3 #
rm(list=ls()) # Remove variables
cat("\014") # Clean workspace

# 3.1

load(file = 'insects.RData')
hist(insects$length, xlab = "Length (in mm)", ylab = "Counts", col = "cornflowerblue", main = "Histogram of the lengths of insects")

load(file = 'asb.RData')
plot(Close ~ log(Volume), data = asb, col = "cornflowerblue", main = "ASB stock: Closing price vs log of trading volume")

date <- as.Date(asb$Date)
Close <- asb$Close
plot(date, Close, col = "cornflowerblue",type = "l", main = "ASB stock: Closing prices during 2014-12-26 to 2017-11-10")

# A possible interpretation of the two clusters could be due to the change in the average stock price level depicted in the second graph. 
# We can see a clear break in the stock price trend at the end of 2016 from around $18 to a new level of around $24. 
# This is reflected in our cluster graph as we can see that majority of the trading occured at 1 price level around $18 and the other cluster at $24. 

mu1 <- -2
sigma1 <- 0.5
mu2 <- 4
sigma2 <- 1.5
pi1 <- 0.2
pi2 <- 1 - pi1
# Store in vectors:
mu <- c(mu1, mu2)
sigma <- c(sigma1, sigma2)
pis <- c(pi1, pi2)
n <- 1000
y <- rep(NA, n)
x <- rep(NA, n)

for(i in 1:n){
  # Simulate indicator with probability pi2 (1 - component 2, 0 - component 1)
  y[i] <- rbinom(n = 1, size=1, prob = pis[2]) + 1
  x[i] <- rnorm(n = 1, mean = mu[y[i]], sd = sigma[y[i]])
}

# Sanity check: Confirm the marginal class probabilities are pi1 and pi2 (approx)
print(paste("Estimated class 1 marginal probability: ", mean(y == 1), ". True: ", pis[1], sep = ""))

print(paste("Estimated class 2 marginal probability: ", mean(y == 2), ". True: ", pis[2], sep = ""))

# Normalised histogram
hist(x, xlab = "x", col = "cornflowerblue", main = "Normalised histogram of the simulated x", prob = TRUE)

# 3.3

x_grid<-seq(-6,10,length.out=1000)

x_mix <- rep(NA, length(x_grid))

for(i in 1:n){
  x_mix[i] <- rnorm(n = 1, mean = 2.8, sd = sqrt(7.61))
}


hist(x, col=rgb(0,0,1,1/4), prob = TRUE, main = "Normalised Histograms of simulated x and p(x) ")                  
hist(x_mix, col=rgb(1,0,0,1/4), breaks = 30, prob = TRUE, add = T)                   

  

