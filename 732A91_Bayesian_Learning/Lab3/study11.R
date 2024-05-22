# Load necessary libraries
library(polyagamma)  # for Polya-Gamma distribution
library(coda)  # for convergence diagnostics
library(MASS)
library(mvtnorm)
set.seed(12345)

# Load the data
data <- read.table("WomenAtWork.dat", header = TRUE)

# Response and Features
y <- data$Work
X <- as.matrix(data[,-1])

# Define the logistic function
logistic <- function(x) {
  return(1 / (1 + exp(-x)))
}

# Initialize parameters
n <- nrow(data)
p <- ncol(data) - 1  # subtract one for the response variable
beta <- matrix(0, nrow = 1000, ncol = p)
tau <- 3
omega <- rep(1, n)

# Gibbs sampler
for (iter in 2:1000) {
  # Update beta
  Sigma <- solve(t(X) %*% diag(omega) %*% X + diag(1/tau^2, p))
  mu <- Sigma %*% t(X) %*% (y - 1/2)
  beta[iter,] <- mvrnorm(1, mu, Sigma)
  
  # Update omega
  for (i in 1:n) {
    omega[i] <- rgamma(1, shape = 1, rate = logistic(X[i,] %*% beta[iter,]))
  }
}

# Evaluate convergence
beta_mcmc <- as.mcmc(beta)
plot(beta_mcmc)
effectiveSize(beta_mcmc)

# Compute 90% credible interval for Pr(y = 1|x)
posterior_prob <- apply(beta, 1, function(b) logistic(sum(b * c(1, 22, 12, 7, 38, 1, 0))))
quantile(posterior_prob, probs = c(0.05, 0.95))
