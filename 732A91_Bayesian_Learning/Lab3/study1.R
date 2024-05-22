# Load necessary libraries
library(MASS)
library(mvtnorm)
set.seed(12345)

# Load the data
data <- read.table("WomenAtWork.dat", header = TRUE)

# Response and Features
y <- data$Work
X <- as.matrix(data[,-1])

# Prior
tau <- 3
p <- ncol(X)
prior_variance <- tau^2 * diag(p)
prior_precision <- solve(prior_variance)


# Gibbs Sampler
# Gibbs Sampler
gibbs <- function(m1, m2, r, n_iter=1000){
  q1 <- q2 <- numeric(n_iter)
  for(i in 2:n_iter){
    # Sample q1 given q2
    q1[i] <- rnorm(1, m1 + r * (q2[i-1] - m2), sqrt(1 - r^2))
    
    # Sample q2 given q1
    q2[i] <- rnorm(1, m2 + r * (q1[i] - m1), sqrt(1 - r^2))
  }
  
  return(data.frame(q1, q2))
}

# Gibbs Sampler
gibbs <- function(X, y, prior_precision, n_iter=1000){
  n <- dim(X)[1]
  p <- dim(X)[2]
  beta <- matrix(0, n_iter, p)
  omega <- rep(1, n)
  
  for(i in 2:n_iter){
    # Sample beta
    post_precision <- t(X) %*% diag(omega) %*% X + prior_precision
    post_variance <- solve(post_precision)
    post_mean <- post_variance %*% t(X) %*% (y - 1/2)
    beta[i,] <- mvrnorm(1, post_mean, post_variance)
    
    # Sample omega
    z <- X %*% beta[i,]
    omega <- rpg(n, z)
  }
  
  return(beta)
}



# Run Gibbs Sampler
beta_draws <- gibbs(X, y, prior_precision)

# Convergence Diagnostics
library(coda)
beta_mcmc <- mcmc(beta_draws)
plot(beta_mcmc)
effectiveSize(beta_mcmc)

# New data
newdata <- c(1, 18, 11, 7, 40, 1, 1)

# Compute probabilities
prob <- plogis(beta_draws %*% newdata)

# Compute 90% credible interval
quantile(prob, c(0.05, 0.95))
