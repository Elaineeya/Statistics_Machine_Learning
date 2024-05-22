# Load necessary libraries
library(MASS)  # for multivariate normal distribution
library(mvtnorm)  # for multivariate t-distribution

# Load the data
data <- read.table("WomenAtWork.dat", header = TRUE)

# Define the logistic function
logistic <- function(x) {
  return(exp(x) / (1 + exp(x)))
}

# Define the log posterior
log_posterior <- function(beta, data, tau) {
  y <- data$Work
  X <- as.matrix(data[,-1])
  eta <- X %*% beta
  p <- logistic(eta)
  log_likelihood <- sum(y * log(p) + (1 - y) * log(1 - p))
  #log_prior <- sum(dnorm(beta, mean = 0, sd = tau, log = TRUE))
  log_prior <- - sum(beta^2) / (2 * tau^2)
  return(log_likelihood + log_prior)
}

# Optimize the log posterior
start <- rep(0, ncol(data) - 1)  # starting values
tau <- 2  # prior standard deviation
result <- optim(start, log_posterior, data = data, tau = tau, control = list(fnscale = -1), hessian = TRUE)

# Extract the posterior mode and observed information matrix
beta_hat <- result$par
obs_hessian <- -solve(result$hessian)

# Compute the 95% equal tail posterior probability interval for NSmallChild
index <- which(colnames(data) == "NSmallChild")
index <- 6
ci <- qnorm(c(0.025, 0.975), beta_hat[index], sqrt(obs_hessian[index, index]))
quantile(mvrnorm(1000, mu = beta_hat, Sigma = obs_hessian)[, index], probs = c(0.025, 0.975))

quantile(rmvnorm(1000,beta_hat, obs_hessian)[, 6], probs = c(0.025, 0.975))


# Fit a logistic regression model
glmModel <- glm(Work ~ 0 + ., data = data, family = binomial)

# Print the coefficients
print(coef(glmModel))

# Compare with the posterior mode
print(beta_hat)



# Simulate from the posterior predictive distribution
simulate_posterior <- function(beta_hat, obs_hessian, x_new, n = 1000) {
  #beta_draws <- rmvnorm(n, mean = beta_hat, sigma = obs_hessian)
  beta_draws <- mvrnorm(n, beta_hat, obs_hessian)
  p_draws <- logistic(x_new %*% t(beta_draws))
  return(1-p_draws)
}

# Define the new woman
x_new <- c(1, 18, 11, 7, 40, 1, 1)

# Simulate from the posterior predictive distribution
p_draws <- simulate_posterior(beta_hat, obs_hessian, x_new)

# Plot the posterior predictive distribution
hist(p_draws, freq = FALSE, break = 50, xlab = "Pr(y = 0|x)", main = "Posterior predictive distribution of Pr(y = 0|x)")

# Simulate from the posterior predictive distribution for 13 women
#n_women <- 13
#y_draws <- rbinom(length(p_draws), size = n_women, prob = p_draws)

# Plot the posterior predictive distribution
#hist(y_draws, freq = FALSE, xlab = "Number of women not working", main = "Posterior predictive distribution")




# Function to simulate the number of women not working out of 13
simulate_not_working <- function(x_new, beta_hat, obs_hessian, n_women = 13, n = 500) {
  p_draws <- simulate_posterior(beta_hat, obs_hessian, x_new, n)
  not_working_draws <- rbinom(n, size = n_women, prob = p_draws)
  return(not_working_draws)
}

# Simulate the number of women not working out of 13
not_working_draws <- simulate_not_working(x_new, beta_hat, obs_hessian)

# Plot the posterior predictive distribution
hist(not_working_draws, breaks = seq(-0.5, 13.5, 1), freq = FALSE, main = "Posterior predictive distribution of the number of women not working", xlab = "Number of women not working")

hist(not_working_draws, breaks = 50, freq = FALSE, main = "Posterior predictive distribution of the number of women not working", xlab = "Number of women not working")







