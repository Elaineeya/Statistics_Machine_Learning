# Load necessary libraries
library(readxl)
library(mvtnorm)

# Import the dataset
data <- read_xlsx("Linkoping2022.xlsx")

# Create time variable
data$time <- seq_along(data$temp) / 365

# Define prior hyperparameters
mu0 <- c(0, 100, -100)
Omega0 <- 0.01 * diag(3)
nu0 <- 1
sigma02 <- 1



# Simulate draws from the joint prior
simulate_prior <- function(mu0, Omega0, nu0, signma02, n) {
  beta <- rmvnorm(n, mu0, Omega0)
  sigma2 <- 1 / rchisq(n, nu0, ncp = nu0 * sigma02)
  cbind(beta, sigma2)
}

x <- as.numeric((as.Date(raw_data$datetime)-as.Date(raw_data$datetime[1]))/365)


# Compute regression curves for each draw from the prior
compute_regression_curve <- function(draw) {
  beta0 <- draw[1]
  beta1 <- draw[2]
  beta2 <- draw[3]
  beta0 + beta1 * data$time + beta2 * data$time^2
}


prior_draws_init <- simulate_prior(mu0, Omega0, nu0, signma02, 1000)

# Define test prior hyperparameters
mu0_test <- c(-10, 105, -100)
Omega0_test <- 0.01 * diag(3)
nu0_test <- 20
sigma02_test <- 0.1

prior_draws_test <- simulate_prior(mu0_test, Omega0_test, nu0_test, signma02_test, 1000)


#Plot the prior regression curve
par(mfrow = c(1, 2))
plot(data$time, data$temp, type = "l", main = "Prior Regression Curves", sub ="with Init Parameters", xlab = "Time", ylab = "Temperature")
for (i in 1:nrow(prior_draws_init)) {
  lines(data$time, compute_regression_curve(prior_draws_init[i,]), col = rgb(0, 0, 1, alpha = 0.1))
}

plot(data$time, data$temp, type = "l", main = "Prior Regression Curves", sub = "with Test Parameters", xlab = "Time", ylab = "Temperature")
for (i in 1:nrow(prior_draws_test)) {
  lines(data$time, compute_regression_curve(prior_draws_test[i,]), col = rgb(0, 0, 1, alpha = 0.1))
}

# (b) Simulate draws from the joint posterior distribution of β0, β1,β2 and σ2.
simulate_posterior <- function(n, data, mu0, Omega0, nu0, sigma02) {
  # Placeholder for the posterior draws
  posterior_draws <- matrix(0, nrow = n, ncol = 4)
  
  X <- cbind(1, data$time, data$time^2)
  
  for (i in 1:n) {
    # Update the hyperparameters based on the data and the prior
    Omega_n <- solve(solve(Omega0) + t(X) %*% X)
    mu_n <- Omega_n %*% (solve(Omega0) %*% mu0 + t(X) %*% data$temp)
    #mu_n <- Omega_n %*% (t(X) %*% data$temp + Omega0_inv %*% mu0)
    
    nu_n <- nu0 + length(data$temp)
    sigma_n2 <- (nu0 * sigma02 + t(data$temp - X %*% mu0) %*% (data$temp - X %*% mu0) + t(mu0) %*% solve(Omega0) %*% mu0 - t(mu_n) %*% solve(Omega_n) %*% mu_n) / nu_n
    
    # Draw from the posterior distribution
    beta <- rmvnorm(1, mu_n, Omega_n)
    sigma2 <- 1 / rchisq(1, nu_n, ncp = nu_n * sigma_n2)
    
    # Store the draw
    posterior_draws[i,] <- c(beta, sigma2)
  }
  
  return(posterior_draws)
}

#mu0 <- c(-10, 105, -100)
#Omega0 <- 0.01 * diag(3)
#nu0 <- 4
#sigma02 <- 1
posterior_draws <- simulate_posterior(1000, data, mu0, Omega0, nu0, sigma02)


# 2) Plot a histogram for each marginal posterior of the parameters
par(mfrow = c(2, 2))  # Arrange the plots in a 2x2 grid
hist(posterior_draws[,1], main = "Posterior of β0", xlab = "β0")
hist(posterior_draws[,2], main = "Posterior of β1", xlab = "β1")
hist(posterior_draws[,3], main = "Posterior of β2", xlab = "β2")
hist(posterior_draws[,4], main = "Posterior of σ2", xlab = "σ2")
par(mfrow = c(1, 1))  # Reset the plot layout

# 3) Make a scatter plot of the temperature data and overlay a curve for the posterior median of the regression function
plot(data$time, data$temp, main = "Posterior Median Regression Curve and 90% Credible Interval", xlab = "Time", ylab = "Temperature")
lines(data$time, t(apply(posterior_draws[,1:3], 2, median)) %*% t(cbind(1, data$time, data$time^2)), col = "red")
lines(data$time, t(apply(posterior_draws[,1:3], 2, quantile, probs = 0.05)) %*% t(cbind(1, data$time, data$time^2)), col = "blue")
lines(data$time, t(apply(posterior_draws[,1:3], 2, quantile, probs = 0.95)) %*% t(cbind(1, data$time, data$time^2)), col = "green")


# (c) Simulate from the posterior distribution of the time with the highest expected temperature (i.e. the time where f(time) is maximal). Let's call this value x~.
simulate_x_tilde <- function(posterior_draws) {
  # Placeholder for the simulations
  x_tilde_simulations <- numeric(nrow(posterior_draws))
  
  for (i in 1:nrow(posterior_draws)) {
    # Extract the parameters
    beta1 <- posterior_draws[i, 2]
    beta2 <- posterior_draws[i, 3]
    
    # Compute the time that maximizes the expected temperature
    x_tilde <- -beta1 / (2 * beta2)
    
    # Store the simulation
    x_tilde_simulations[i] <- x_tilde
  }
  
  return(x_tilde_simulations)
}

x_tilde_simulations <- simulate_x_tilde(posterior_draws)
hist(x_tilde_simulations, main = "Posterior of x~", xlab = "x~")




# Define the order of the polynomial
p <- 10

# Define mu0
mu0 <- c(0, rep(100, p - 1), rep(0, p - 1))

# Define Omega0
Omega0 <- diag(c(0.01, rep(1, p - 1), rep(0.01, p - 1)))










sigma_n2 <- (nu0 * sigma02 + t(data$temp - X %*% mu_n) %*% (data$temp - X %*% mu_n) + t(mu0 - mu_n) %*% solve(Omega0 + Omega_n) %*% (mu0 - mu_n)) / nu_n

