install.packages("readxl")

# Load required libraries
library(readxl)
library(mvtnorm)
library(ggplot2)

# Load the data
data <- read_xlsx("Linkoping2022.xlsx")

# Create time variable
data$time <- (1:nrow(data)) / 365

# Define the model matrix
X <- cbind(1, data$time, data$time^2)
# Define the model
#model <- function(beta, time) {
#  beta[1] + beta[2]*time + beta[3]*time^2
#}


# Define prior parameters
mu0 <- c(0, 100, -100)
Omega0 <- 0.01 * diag(3)
nu0 <- 1
sigma02 <- 1


# Simulate draws from the joint prior
#simulate_prior <- function(n) {
#  beta <- rmvnorm(n, mu0, Omega0)
#  sigma2 <- 1 / rchisq(n, df = nu0, ncp = sigma02)
#  list(beta = beta, sigma = sigma2)
#}

 
# Check if the prior agrees with your prior opinions
#n_draws <- 1000
#prior <- simulate_prior(n_draws)
#prior_curves <- apply(prior$beta, 1, model, time = data$time)


# Simulate draws from the joint prior
n_draws <- 1000
beta_draws <- rmvnorm(n_draws, mu0, Omega0)
#sigma2_draws <- 1/rgamma(n_draws, nu0/2, nu0*sigma02/2)
sigma2_draws <- 1 / rchisq(n_draws, nu0, ncp = nu0 * sigma02)

# Compute the regression curves for each draw
reg_curves <- sapply(1:n_draws, function(i) {
  beta <- beta_draws[i,]
  sigma2 <- sigma2_draws[i]
  X %*% beta
})

# Plot the prior regression curves
plot(data$time, data$temp, type = "l", xlab = "Time", ylab = "Temperature", main = "Prior Regression Curves")
for(i in 1:n_draws) {
  lines(data$time, reg_curves[,i], col = rgb(0, 0, 1, alpha = 0.1))
}



# Define the posterior hyperparameters
Omega0_inv <- solve(Omega0)
Omega_n <- solve(t(X) %*% X + Omega0_inv)
mu_n <- Omega_n %*% (t(X) %*% data$temp + Omega0_inv %*% mu0)

nu_n <- nu0 + nrow(data)
#sigma02_n <- (t(data$temp) %*% data$temp + t(mu0) %*% Omega0_inv %*% mu0 - t(mu_n) %*% solve(Omega_n) %*% mu_n) / nu_n
sigma02_n <- (nu0 * sigma02 + t(data$temp) %*% data$temp + t(mu0) %*% Omega0_inv %*% mu0 - t(mu_n) %*% solve(Omega_n) %*% mu_n) / nu_n

#sigma_n2 <- (nu0 * sigma02 + t(y) %*% y + t(mu0) %*% solve(Omega0) %*% mu0 - t(mu_n) %*% solve(Omega_n) %*% mu_n) / nu_n

# Simulate draws from the joint posterior
beta_draws <- rmvnorm(n_draws, mu_n, Omega_n)
sigma2_draws <- 1/rchisq(n_draws, nu_n, nu_n*sigma02_n)
#sigma2_draws <- 1/rgamma(n_draws, nu_n/2, nu_n*sigma02_n/2)


# Simulate from the posterior
sim_posterior <- function(n) {
  sigma2 <- 1 / rchisq(n, nu_n, ncp = nu_n * sigma02_n)
  beta <- rmvnorm(n, mu_n, Omega_n)
  list(beta = beta, sigma2 = sigma2)
}

posterior <- sim_posterior(1000)

# Plot histograms of the marginal posteriors
par(mfrow = c(2, 2))
hist(posterior$beta[,1], main = "Posterior of beta0", xlab = "beta0")
hist(posterior$beta[,2], main = "Posterior of beta1", xlab = "beta1")
hist(posterior$beta[,3], main = "Posterior of beta2", xlab = "beta2")
hist(posterior$sigma2, main = "Posterior of sigma2", xlab = "sigma2")



# Compute and plot the posterior median and 90% interval of the regression function
posterior_function <- function(time) {
  predict(X, newdata = data.frame(time = time, beta = posterior$beta))
}
ggplot(data, aes(x = time, y = temp)) +
  geom_point() +
  stat_summary(fun.data = function(y) mean(y), geom = "line", color = "blue") +
  stat_summary(fun.data = function(y) quantile(y, probs = c(0.05, 0.95)), geom = "ribbon", alpha = 0.5)

# Compute and plot the posterior median and 90% interval of the regression function
time_values <- seq(min(data$time), max(data$time), length.out = length(data$time))
posterior_predictions <- sapply(time_values, function(time) {
  y_values <- posterior$beta[,1] + posterior$beta[,2]*time + posterior$beta[,3]*time^2
  c(median = median(y_values), lower = quantile(y_values, probs = 0.05), upper = quantile(y_values, probs = 0.95))
})

ggplot(data, aes(x = time, y = temp)) +
  geom_point() +
  geom_line(aes(x = time_values, y = posterior_predictions["median",]), color = "blue") +
  geom_ribbon(aes(x = time_values, ymin = posterior_predictions["lower.5%",], ymax = posterior_predictions["upper.95%",]), alpha = 0.5, color = "red")


# (b.i) Plot a histogram for each marginal posterior of the parameters
par(mfrow = c(2, 2))
hist(beta_draws[,1], main = "Posterior of Beta0", xlab = "Beta0")
hist(beta_draws[,2], main = "Posterior of Beta1", xlab = "Beta1")
hist(beta_draws[,3], main = "Posterior of Beta2", xlab = "Beta2")
hist(sigma2_draws, main = "Posterior of Sigma2", xlab = "Sigma2")

# (b.ii) Make a scatter plot of the temperature data and overlay a curve for the posterior median of the regression function
times <- seq(min(data$time), max(data$time), length.out = 100)
posterior_reg_curves <- sapply(1:n_draws, function(i) {
  beta <- beta_draws[i,]
  sigma2 <- sigma2_draws[i]
  beta[1] + beta[2]*times + beta[3]*times^2
})
reg_median <- apply(posterior_reg_curves, 1, median)
reg_lower <- apply(posterior_reg_curves, 1, function(x) quantile(x, 0.05))
reg_upper <- apply(posterior_reg_curves, 1, function(x) quantile(x, 0.95))

par(mfrow = c(1, 1))
plot(data$time, data$temp, type = "l", xlab = "Time", ylab = "Temperature")
lines(times, reg_median, col = "red")
lines(times, reg_lower, col = "blue", lty = 5)
lines(times, reg_upper, col = "green", lty = 2)

# (c) Plot the posterior distribution of x~
hist(x_tilde_draws, main = "Posterior of x~", xlab = "x~")










simulate_posterior <- function(n, data, mu0, Omega0, nu0, sigma02) {
  # Compute the sufficient statistics
  X <- cbind(1, data$time, data$time^2)
  y <- data$temp
  S <- t(X) %*% X
  m <- t(X) %*% y
  
  # Update the hyperparameters
  Omega_n <- solve(solve(Omega0) + S)
  mu_n <- Omega_n %*% (solve(Omega0) %*% mu0 + m)
  nu_n <- nu0 + length(y)
  sigma_n2 <- (nu0 * sigma02 + t(y) %*% y + t(mu0) %*% solve(Omega0) %*% mu0 - t(mu_n) %*% solve(Omega_n) %*% mu_n) / nu_n
  
  # Simulate draws from the joint posterior distribution
  beta_samples <- rmvnorm(n, mu_n, Omega_n)
  sigma_samples <- 1 / rchisq(n, df = nu_n, ncp = sigma_n2 * nu_n)
  
  list(beta = beta_samples, sigma = sigma_samples)
}


beta <- rmvnorm(n, mu0, Omega0)
sigma2 <- 1 / rchisq(n, nu0, ncp = nu0 * sigma02)

# Simulate draws from the joint posterior
simulate_posterior <- function(n, data, mu0, Omega0, nu0, sigma02) {
  # Initialize arrays to store the samples
  beta_samples <- matrix(0, n, 3)
  sigma2_samples <- numeric(n)
  
  # Initialize the parameters
  beta <- mu0
  sigma2 <- sigma02
  
  for (i in 1:n) {
    # Update beta
    Omega_n <- solve(solve(Omega0) + t(data$time) %*% data$time / sigma2)
    mu_n <- Omega_n %*% (solve(Omega0) %*% mu0 + t(data$time) %*% data$temp / sigma2)
    beta <- rmvnorm(1, mu_n, Omega_n)
    
    # Update sigma2
    nu_n <- nu0 + length(data$temp)
    sigma02_n <- (nu0 * sigma02 + t(data$temp - data$time %*% beta) %*% (data$temp - data$time %*% beta)) / nu_n
    sigma2 <- 1 / rchisq(1, nu_n, ncp = nu_n * sigma02_n)
    
    # Store the samples
    beta_samples[i, ] <- beta
    sigma2_samples[i] <- sigma2
  }
  
  list(beta = beta_samples, sigma2 = sigma2_samples)
}


# Plot histogram for each marginal posterior
plot_histogram <- function(draws) {
  par(mfrow = c(2, 2))
  hist(draws$beta[,1], main = "Posterior of beta0", xlab = "beta0")
  hist(draws$beta[,2], main = "Posterior of beta1", xlab = "beta1")
  hist(draws$beta[,3], main = "Posterior of beta2", xlab = "beta2")
  hist(draws$sigma2, main = "Posterior of sigma2", xlab = "sigma2")
}


# Plot scatter plot of the temperature data and overlay a curve for the posterior median
plot_scatter <- function(data, draws) {
  plot(data$time, data$temp, xlab = "Time", ylab = "Temperature")
  f_time <- apply(draws$beta, 1, function(beta) beta[1] + beta[2] * data$time + beta[3] * data$time^2)
  lines(data$time, apply(f_time, 2, median), col = "red")
}


# Simulate from the posterior distribution of x~
simulate_x_tilde <- function(draws) {
  x_tilde <- -draws$beta[,2] / (2 * draws$beta[,3])
  x_tilde
}


# Suggest a suitable prior
suggest_prior <- function() {
  mu0 <- rep(0, 11)
  Omega0 <- diag(c(1, rep(0.01, 10)))
  list(mu0 = mu0, Omega0 = Omega0)
}

