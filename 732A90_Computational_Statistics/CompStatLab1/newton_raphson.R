x <- c(-2.8,3.4,1.2,-0.3,-2.6)

# Define the range of theta
theta <- seq(-4, 4, length.out = 1000)

# Define the log likelihood function
log_likelihood <- function(x, theta) {
  n <- length(x)
  return(-n * log(pi) - sum(log(1 + (x - theta)^2)))
}

# First derivative
derivative <- function(x, theta) {
  return(sum(2 * (x - theta) / (1 + (x - theta)^2)))
}

# Second derivative
derivative_second <- function(x, theta) {
  return(sum(2 * ((x - theta)^2 - 1) / (1 + (x - theta)^2)^2))
}

# Calculate the log likelihood and its derivative for each theta
log_likelihood_values <- sapply(theta, log_likelihood, x = x)
derivative_values <- sapply(theta, derivative, x = x)

# Plot the log likelihood function
plot(theta, log_likelihood_values, type = "l", main = "Log Likelihood Function", xlab = "Theta", ylab = "Log Likelihood")
abline(h = 0, col = "red")

# Plot the derivative of the log likelihood function
plot(theta, derivative_values, type = "l", main = "Derivative of Log Likelihood Function", xlab = "Theta", ylab = "Derivative")
abline(h = 0, col = "red")


# Newton Raphson function
newton_raphson <- function (x, inital_theta) {
  tol <- 1e-6
  new_theta <- inital_theta
  #old_theta <- inital_theta + 2
  #while ( abs(new_theta - old_theta) > tol) {
  #  old_theta <- new_theta
  #  new_theta <- old_theta - derivative(x, old_theta) / (derivative_second(x, old_theta) + 1e-6)
  #}
  for (i in 1:10000) {
    old_theta <- new_theta
    new_theta <- old_theta - derivative(x, old_theta) / (derivative_second(x, old_theta) + 1e-8)
    if ( abs(new_theta - old_theta) > tol ) {
      break
    }
  }
  #cat("The estimated theta is:", new_theta, "\n")
  return(new_theta)
}

# Perform a multi-start strategy that is running the Newton Raphson function multiple times
# from different randomly chosen starting points and then selecting the best solution.

multi_start_strategy <- function(x, num_starts, lower_bound, upper_bound) {
  set.seed(123)
  # Generate random starting values
  theta_initial_values <- seq(lower_bound, upper_bound, length.out = num_starts)
  # Apply newton_raphson method to each starting values
  theta_opt_values <- sapply(theta_initial_values, newton_raphson, x = x)

  # Evaluate the log_likelihood function at each optimized theta value
  log_likelihood_opt_values <- sapply(theta_opt_values, log_likelihood, x = x)

  # Find the theta value that gives the maximum log-likelihood
  theta_global_max <- theta_opt_values[which.max(log_likelihood_opt_values)]
  log_likelihood_max <- log_likelihood(x, theta_global_max)
  cat("The theta value that gives the global maximun of the log_likelihood function is: ", theta_global_max, "\n")
  cat("Global maximun of the log_likelihood function is: ", log_likelihood_max,"\n")
}

