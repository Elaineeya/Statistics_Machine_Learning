myvar <- function(X) {
  if ( !is.vector(X) ) {
    stop("Argument x should be a vector of n observations")
  }
  n <- length(X)
  if ( n < 1) {
    stop("Argument X's size must be greater than 1 for variance estimation.")
  }

  sum_x_squared <- sum(x^2)
  sum_x <- sum(X)

  estimate_variance <- (1 / (n-1) ) * (sum_x_squared - ( 1 / n ) * sum_x^2)

  return(estimate_variance)
}

# Generate a vector x = (x1, . . . , x10000)
set.seed(123)
X <- rnorm(n = 10000, mean = 100000000, sd = sqrt(1))

# Compute the difference between myvar and var
Y <- numeric(10000)
for ( i in 1:length(X)) {
  diff <- myvar(X[1:i]) - var(X[1:i])
  Y[i] <- diff
}

# Plot the dependence Y on i
n <- 1:10000
plot(n, Y, type = "l", main = "Dependence Yi", xlab = "Subset Size (i)", ylab = "Yi = myvar(Xi) - var(Xi)")
abline(h = 0, col = "red")


# better implement a variance estimater using original formula
better_var <- function(X) {
  if ( !is.vector(X) ) {
    stop("Argument x should be a vector of n observations")
  }
  n <- length(X)
  if ( n < 1) {
    stop("Argument X's size must be greater than 1 for variance estimation.")
  }

  mean_x <- mean(X)
  estimate_variance <- sum((X - mean_x)^2) / (n-1)

  return(estimate_variance)
}

# Compute the difference between myvar() and var()
Z <- numeric(10000)
for ( i in 1:length(X)) {
  Z[i] <- better_var(X[1:i]) - var(X[1:i])
}

# Plot the Difference Z on i
plot(1:10000, Z, pch = 1, main = "Difference between better_var and var", xlab = "Subset Size (i)", ylab = "Zi = better_var(Xi) - var(Xi)")
abline(h = 0, col = "red")



