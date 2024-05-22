# Load necessary libraries
library(MASS)
library(BayesLogit)
library(coda)
library(mvtnorm)
set.seed(12345)

# Load the data
data <- read.table("WomenAtWork.dat", header = TRUE)

# Define the logistic function
logistic <- function(x) {
  return(exp(x) / (1 + exp(x)))
}

# Define Response and Features
y <- data$Work
X <- as.matrix(data[,-1])
n <- nrow(X)
p <- ncol(X)


# Initialize parameters
tau <- 3
B <- diag(rep(tau^2, p))
b <- rep(0, p)
w <- rep(1, n)
k <- y - 1/2

# Gibbs sampler
n_iter <- 1000
chain_b <- matrix(0, nrow = n_iter, ncol = p)
for (iter in 1:n_iter) {
  for (i in 1:n) {
    w[i] <- rpg(1, abs(X[i,] %*% b) + 1e-10)
  }
  Vw <- solve(t(X) %*% diag(w) %*% X + solve(B))
  mw <- Vw %*% (t(X) %*% k + solve(B) %*% b)
  b <- rnorm(p, mean = mw, sd = sqrt(diag(Vw)))
  chain_b[iter,] <- b
}

# Evaluate convergence
chain_b_mcmc <- mcmc(chain_b)
dev.new()
plot(chain_b_mcmc)
summary(chain_b_mcmc)
effectiveSize(chain_b_mcmc)


# Calculate Inefficiency Factors (IFs)
IFs <- apply(chain_b, 2, function(x) {
  acf_x <- acf(x, plot = FALSE)$acf[,,1]
  1 + 2 * sum(acf_x[2:length(acf_x)])
})
print(IFs)


# Compute 90% credible interval
x_new <- c(1, 22, 12, 7, 38, 1, 0)
pr_y <- logistic(x_new %*% t(chain_b))
CI_90 <- quantile(pr_y, probs = c(0.05, 0.95), na.rm = TRUE)

