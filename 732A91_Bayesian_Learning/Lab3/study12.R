# Load necessary libraries
library(MASS)  # for multivariate normal distribution
library(BayesLogit)  # for Polya-Gamma distribution
library(coda)  # for convergence diagnostics
library(mvtnorm)
set.seed(12345)

# Load the data
data <- read.table("WomenAtWork.dat", header = TRUE)

# Response and Features
y <- data$Work
X <- as.matrix(data[,-1])

# Define the logistic function
logistic <- function(x) {
  return(exp(x) / (1 + exp(x)))
}

# Initialize parameters
n <- nrow(data)
p <- ncol(data) - 1  # subtract one for the response variable
beta <- matrix(0, nrow = 1000, ncol = p)
tau <- 3
omega <- rep(1, n)

# Gibbs sampler
for (iter in 1:1000) {
  # Update beta
  Sigma <- solve(t(X) %*% diag(omega) %*% X + diag(1/tau^2, p))
  mu <- Sigma %*% t(X) %*% (y - 1/2)
  beta[iter,] <- mvrnorm(1, mu, Sigma)
  
  # Update omega using Polya-Gamma sampler
  for (i in 1:n) {
    omega[i] <- rpg(1, abs(X[i,] %*% beta[iter,]))
    #eta <- sum(X[i,] * beta[iter,])
    #omega[i] <- rpg(1, abs(eta))
  }
}


# Evaluate convergence
beta_mcmc <- as.mcmc(beta)
#pdf("plot.pdf", width = 10, height = 10)
summary(beta_mcmc)
dev.new()
plot(beta_mcmc)
dev.off()
effectiveSize(beta_mcmc)


a_Gibbs <- acf(beta)


IF_Gibbs <- 1+2*sum(a_Gibbs$acf[-1])


# Compute 90% credible interval for Pr(y = 1|x)
posterior_prob <- apply(beta, 1, function(b) logistic(sum(b * c(1, 22, 12, 7, 38, 1, 0))))
quantile(posterior_prob, probs = c(0.05, 0.95))


prob <- plogis(sum(c(1, 22, 12, 7, 38, 1, 0) * beta))
quantile(prob, probs = c(0.05, 0.95))


var1      var2      var3      var4      var5      var6      var7 
696.3593  255.6023  166.5617  153.8279  461.4228  142.2034 2429.7449 
var1      var2      var3      var4      var5      var6      var7 
1313.7981  160.5816  148.4596  105.7966  158.8862  113.2149  787.8307 