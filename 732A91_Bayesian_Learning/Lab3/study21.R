# Load necessary libraries
library(MASS)

# Load the data
data <- read.table("eBayNumberOfBidderData_2024.dat", header = TRUE)

# (a) Obtain the maximum likelihood estimator of β in the Poisson regression model
model <- glm(nBids ~ PowerSeller + VerifyID + Sealed + Minblem + MajBlem + LargNeg + LogBook + MinBidShare, 
             family = poisson(link = "log"), data = data)
summary(model)

# (b) Bayesian analysis of the Poisson regression
prior <- function(beta, X, y) {
  n <- dim(X)[1]
  p <- dim(X)[2]
  beta_prior <- rep(0, p)
  Sigma_prior <- 100 * solve(t(X) %*% X)
  dmvnorm(beta, mean = beta_prior, sigma = Sigma_prior)
}

posterior <- function(beta, X, y) {
  likelihood <- sum(dpois(y, lambda = exp(X %*% beta), log = TRUE))
  prior <- prior(beta, X, y)
  return(likelihood + prior)
}

optim_result <- optim(par = coef(model), fn = posterior, X = model.matrix(model), y = data$nBids, 
                      control = list(fnscale = -1), hessian = TRUE)
beta_hat <- optim_result$par
Sigma_hat <- solve(-optim_result$hessian)

# (c) Metropolis algorithm
metropolis <- function(posterior, init, iter, Sigma, c, X, y) {
  chain <- matrix(NA, nrow = iter, ncol = length(init))
  chain[1, ] <- init
  for (i in 2:iter) {
    proposal <- mvrnorm(n = 1, mu = chain[(i - 1), ], Sigma = c * Sigma)
    log_prob <- exp(posterior(proposal, X, y) - posterior(chain[(i - 1), ], X, y))
    accept_prob <- min(1,log_prob)
    if (runif(1) <= accept_prob) {
      chain[i, ] <- proposal
    } else {
      chain[i, ] <- chain[(i - 1), ]
    }
  }
  return(chain)
}

#chain <- metropolis(posterior, init = beta_hat, iter = 10000, Sigma = Sigma_hat, c = 0.1, 
#                   X = model.matrix(model), y = data$nBids)
X = model.matrix(model)
inital_0 <- rep(0, ncol(X))
chain <- metropolis(posterior, init = inital_0, iter = 10000, Sigma = Sigma_hat, c = 0.1, 
                   X = model.matrix(model), y = data$nBids)

#chain <- metropolis(posterior, init = coef(model), iter = 10000, Sigma = Sigma_hat, c = 0.1, 
#                    X = model.matrix(model), y = data$nBids)

# Assess MCMC convergence
plot(chain[, 1], type = "l")  # for the first parameter
plot(chain[, 2], type = "l")  # for the second parameter

chain_mcmc <- mcmc(chain)
dev.new()
plot(chain_mcmc)

names <- colnames(data[,-1])
par(mfrow = c(2, 2)) 
for (i in 1:9) {
  plot(chain[, i], main = paste("Trace of", names[i]), type = "l", xlab='Iterations', ylab= paste(names[i]))
}
par(mfrow = c(1, 1))  # Reset the plot layout



# (d) Predictive distribution
new_auction <- c(1, 1, 0, 1, 0, 1, 0, 1.2, 0.8)
pred_draws <- rpois(10000, lambda = exp(new_auction %*% t(chain)))
hist(pred_draws, freq = FALSE,breaks = seq(-0.5, 8.5, 1), main = "Predictive distribution", xlab = "Number of bidders")
prob_no_bidders <- mean(pred_draws == 0)
prob_no_bidders