# Load required library
library(rstan)

# Function to simulate AR(1) process
simulate_ar1 <- function(mu, phi, sigma_sq, T) {
  x <- numeric(T)
  x[1] <- mu
  for (t in 2:T) {
    e_t <- rnorm(1, mean = 0, sd = sqrt(sigma_sq))
    x[t] <- mu + phi * (x[t-1] - mu) + e_t
  }
  return(x)
}

# Simulate AR(1) process
mu <- 9
sigma_sq <- 4
T <- 250
phi_values <- seq(-1, 1, by = 0.2)
par(mfrow = c(2, 2)) 
for (phi in phi_values) {
  x <- simulate_ar1(mu, phi, sigma_sq, T)
  plot(x, main = paste("AR(1) process with phi =", phi), type = "l")
}
par(mfrow = c(1, 1))  # Reset the plot layout




# Stan code
stan_code <- "
data {
  int<lower=0> T;
  vector[T] y;
}
parameters {
  real mu;
  real<lower=-1, upper=1> phi;
  real<lower=0> sigma;
}
model {
  vector[T] nu;
  nu[1] <- mu;
  for (t in 2:T) {
    nu[t] <- mu + phi * (y[t-1] - mu);
  }
  y ~ normal(nu, sigma);
}
"

# Compile Stan model
stan_model <- stan_model(model_code = stan_code)

# Simulate two AR(1) processes
x1 <- simulate_ar1(mu, 0.3, sigma_sq, T)
y1 <- simulate_ar1(mu, 0.97, sigma_sq, T)

# Fit Stan model to the simulated data
fit_x1 <- sampling(stan_model, data = list(T = T, y = x1))
fit_y1 <- sampling(stan_model, data = list(T = T, y = y1))

# Print the results
print(fit_x1)
print(fit_y1)

print(summary(fit_x1)$summary)
pairs(fit_x1, pars = c("mu", "phi"))
pairs(fit_y1, pars = c("mu", "phi"))


stan_trace(fit_x1, pars=c("mu", "phi"))

stan_trace(fit_y1, pars=c("mu", "phi"))