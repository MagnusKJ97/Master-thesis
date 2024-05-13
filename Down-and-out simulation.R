# Define the parameters
S0 <- 1.0689  # initial stock price
K <- 1.0689  # strike price
B <- 0.97  # barrier
T <- 252/252  # time to maturity
rd <- 0.03352 # risk-free interest rate
rf <- 0.05202
mu <- rd-rf  # annual drift
sigma <- 0.11055126     # volatility
N_STEPS <- 252  # number of time steps
N_PATHS <- 1000  # number of paths

# Calculate the time step
dt <- T / N_STEPS

# Initialize the price array
prices <- matrix(0, nrow = N_PATHS, ncol = N_STEPS)

# Simulate the paths
for (i in 1:N_PATHS) {
  s_curr <- S0
  for (j in 1:N_STEPS) {
    # Generate a random normal variable
    epsilon <- rnorm(1, mean = 0, sd = 1)
    # Calculate the stock price at the next time step
    s_curr <- s_curr + mu * s_curr * dt + sigma * s_curr * sqrt(dt) * epsilon
    # Check if the barrier is hit
    if (s_curr <= B) {
      prices[i, j] <- 0
      break
    } else {
      prices[i, j] <- s_curr
    }
  }
}

# Calculate the payoff for each path
payoffs <- ifelse(prices[, N_STEPS] > K, prices[, N_STEPS] - K, 0)

# Calculate the option price
option_price <- mean(payoffs) * exp(-rd * T)

# Print the result
cat("Option Price:", option_price)
