# Load CVXR
library(CVXR)

# Assume we have 6 features
n <- 6

# Optimization variable
x <- Variable(n)

# Estimated parameters (dummy values – replace with real ones from logistic model)
a <- c(-0.05, 0.1, 0.03, 0.02, 0.04, 0.01)  # Logistic regression weights
b <- -3                                    # Intercept

# Profit model parameters
c <- c(5, 3, 2, 4, 1, 6)   # Profit contribution per feature
d <- 100                   # Base profit from policy

# Linear constraints (example bounds)
F_mat <- diag(n)
g_vec <- c(70, 1, 35, 10, 10, 100000)  # Upper bounds for each variable
constraints <- list(F_mat %*% x <= g_vec,
                    x >= 0)            # All features must be non-negative

# Define expected profit (objective function)
logit_prob <- 1 / (1 + exp(t(a) %*% x + b))
expected_profit <- (t(c) %*% x + d) * logit_prob

# Define and solve the optimization problem
problem <- Problem(Maximize(expected_profit), constraints)
result <- solve(problem)

# Output optimal profile
optimal_x <- result$getValue(x)
cat("Optimal customer profile:\n")
print(optimal_x)

cat("Maximum expected profit:\n")
print(result$value)
