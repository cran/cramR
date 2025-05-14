## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(cramR)
library(DT)

## -----------------------------------------------------------------------------
# Set random seed for reproducibility
set.seed(42)

# Define parameters
T <- 100  # Number of timesteps
K <- 4    # Number of arms

# Simulate a 3D array `pi` of shape (T, T, K)
# - First dimension: Individuals (context Xj)
# - Second dimension: Time steps (pi_t)
# - Third dimension: Arms (depth)
pi <- array(runif(T * T * K, 0.1, 1), dim = c(T, T, K))

# Normalize probabilities so that each row sums to 1 across arms
for (t in 1:T) {
  for (j in 1:T) {
    pi[j, t, ] <- pi[j, t, ] / sum(pi[j, t, ])  
  }
}

# Simulate arm selections (randomly choosing an arm)
arm <- sample(1:K, T, replace = TRUE)

# Simulate rewards (assume normally distributed rewards)
reward <- rnorm(T, mean = 1, sd = 0.5)

## -----------------------------------------------------------------------------
result <- cram_bandit(pi, arm, reward, batch=1, alpha=0.05)

## -----------------------------------------------------------------------------
result$raw_results

## -----------------------------------------------------------------------------
result$interactive_table

## ----cleanup-autograph, include=FALSE-----------------------------------------
autograph_files <- list.files(tempdir(), pattern = "^__autograph_generated_file.*\\.py$", full.names = TRUE)
if (length(autograph_files) > 0) {
  try(unlink(autograph_files, recursive = TRUE, force = TRUE), silent = TRUE)
}


