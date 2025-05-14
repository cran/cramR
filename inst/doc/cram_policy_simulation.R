## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(cramR)
library(data.table)
library(DT)

## -----------------------------------------------------------------------------
set.seed(123)

# dgp_X <- function(n) {
#   data.table::data.table(
#     binary     = rbinom(n, 1, 0.5),
#     discrete   = sample(1:5, n, replace = TRUE),
#     continuous = rnorm(n)
#   )
# }

n <- 100

X_data <- data.table::data.table(
    binary     = rbinom(n, 1, 0.5),
    discrete   = sample(1:5, n, replace = TRUE),
    continuous = rnorm(n)
  )


dgp_D <- function(X) rbinom(nrow(X), 1, 0.5)

dgp_Y <- function(D, X) {
  theta <- ifelse(
    X[, binary] == 1 & X[, discrete] <= 2,  # Group 1: High benefit
    1,
    ifelse(X[, binary] == 0 & X[, discrete] >= 4,  # Group 3: Negative benefit
           -1,
           0.1)  # Group 2: Neutral effect
  )
  Y <- D * (theta + rnorm(length(D), mean = 0, sd = 1)) +
    (1 - D) * rnorm(length(D))  # Outcome for untreated
  return(Y)
}

# Parameters
nb_simulations <- 100
nb_simulations_truth <- 200
batch <- 5

# Perform CRAM simulation
result <- cram_simulation(
  X = X_data,
  dgp_D = dgp_D,
  dgp_Y = dgp_Y,
  batch = batch,
  nb_simulations = nb_simulations,
  nb_simulations_truth = nb_simulations_truth,
  sample_size = 500
)

## -----------------------------------------------------------------------------
result$raw_results

## -----------------------------------------------------------------------------
result$interactive_table

## ----cleanup-autograph, include=FALSE-----------------------------------------
autograph_files <- list.files(tempdir(), pattern = "^__autograph_generated_file.*\\.py$", full.names = TRUE)
if (length(autograph_files) > 0) {
  try(unlink(autograph_files, recursive = TRUE, force = TRUE), silent = TRUE)
}


