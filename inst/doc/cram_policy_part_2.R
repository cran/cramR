## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(cramR)
library(DT)

## -----------------------------------------------------------------------------
library(data.table)
# Function to generate sample data with heterogeneous treatment effects:
# - Positive effect group
# - Neutral effect group
# - Adverse effect group
generate_data <- function(n) {
  X <- data.table(
    binary = rbinom(n, 1, 0.5),                 # Binary variable
    discrete = sample(1:5, n, replace = TRUE),  # Discrete variable
    continuous = rnorm(n)                       # Continuous variable
  )

  # Binary treatment assignment (50% treated)
  D <- rbinom(n, 1, 0.5)

  # Define heterogeneous treatment effects based on X
  treatment_effect <- ifelse(
    X[, binary] == 1 & X[, discrete] <= 2,        # Group 1: Positive effect
    1,
    ifelse(X[, binary] == 0 & X[, discrete] >= 4, # Group 3: Adverse effect
           -1,
           0.1)                                   # Group 2: Neutral effect
  )

  # Outcome depends on treatment effect + noise
  Y <- D * (treatment_effect + rnorm(n, mean = 0, sd = 1)) +
    (1 - D) * rnorm(n)

  return(list(X = X, D = D, Y = Y))
}

# Generate a sample dataset
set.seed(123)
n <- 1000
data <- generate_data(n)
X <- data$X
D <- data$D
Y <- data$Y


## -----------------------------------------------------------------------------
# Options for batch:
# Either an integer specifying the number of batches or a vector/list of batch assignments for all individuals
batch <- 20

# Model type for estimating treatment effects
# Options for model_type: 'causal_forest', 's_learner', 'm_learner'
# Note: you can also set model_type to NULL and specify custom_fit and custom_predict to use your custom model
model_type <- "causal_forest"  

# Options for learner_type:
# if model_type == 'causal_forest', choose NULL
# if model_type == 's_learner' or 'm_learner', choose between 'ridge', 'fnn' and 'caret'
learner_type <- NULL  

# Baseline policy to compare against (list of 0/1 for each individual)
# Options for baseline_policy:
# A list representing the baseline policy assignment for each individual.
# If NULL, a default baseline policy of zeros is created.
# Examples of baseline policy: 
# - All-control baseline: as.list(rep(0, nrow(X))) or NULL
# - Randomized baseline: as.list(sample(c(0, 1), nrow(X), replace = TRUE))
baseline_policy <- as.list(rep(0, nrow(X)))  

# Whether to parallelize batch processing (i.e. the cram method learns T policies, with T the number of batches.
# They are learned in parallel when parallelize_batch is TRUE
# vs. learned sequentially using the efficient data.table structure when parallelize_batch is FALSE, recommended for light weight training).
# Defaults to FALSE.
parallelize_batch <- FALSE  
 

# Model-specific parameters (more details in the article "Quick Start")
# Examples: NULL defaults to the following:
# - causal_forest: list(num.trees = 100)
# - ridge: list(alpha = 1)
# - caret: list(formula = Y ~ ., caret_params = list(method = "lm", trControl = trainControl(method = "none")))
# - fnn (Feedforward Neural Network): see below
# input_shape <- if (model_type == "s_learner") ncol(X) + 1 else ncol(X)
# default_model_params <- list(
#       input_layer = list(units = 64, activation = 'relu', input_shape = input_shape),
#       layers = list(
#         list(units = 32, activation = 'relu')
#       ),
#       output_layer = list(units = 1, activation = 'linear'),
#       compile_args = list(optimizer = 'adam', loss = 'mse'),
#       fit_params = list(epochs = 5, batch_size = 32, verbose = 0)
#     )
model_params <- NULL  

## -----------------------------------------------------------------------------
# Significance level for confidence intervals (default = 95%)
alpha <- 0.05  

# Run the CRAM policy method
result <- cram_policy(
  X, D, Y,
  batch = batch,
  model_type = model_type,
  learner_type = learner_type,
  baseline_policy = baseline_policy,
  parallelize_batch = parallelize_batch,
  model_params = model_params,
  alpha = alpha
)

# Display the results
print(result)


## -----------------------------------------------------------------------------
library(glmnet)

custom_fit <- function(X, Y, D, n_folds = 5) {
  treated_indices <- which(D == 1)
  control_indices <- which(D == 0)
  X_treated <- X[treated_indices, ]
  Y_treated <- Y[treated_indices]
  X_control <- X[control_indices, ]
  Y_control <- Y[control_indices]
  model_treated <- cv.glmnet(as.matrix(X_treated), Y_treated, alpha = 0, nfolds = n_folds)
  model_control <- cv.glmnet(as.matrix(X_control), Y_control, alpha = 0, nfolds = n_folds)
  tau_control <- Y_treated - predict(model_control, as.matrix(X_treated), s = "lambda.min")
  tau_treated <- predict(model_treated, as.matrix(X_control), s = "lambda.min") - Y_control
  X_combined <- rbind(X_treated, X_control)
  tau_combined <- c(tau_control, tau_treated)
  weights <- c(rep(1, length(tau_control)), rep(1, length(tau_treated)))
  final_model <- cv.glmnet(as.matrix(X_combined), tau_combined, alpha = 0, weights = weights, nfolds = n_folds)
  return(final_model)
}

## -----------------------------------------------------------------------------
custom_predict <- function(model, X_new, D_new) {
  cate <- predict(model, as.matrix(X_new), s = "lambda.min")

  # Apply decision rule: treat if CATE > 0
  as.integer(cate > 0)
}

## -----------------------------------------------------------------------------
experiment_results <- cram_policy(
  X, D, Y,
  batch = 20,
  model_type = NULL,
  custom_fit = custom_fit,
  custom_predict = custom_predict,
  alpha = 0.05
)
print(experiment_results)

## ----cleanup-autograph, include=FALSE-----------------------------------------
autograph_files <- list.files(tempdir(), pattern = "^__autograph_generated_file.*\\.py$", full.names = TRUE)
if (length(autograph_files) > 0) {
  try(unlink(autograph_files, recursive = TRUE, force = TRUE), silent = TRUE)
}


