## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(cramR)
library(caret)
library(DT)

## -----------------------------------------------------------------------------
set.seed(42)
X_data <- data.frame(x1 = rnorm(100), x2 = rnorm(100), x3 = rnorm(100))
Y_data <- rnorm(100)
data_df <- data.frame(X_data, Y = Y_data)

## -----------------------------------------------------------------------------
caret_params_lm <- list(
  method = "lm",
  trControl = trainControl(method = "none")
)

result <- cram_ml(
  data = data_df,
  formula = Y ~ .,
  batch = 5,
  loss_name = "se",
  caret_params = caret_params_lm
)
print(result)

## ----eval = requireNamespace("randomForest", quietly = TRUE)------------------
set.seed(42)

# Generate binary classification dataset
X_data <- data.frame(x1 = rnorm(100), x2 = rnorm(100), x3 = rnorm(100))
Y_data <- rbinom(nrow(X_data), 1, 0.5)
data_df <- data.frame(X_data, Y = Y_data)

# Define caret parameters: predict labels (default behavior)
caret_params_rf <- list(
  method = "rf",
  trControl = trainControl(method = "none")
)

# Run CRAM ML with accuracy as loss
result <- cram_ml(
  data = data_df,
  formula = Y ~ .,
  batch = 5,
  loss_name = "accuracy",
  caret_params = caret_params_rf,
  classify = TRUE
)

print(result)

## ----eval = requireNamespace("randomForest", quietly = TRUE)------------------
set.seed(42)

# Generate binary classification dataset
X_data <- data.frame(x1 = rnorm(100), x2 = rnorm(100), x3 = rnorm(100))
Y_data <- rbinom(nrow(X_data), 1, 0.5)
data_df <- data.frame(X_data, Y = Y_data)

# Define caret parameters for probability output
caret_params_rf_probs <- list(
  method = "rf",
  trControl = trainControl(method = "none", classProbs = TRUE)
)

# Run CRAM ML with logloss as the evaluation loss
result <- cram_ml(
  data = data_df,
  formula = Y ~ .,
  batch = 5,
  loss_name = "logloss",
  caret_params = caret_params_rf_probs,
  classify = TRUE
)

print(result)

## -----------------------------------------------------------------------------
custom_fit <- function(data) {
  lm(Y ~ x1 + x2 + x3, data = data)
}

## -----------------------------------------------------------------------------
custom_predict <- function(model, data) {
  predictors_only <- data[, setdiff(names(data), "Y"), drop = FALSE]
  predict(model, newdata = predictors_only)
}

## -----------------------------------------------------------------------------
custom_loss <- function(predictions, data) {
  actuals <- data$Y
  se_loss <- (predictions - actuals)^2
  return(se_loss)
}

## -----------------------------------------------------------------------------
set.seed(42)
X_data <- data.frame(x1 = rnorm(100), x2 = rnorm(100), x3 = rnorm(100))
Y_data <- rnorm(100)
data_df <- data.frame(X_data, Y = Y_data)

result <- cram_ml(
  data = data_df,
  formula = Y ~ .,
  batch = 5,
  custom_fit = custom_fit,
  custom_predict = custom_predict,
  custom_loss = custom_loss
)
print(result)


## ----cleanup-autograph, include=FALSE-----------------------------------------
autograph_files <- list.files(tempdir(), pattern = "^__autograph_generated_file.*\\.py$", full.names = TRUE)
if (length(autograph_files) > 0) {
  try(unlink(autograph_files, recursive = TRUE, force = TRUE), silent = TRUE)
}


