## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(cramR)
library(data.table)
library(DT)

## ----contextual-example, fig.alt="Cumulative regret curve over time for the selected policy"----

# Number of time steps
horizon       <- 500L

# Number of simulations 
simulations   <- 100L

# Number of arms
k = 4

# Number of context features
d= 3

# Reward beta parameters of linear model (the outcome generation models, one for each arm, are linear with arm-specific parameters betas)
list_betas <- cramR::get_betas(simulations, d, k)

# Define the contextual linear bandit, where sigma is the scale of the noise in the outcome linear model
bandit        <- cramR::ContextualLinearBandit$new(k = k, d = d, list_betas = list_betas, sigma = 0.3)

# Define the policy object (choose between Contextual Epsilon Greedy, UCB Disjoint and Thompson Sampling)
policy <- cramR::BatchContextualEpsilonGreedyPolicy$new(epsilon=0.1, batch_size=5)
# policy <- cramR::BatchLinUCBDisjointPolicyEpsilon$new(alpha=1.0, epsilon=0.1, batch_size=1)
# policy <- cramR::BatchContextualLinTSPolicy$new(v = 0.1, batch_size=1)


sim <- cram_bandit_sim(horizon, simulations,
                            bandit, policy,
                            alpha=0.05, do_parallel = FALSE)
  

## ----contextual-estimates, fig.alt = "First rows of simulation output estimates"----
head(sim$estimates)

## ----contextual-table, fig.alt = "Table of results"---------------------------
sim$interactive_table

## ----cleanup-autograph, include=FALSE-----------------------------------------
autograph_files <- list.files(tempdir(), pattern = "^__autograph_generated_file.*\\.py$", full.names = TRUE)
if (length(autograph_files) > 0) {
  try(unlink(autograph_files, recursive = TRUE, force = TRUE), silent = TRUE)
}


