library(tidyverse)
library(progressr)

load_dependencies <- function() {
    source("./generate_data.R")
    source("./estimator.R")
    source("./nuisance.R")
}

simulate <- function(round, sample, num.folds = 2, learner, tuned_params, X_pscore, X_resp, prog = NULL) {
    # num.folds: number of folds for crossfitting

    # Update progress bar
    if (!is.null(prog)) {
        prog(message = sprintf("Simulating... %g", round))
    }

    fold <- rep(1:num.folds, length.out = nrow(sample))
    sample <- cbind(sample, fold)

    est_ate_ipw <- numeric()
    est_ate_aipw <- numeric()
    est_ate_plr <- numeric()

    # Crossfitting
    for (k in 1:num.folds) {
        train <- sample[sample$fold != k, ]
        test <- sample[sample$fold == k, ]

        tasks <- generate_nuisance_tasks(train, "Y", "D", X_pscore, X_resp)
        pred <- train_all_learners(learner, tasks, tuned_params, test)

        est_ate_ipw[k] <- ate_ipw(test, pred$pscore)
        est_ate_aipw[k] <- ate_aipw(test, pred$pscore, pred$resp_1, pred$resp_0)
        est_ate_plr[k] <- ate_plr(test, pred$pscore, pred$resp)
    }

    # Take average of cross fitted ATE
    est_ate_ipw <- mean(est_ate_ipw)
    est_ate_aipw <- mean(est_ate_aipw)
    est_ate_plr <- mean(est_ate_plr)

    est_ate_ols <- ate_ols(sample, X_resp)

    return(
        list(
            ate_ipw = est_ate_ipw,
            ate_aipw = est_ate_aipw,
            ate_plr = est_ate_plr,
            ate_ols = est_ate_ols
        )
    )
}

simulation_setup <- function(rounds, lrn_type = "lasso", spec_variant = "both") {
    # Set specifications
    if (spec_variant == "pscore") {
        X_pscore <- paste("X", 1:10, sep = ".") # All covariates
        X_resp <- c("X.2", "X.4") # Wrongly specified
    } else if (spec_variant == "resp") {
        X_pscore <- c("X.2", "X.4")  # Wrongly specified
        X_resp <- paste("X", 1:10, sep = ".") # All covariates
    } else if (spec_variant == "both") {
        X_pscore <- paste("X", 1:10, sep = ".")
        X_resp <- paste("X", 1:10, sep = ".")
    } else if (spec_variant == "none") {
        X_pscore <- paste("X", 5:10, sep = ".")
        X_resp <- paste("X", 5:10, sep = ".")
    }

    # Generate list of simulation samples
    sim_samples <- generate_sim_samples(S = rounds, n = 2000, tau = 5, seed = 20230522)

    # Tune hyperparameters using first sample of current DGP
    cat("Tuning hyperparameter...")
    data_tune <- sim_samples[[1]]
    tasks <- generate_nuisance_tasks(data_tune, "Y", "D", X_pscore, X_resp)
    learner <- generate_nuisance_learner(lrn_type)
    tuned_params <- tune_all_learners(learner, tasks)

    round <- 1:rounds

    prog <- progressor(along = round)
    simulation_result <- future.apply::future_lapply(
        round,
        function(r, prog) simulate(r, sim_samples[[r]], num.folds = 2, learner, tuned_params, X_pscore, X_resp, prog),
        prog = prog,
        future.seed = TRUE
    ) %>% bind_rows()

    return(simulation_result)
}
