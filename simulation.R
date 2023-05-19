library(tidyverse)
library(pbapply)

load_dependencies <- function() {
    source("./generate_data.R")
    source("./estimation.R")
    source("./nuisance.R")

    library(grf)
}

simulation <- function(round) {
    num.folds <- 2

    data <- generate_data(1000, 3, 5, 1, c(0.3, 0, 0), c(0.5, 1.5, 3))
    fold <- rep(1:num.folds, length.out = nrow(data))
    data <- cbind(data, fold)

    est_ate_ipw <- numeric()
    est_ate_aipw <- numeric()

    # Crossfitting
    for (k in 1:num.folds) {
        train <- data[data$fold != k, ]
        test <- data[data$fold == k, ]

        nui_learners <- train_nuisance_learners(train, c("rf"), "Y", "D", c("X.1", "X.2", "X.3"))
        nui <- nuisance_fitted_value(test, "rf", nui_learners)

        est_ate_ipw[k] <- ate_ipw(test, nui$pscore_fit_rf)
        est_ate_aipw[k] <- ate_aipw(test, nui$pscore_fit_rf, nui$response_treat_fit_rf, nui$response_contr_fit_rf)
    }

    # Take average of cross fitted ATE
    est_ate_ipw <- mean(est_ate_ipw)
    est_ate_aipw <- mean(est_ate_aipw)

    est_ate_ols <- ate_ols(data, "Y", "D", c("X.1", "X.2", "X.3"))
    forest <- causal_forest(data[c("X.1", "X.2", "X.3")], data$Y, data$D)
    est_ate_forest <- average_treatment_effect(forest, method = "AIPW")[1]

    list(
        ate_ipw = est_ate_ipw,
        ate_aipw = est_ate_aipw,
        ate_ols = est_ate_ols,
        ate_forest = est_ate_forest
    )
}

# Parallel clusters
cl <- parallel::makeCluster(16)

parallel::clusterExport(cl, c("load_dependencies", "simulation"))
parallel::clusterEvalQ(cl, load_dependencies())
simulation_result <- pblapply(1:100, simulation, cl = cl) %>% bind_rows()
parallel::stopCluster(cl)

saveRDS(simulation_result, "./simulation_result.rds")
