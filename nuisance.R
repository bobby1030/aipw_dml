library(mlr3)
library(mlr3learners)

# Estimate nuisance function using LASSO
learner_lasso <- function(task) {
    learner <- lrn("regr.cv_glmnet", nfolds = 10)
    learner$train(task)
    return(learner)
}

# Estimate nuisance function using random forest
learner_rf <- function(task) {
    learner <- lrn(
        "regr.ranger",
        num.trees = 2000,
        min.node.size = 5,
        sample.fraction = 0.5,
        mtry = 3,
        num.threads = 8
    )
    learner$train(task)
    return(learner)
}

# Esimate nuisance function using gradient boosting
learner_xgboost <- function(task) {
    learner <- lrn("regr.xgboost")
    learner$train(task)
    return(learner)
}

# High-level function for fitting nuisance functions
train_nuisance_learners <- function(data, learner_types, Y, D, X) {
    task_pscore <- as_task_regr(data, target = D)$select(X)
    task_response <- as_task_regr(data, target = Y)$select(X)
    task_response_treat <- as_task_regr(data[data$D == 1, ], target = Y)$select(X)
    task_response_contr <- as_task_regr(data[data$D == 0, ], target = Y)$select(X)

    learners <- list()

    if ("lasso" %in% learner_types) {
        learners_lasso <- list(
            pscore_lasso = learner_lasso(task_pscore),
            response_lasso = learner_lasso(task_response),
            response_treat_lasso = learner_lasso(task_response_treat),
            response_contr_lasso = learner_lasso(task_response_contr)
        )
        learners <- c(learners, learners_lasso)
    }

    if ("rf" %in% learner_types) {
        learners_rf <- list(
            pscore_rf = learner_rf(task_pscore),
            response_rf = learner_rf(task_response),
            response_treat_rf = learner_rf(task_response_treat),
            response_contr_rf = learner_rf(task_response_contr)
        )
        learners <- c(learners, learners_rf)
    }

    if ("xgboost" %in% learner_types) {
        learners_xgboost <- list(
            pscore_xgboost = learner_xgboost(task_pscore),
            response_xgboost = learner_xgboost(task_response),
            response_treat_xgboost = learner_xgboost(task_response_treat),
            response_contr_xgboost = learner_xgboost(task_response_contr)
        )
        learners <- c(learners, learners_xgboost)
    }

    return(learners)
}

nuisance_fitted_value <- function(test, learner_types, trained_learners) {

    fitted_values <- list()

    if ("lasso" %in% learner_types) {
        fitted_values <- c(
            fitted_values,
            list(
                pscore_fit_lasso = predict(trained_learners$pscore_lasso, test),
                response_fit_lasso = predict(trained_learners$response_lasso, test),
                response_treat_fit_lasso = predict(trained_learners$response_treat_lasso, test),
                response_contr_fit_lasso = predict(trained_learners$response_contr_lasso, test)
            )
        )
    }

    if ("rf" %in% learner_types) {
        fitted_values <- c(
            fitted_values,
            list(
                pscore_fit_rf = predict(trained_learners$pscore_rf, test),
                response_fit_rf = predict(trained_learners$response_rf, test),
                response_treat_fit_rf = predict(trained_learners$response_treat_rf, test),
                response_contr_fit_rf = predict(trained_learners$response_contr_rf, test)
            )
        )
    }

    if ("xgboost" %in% learner_types) {
        fitted_values <- c(
            fitted_values,
            list(
                pscore_fit_xgboost = predict(trained_learners$pscore_xgboost, test),
                response_fit_xgboost = predict(trained_learners$response_xgboost, test),
                response_treat_fit_xgboost = predict(trained_learners$response_treat_xgboost, test),
                response_contr_fit_xgboost = predict(trained_learners$response_contr_xgboost, test)
            )
        )
    }

    return(fitted_values)
}