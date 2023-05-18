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
    learner <- lrn("regr.ranger")
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
train_nuisance_learners <- function(data, Y, D, X) {
    task_pscore <- as_task_regr(data, target = D, predictors = X)
    task_response <- as_task_regr(data, target = Y, predictors = X)
    task_response_treat <- as_task_regr(data[data$D == 0,], target = Y, predictors = X)
    task_response_contr <- as_task_regr(data[data$D == 1,], target = Y, predictors = X)

    return(
        list(
            # Propensity score
            pscore_lasso = learner_lasso(task_pscore),
            pscore_rf = learner_rf(task_pscore),
            pscore_xgboost = learner_xgboost(task_pscore),

            # Response function (for PLR)
            response_lasso = learner_lasso(task_response),
            response_rf = learner_rf(task_response),
            response_xgboost = learner_xgboost(task_response),

            # Response function for the treatment group
            response_treat_lasso = learner_lasso(task_response_treat),
            response_treat_rf = learner_rf(task_response_treat),
            response_treat_xgboost = learner_xgboost(task_response_treat),

            # Response function for the control group
            response_contr_lasso = learner_lasso(task_response_contr),
            response_contr_rf = learner_rf(task_response_contr),
            response_contr_xgboost = learner_xgboost(task_response_contr)
        )
    )
}

nuisance_fitted_value <- function(data, trained_learners) {
    return(
        list(
            pscore_fit_lasso = predict(trained_learners$pscore_lasso, data),
            pscore_fit_rf = predict(trained_learners$pscore_rf, data),
            pscore_fit_xgboost = predict(trained_learners$pscore_xgboost, data),

            response_fit_lasso = predict(trained_learners$response_lasso, data),
            response_fit_rf = predict(trained_learners$response_rf, data),
            response_fit_xgboost = predict(trained_learners$response_xgboost, data),

            response_treat_fit_lasso = predict(trained_learners$response_treat_lasso, data),
            response_treat_fit_rf = predict(trained_learners$response_treat_rf, data),
            response_treat_fit_xgboost = predict(trained_learners$response_treat_xgboost, data),

            response_contr_fit_lasso = predict(trained_learners$response_contr_lasso, data),
            response_contr_fit_rf = predict(trained_learners$response_contr_rf, data),
            response_contr_fit_xgboost = predict(trained_learners$response_contr_xgboost, data)
        )
    )
}