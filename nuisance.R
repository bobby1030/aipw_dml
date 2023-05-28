library(mlr3)
library(mlr3learners)
library(mlr3tuning)

generate_nuisance_learner <- function(lnr.type) {
    if (lnr.type == "lasso") {
        learner <- lrn("regr.cv_glmnet", nfolds = 10)
    } else if (lnr.type == "rf") {
        learner <- lrn(
            "regr.ranger",
            num.trees = to_tune(2000, 4000),
            min.node.size = 5,
            sample.fraction = 0.5,
            mtry.ratio = to_tune(seq(0.2, 1, 0.2)),
            num.threads = 4
        )
    } else if (lnr.type == "xgboost") {
        learner <- lrn(
            "regr.xgboost",
            nrounds = to_tune(100, 3000),
            max_depth = 2,
            eta = 0.01,
            subsample = 0.5
        )
    } else {
        stop("Invalid learner type")
    }

    return(learner)
}

generate_nuisance_tasks <- function(data, Y, D, X_pscore, X_resp) {
    task_pscore <- as_task_regr(data, D)$select(X_pscore)
    task_resp <- as_task_regr(data, Y)$select(X_resp)
    task_resp_0 <- as_task_regr(data[data$D == 0, ], Y)$select(X_resp)
    task_resp_1 <- as_task_regr(data[data$D == 1, ], Y)$select(X_resp)

    return(
        list(
            pscore = task_pscore,
            resp = task_resp,
            resp_0 = task_resp_0,
            resp_1 = task_resp_1
        )
    )
}

tune_learner <- function(learner, task) {
    if (class(learner)[1] == "LearnerRegrCVGlmnet") {
        # No need to tune LASSO
        return(NA)
    } else {
        # Tune hyperparameter with 10-fold CV
        tuner <- tune(tnr("grid_search", resolution = 20, batch_size = 10), task, learner, rsmp("cv", folds = 10))
        param <- tuner$result_learner_param_vals
        return(param)
    }
}

tune_all_learners <- function(learner, tasks) {
    # Tune hyperparameter with specific task, return parameter
    tuned_params <- list()
    for (t in seq_along(tasks)) {
        tuned_params[[names(tasks)[t]]] <- tune_learner(learner, tasks[[t]])
    }
    return(tuned_params)
}

train_learner <- function(learner, task, param) {
    # Tune hyperparameter with 10-fold CV
    if (sum(!is.na(param)) > 0) {
        learner$param_set$values <- param
    }
    learner$train(task)
    return(learner)
}

train_all_learners <- function(learner, tasks, params, test_data) {
    # Train learner with specific task, return prediction
    preds <- list()
    for (t in seq_along(tasks)) {
        trained_learner <- train_learner(learner, tasks[[t]], params[[t]])
        preds[[names(tasks)[t]]] <- trained_learner$predict_newdata(test_data)$response
    }
    return(preds)
}