library(mlr3)
library(mlr3learners)
library(mlr3tuning)
library(tidyverse)

source("./generate_data.R")

data <- generate_data()

task_pscore <- as_task_regr(data, "D")$select(paste("X", 1:10, sep = "."))

lrn_lasso <- lrn("regr.cv_glmnet", nfolds = 10)
lrn_rf <- lrn(
    "regr.ranger",
    num.trees = to_tune(2000, 4000),
    min.node.size = 5,
    sample.fraction = 0.5,
    mtry.ratio = to_tune(seq(0.2, 1, 0.2)),
    num.threads = 4
)
lrn_xgboost <- lrn(
    "regr.xgboost",
    nrounds = to_tune(1, 2000),
    max_depth = 2,
    eta = 0.01,
    subsample = 0.5
)

future::plan("multisession", workers = 16)

param_rf <- tune(tnr("grid_search", resolution = 10, batch_size = 10), task_pscore, lrn_rf, rsmp("cv", folds = 10))$result_learner_param_vals
param_xgboost <- tune(tnr("grid_search", resolution = 20, batch_size = 10), task_pscore, lrn_xgboost, rsmp("cv", folds = 10))$result_learner_param_vals

lrn_rf$param_set$values <- param_rf
lrn_xgboost$param_set$values <- param_xgboost

pscore_hat <- list(
    lasso = lrn_lasso$train(task_pscore)$predict(task_pscore),
    rf = lrn_rf$train(task_pscore)$predict(task_pscore),
    xgboost = lrn_xgboost$train(task_pscore)$predict(task_pscore)
)

