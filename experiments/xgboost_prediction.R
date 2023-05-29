library(mlr3)
library(mlr3learners)
library(mlr3tuning)
library(tidyverse)

source("./generate_data.R")
source("./estimator.R")

data <- generate_data()

task_pscore <- as_task_regr(data, "D")$select(paste("X", 1:10, sep = "."))
task_response <- as_task_regr(data, "Y")$select(paste("X", 1:10, sep = "."))

learner <- lrn(
    "regr.xgboost",
    nrounds = to_tune(1, 8000),
    max_depth = 2,
    eta = 0.01,
    subsample = 0.5,
    
)

future::plan(list("multisession"))

# Tune parameter
learner_tuned <- auto_tuner(tnr("grid_search", resolution = 50, batch_size = 10), learner, rsmp("cv", folds = 10))
pscore_hat <- as.data.table(learner_tuned$train(task_pscore)$predict(task_pscore))[, response]
response_hat <- as.data.table(learner_tuned$train(task_response)$predict(task_response))[, response]

ate_plr(data, "Y", "D", pscore_hat, response_hat)
ate_ipw(data, pscore_hat)
ate_ols(data, "Y", "D", c("X.1", "X.2", "X.3"))

ate_aipw(data, "Y", "D", pscore_hat, response_hat)

# Benchmark
data <- cbind(data, pscore_hat)

ggplot(data) +
    geom_density(aes(x = pscore, color = factor(D))) +
    geom_density(aes(x = pscore_hat, color = factor(D)), linetype = "dashed")

ggplot(data) +
    geom_density(aes(x = Y)) +
    geom_density(aes(x = response_hat), linetype = "dashed")


ggplot(data) +
    geom_point(aes(x = pscore, y = pscore_hat, color = factor(D))) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    xlim(0, 1) +
    ylim(0, 1)
