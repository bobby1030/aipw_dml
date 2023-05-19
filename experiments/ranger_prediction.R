library(mlr3)
library(mlr3learners)
library(tidyverse)

source("./generate_data.R")
source("./estimation.R")

extract_cv_fitted_value <- function(rsmp_result) {
    prediction <- as.data.table(rsmp_result$prediction())
    prediction <- prediction[order(row_ids), response]
    return(prediction)
}

data <- generate_data(1000, 3, 5, 1, c(0.3, 0, 0), c(0.5, 1.5, 3))
fold <- rep(1:5, length.out = nrow(data))
data <- cbind(data, fold)

ate <- numeric(5)

# Crossfitting
for (k in 1:5) {
    train <- data[data$fold != k, ]
    test <- data[data$fold == k, ]

    task_pscore <- as_task_regr(train, "D")$select(c("X.1", "X.2", "X.3"))
    task_resp_1 <- as_task_regr(train[train$D == 1, ], "Y")$select(c("X.1", "X.2", "X.3"))
    task_resp_0 <- as_task_regr(train[train$D == 0, ], "Y")$select(c("X.1", "X.2", "X.3"))

    learner <- lrn(
        "regr.ranger",
        num.trees = 2000,
        min.node.size = 5,
        sample.fraction = 0.5,
        mtry = 3,
        num.threads = 8
    )

    pscore_hat <- learner$train(task_pscore)
    test$pscore_hat <- predict(pscore_hat, test)

    resp_1_hat <- learner$train(task_resp_1)
    test$resp_1_hat <- predict(resp_1_hat, test)

    resp_0_hat <- learner$train(task_resp_0)
    test$resp_0_hat <- predict(resp_0_hat, test)

    ate[k] <- ate_aipw(test, test$pscore_hat, test$resp_1_hat, test$resp_0_hat)
}

data_bind <- cbind(data, pscore_hat)

ggplot(data_bind, aes(color = factor(D))) +
    geom_density(aes(x = pscore)) +
    geom_density(aes(x = pscore_hat), linetype = "dashed") +
    xlim(0, 1) +
    theme_minimal()

ggplot(data_bind) +
    geom_point(aes(x = pscore, y = pscore_hat, color = factor(D)))

ate_aipw(data, pscore_hat, resp_1_hat, resp_0_hat)
