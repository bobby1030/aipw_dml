library(tidyverse)

simulation_result <- readRDS("./simulation_result_lasso.rds")

simulation_result %>%
    pivot_longer(everything(), names_to = "estimator", values_to = "ate") %>%
    ggplot(aes(x = ate, color = estimator)) +
    geom_density() +
    geom_point(
        aes(y = 0),
        data = . %>%
            group_by(estimator) %>%
            summarize(ate = mean(ate)),
    ) +
    geom_vline(xintercept = 5, linetype = "dashed") +
    theme_minimal()


plot(density(data$pscore[data$D == 0]), xlim = c(0, 1))
lines(density(data$pscore[data$D == 1]), xlim = c(0, 1))

lines(density(forest$W.hat[data$D == 0]), xlim = c(0, 1), col = "red")
lines(density(forest$W.hat[data$D == 1]), xlim = c(0, 1), col = "red")

plot(density(nuisance$pscore_fit_lasso[data$D == 0]), xlim = c(0, 1))
lines(density(nuisance$pscore_fit_lasso[data$D == 1]), xlim = c(0, 1))

plot(density(nui$pscore_fit_rf[data$D == 0]), xlim = c(0, 1))
lines(density(nui$pscore_fit_rf[data$D == 1]), xlim = c(0, 1))
