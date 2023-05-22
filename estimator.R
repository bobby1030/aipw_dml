library(tidyverse)

source("./tools.R")

# TODO: Tackle with extreme pscore_fit (use only overlapping sample)
ate_ipw <- function(data, pscore_fit) {
    data <- cbind(data, pscore_fit)

    # estimate ATE using IPW
    ate_ipw <- data %>%
        drop_na(pscore_fit) %>%
        mutate(
            weight = ifelse(D == 1, 1 / pscore_fit, 1 / (1 - pscore_fit)),
            # Normalize weight to 1
            weight = ifelse(D == 1, weight / sum(weight[D == 1]), weight / sum(weight[D == 0]))
        ) %>%
        summarize(
            ate_ipw = sum(weight * D * Y) - sum(weight * (1 - D) * Y)
        ) %>%
        unlist()

    return(ate = ate_ipw)
}

ate_aipw <- function(data, pscore_fit, response_treat_fit, response_contr_fit) {
    data <- cbind(data, pscore_fit, response_treat_fit, response_contr_fit)

    # estimate ATE using AIPW
    ate_aipw <- data %>%
        drop_na(pscore_fit, response_treat_fit, response_contr_fit) %>%
        mutate(
            tau_i = response_treat_fit - response_contr_fit,
            correction =
                D / pscore_fit * (Y - response_treat_fit)
                    - (1 - D) / (1 - pscore_fit) * (Y - response_contr_fit)
        ) %>%
        summarize(
            ate_aipw = mean(tau_i + correction)
        ) %>%
        unlist()

    return(ate_aipw)
}

ate_ols <- function(data, X) {
    # estimate ATE using OLS
    formula <- build_formula("Y", c("D", X))
    ate_ols <- coef(lm(formula, data = data))[2]

    return(ate_ols)
}

ate_plr <- function(data, pscore_fit, response_fit) {
    # estimate ATE using PLR
    Y_resid <- data$Y - response_fit
    D_resid <- data$D - pscore_fit

    ate_plr <- coef(lm(Y_resid ~ D_resid - 1))[1]

    return(ate_plr)
}
