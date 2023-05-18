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
            weight = ifelse(D == 1, 1 / pscore_fit, 1 / (1 - pscore_fit)),
            scale = (D - pscore_fit) / (pscore_fit * (1 - pscore_fit)),
            weighted_response = (1 - pscore_fit) * response_treat_fit + pscore_fit * response_contr_fit
        ) %>%
        summarize(
            ate_aipw = (sum(weight * D * Y) - sum(weight * (1 - D) * Y) - sum(scale * weighted_response)) / n()
        ) %>% 
        unlist()

    return(ate_aipw)
}

ate_ols <- function(data, Y, D, X) {
    # estimate ATE using OLS
    formula <- build_formula(Y, c(D, X))
    ate_ols <- coef(lm(formula, data = data))[2]

    return(ate_ols)
}