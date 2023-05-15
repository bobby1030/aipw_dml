library(tidyverse)

ate_ipw <- function(data, formula_pscore) {
    # estimate propensity score using LOESS
    pscore_fit <- loess(formula_pscore, data = data)$fitted
    data$pscore_fit <- pscore_fit

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
        )

    return(
        list(
            ate = ate_ipw,
            pscore_fit = pscore_fit
        )
    )
}

ate_aipw <- function(data, formula_pscore, formula_response) {
    # estimate propensity score using LOESS
    pscore_fit <- loess(formula_pscore, data = data)$fitted
    data$pscore_fit <- pscore_fit

    # estimate response function using LOESS
    response_model_treat <- loess(formula_response, data = data[data$D == 1, ])
    response_model_contr <- loess(formula_response, data = data[data$D == 0, ])

    data$response_fit_treat <- predict(response_model_treat, data)
    data$response_fit_contr <- predict(response_model_contr, data)

    # estimate ATE using AIPW
    ate_aipw <- data %>%
        drop_na(pscore_fit, response_fit_treat, response_fit_contr) %>%
        mutate(
            weight = ifelse(D == 1, 1 / pscore_fit, 1 / (1 - pscore_fit)),
            scale = (D - pscore_fit) / (pscore_fit * (1 - pscore_fit)),
            weighted_response = (1 - pscore_fit) * response_fit_treat + pscore_fit * response_fit_contr
        ) %>%
        summarize(
            ate_aipw = (sum(weight * D * Y) - sum(weight * (1 - D) * Y) - sum(scale * weighted_response)) / n()
        )

    return(
        list(
            ate = ate_aipw,
            pscore_fit = pscore_fit,
            response_fit_treat = data$response_fit_treat,
            response_fit_contr = data$response_fit_contr
        )
    )
}
