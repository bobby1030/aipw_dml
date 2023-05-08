library(tidyverse)

ate_ipw <- function(data, formula_pscore) {

    # estimate propensity score using LOESS
    pscore_fit <- loess(formula_pscore, data = data)$fitted
    data$pscore_fit <- pscore_fit

    # estimate ATE using IPW
    ate_ipw <- data %>%
        mutate(
            weight = ifelse(D == 1, 1 / pscore_fit, 1 / (1 - pscore_fit))
        ) %>%
        summarize(
            ate_ipw = sum(weight * D * Y) / sum(weight * D) - sum(weight * (1 - D) * Y) / sum(weight * (1 - D))
        )

    return(
        list(
            ate = ate_ipw,
            pscore_fit = pscore_fit
        )
    )
}
