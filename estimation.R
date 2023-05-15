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
