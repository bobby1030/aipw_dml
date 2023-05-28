# Calculate empirical size of each estimator
library(tidyverse)


# Return critical value of each estimator
empirical_size <- function(x, mu_null = 0) {
    x_scaled <- (x - mu_null) / sd(x)

    # Normal approximation
    c.value <- -qnorm(0.05)

    size <- mean(abs(x_scaled) > c.value)
    df_return <- tibble(size = size)

    return(df_return)
}
