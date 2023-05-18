library(tidyverse)

simulation_result <- readRDS("./simulation_result.rds")

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
