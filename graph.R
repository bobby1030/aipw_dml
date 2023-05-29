library(tidyverse)
library(ggridges)

simulation_result <- tibble()

# Read results 
for (spec in c("both", "pscore", "resp")) {
    for (lrn_type in c("lasso", "rf", "xgboost")) {
        result <- readRDS(sprintf("./results/tau0/sim_result_%s_%s.rds", lrn_type, spec))
        result$lrn_type <- lrn_type
        result$spec <- spec
        simulation_result <- bind_rows(simulation_result, result)
    }
}

simulation_result %>%
    pivot_longer(starts_with("ate"), names_to = "estimator", values_to = "ate") %>%
    ggplot(aes(x = ate, y = estimator)) +
        facet_grid(vars(spec), vars(lrn_type)) +
        geom_density_ridges(alpha = 0.7) +
        geom_vline(xintercept = 0, linetype = "dashed") +
        xlim(-5, 5) +
        theme_minimal()