library(tidyverse)

simulation_result <- tibble()

# Read results 
for (spec in c("both", "pscore", "resp", "none")) {
    for (lrn_type in c("lasso", "rf", "xgboost")) {
        result <- readRDS(sprintf("./results/sim_result_%s_%s.rds", lrn_type, spec))
        result$lrn_type <- lrn_type
        result$spec <- spec
        simulation_result <- bind_rows(simulation_result, result)
    }
}

sim_result_plot <- simulation_result %>%
    pivot_longer(starts_with("ate"), names_to = "estimator", values_to = "ate") %>%
    ggplot(aes(x = ate, color = estimator)) +
        facet_grid(vars(spec), vars(lrn_type)) +
        geom_line(stat = "density", alpha = 0.7) +
        geom_point(
            aes(y = 0),
            data = . %>%
                group_by(estimator) %>%
                summarize(ate = mean(ate)),
        ) +
        geom_vline(xintercept = 5, linetype = "dashed") +
        xlim(0, 10) +
        theme_minimal()

ggsave("./results/sim_result_plot.pdf", sim_result_plot, width = 10, height = 6, dpi = 300)
