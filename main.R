source("./simulation.R")

load_dependencies()

future::plan(list("multisession", "multisession"), workers = 40)

# Simulate tau = 5
for (spec in c("both", "pscore", "resp")) {
    for (lrn_type in c("lasso", "rf", "xgboost")) {
        simulation_result <- with_progress(simulation_setup(2000, 5, lrn_type, spec))

        output_dir <- "./results/tau5/"
        if (!dir.exists(output_dir)) dir.create(output_dir, recursive = T)

        saveRDS(simulation_result, sprintf(paste0(output_dir, "sim_result_%s_%s.rds"), lrn_type, spec))
    }
}