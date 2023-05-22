source("./simulation.R")

load_dependencies()

future::plan(list("multisession", "multisession"), workers = 40)

for (spec in c("both", "pscore", "resp", "none")) {
    for (lrn_type in c("lasso", "rf", "xgboost")) {
        simulation_result <- with_progress(simulation_setup(100, lrn_type, spec))
        saveRDS(simulation_result, sprintf("./results/sim_result_%s_%s.rds", lrn_type, spec))
    }
}