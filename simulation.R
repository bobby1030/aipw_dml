library(tidyverse)
library(pbapply)

load_dependencies <- function() {
    source("./generate_data.R")
    source("./estimation.R")

    library(grf)
}

simulation <- function(round) {
    data <- generate_data(1000, 3, 5, 1, c(1, 1.5, 2), c(1, 2, 3))
 
    est_ate_ipw <- ate_ipw(data, "Y", "D", c("X.1", "X.2", "X.3"))$ate
    est_ate_aipw <- ate_aipw(data, "Y", "D", c("X.1", "X.2", "X.3"))$ate
    est_ate_ols <- ate_ols(data, "Y", "D", c("X.1", "X.2", "X.3"))$ate
 
    forest <- causal_forest(data[c("X.1", "X.2", "X.3")], data$Y, data$D)
    est_ate_forest <- average_treatment_effect(forest, target.sample = "overlap")[1]
 
    list(
        ate_ipw = est_ate_ipw,
        ate_aipw = est_ate_aipw,
        ate_ols = est_ate_ols,
        ate_forest = est_ate_forest
    )

}

# Parallel clusters
cl <- parallel::makeCluster(16)

parallel::clusterExport(cl, c("load_dependencies", "simulation"))
parallel::clusterEvalQ(cl, load_dependencies())
simulation_result <- pblapply(1:100, simulation, cl = cl) %>% bind_rows()
parallel::stopCluster(cl)

saveRDS(simulation_result, "./simulation_result.rds")
