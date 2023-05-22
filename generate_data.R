generate_data <- function(n = 2000, tau = 5) {
    # n: number of observations
    # p: number of covariates
    # tau: treatment effect
    # sigma: variance of normal error
    p <- 10

    # generate covariates
    X <- matrix(rnorm(n * p), nrow = n, ncol = p) # n * p matrix

    # generate propensity score (normal cdf)
    pscore <- pnorm(X[, 1] + X[, 3] + X[, 5] + X[, 1] * X[, 3])

    # generate treatment indicator
    D <- rbinom(n, 1, pscore)

    # generate realized outcome
    Y <- tau * D + X[, 1] * X[, 2] + 4 * sin(pi * X[, 3] * X[, 4]) + exp(X[, 5]) + rnorm(n)

    # construct data frame
    data <- data.frame(
        Y = Y, # Outcome
        D = D, # Treatment indicator
        X = X, # Convariates
        pscore = pscore # True propensity score
    )

    return(data)
}

generate_sim_samples <- function(S = 1000, n = 2000, tau = 5, seed = NULL) {
    # S: number of simulations
    
    if (!is.null(seed)) {
        set.seed(seed)
    }

    sim_samples <- lapply(1:S, function(x) generate_data(n, tau))

    return(sim_samples)
}