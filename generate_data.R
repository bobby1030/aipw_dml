generate_data <- function(n = 2000, tau = 5, seed = NULL) {
    # n: number of observations
    # p: number of covariates
    # tau: treatment effect
    # sigma: variance of normal error
    p <- 10
    sigma <- 1

    # beta_pscore: p * 1 coefficients on treatment status
    # beta_response: p * 1 coefficients on outcome response

    if (!is.null(seed)) {
        set.seed(seed)
    }

    # generate covariates
    X <- matrix(rnorm(n * p), nrow = n, ncol = p) # n * p matrix

    # generate propensity score (normal cdf)
    pscore <- pnorm(X[, 1] + X[, 3] + X[, 5] + X[, 1] * X[, 3])

    # generate treatment indicator
    D <- rbinom(n, 1, pscore)

    # generate realized outcome
    Y <- tau * D + X[, 1] * X[, 2] + sin(pi * X[, 3] * X[, 4]) + 0.5 * exp(X[, 5]) + rnorm(n, 0, sigma)

    # construct data frame
    data <- data.frame(
        Y = Y, # Outcome
        D = D, # Treatment indicator
        X = X, # Convariates
        pscore = pscore # True propensity score
    )

    return(data)
}
