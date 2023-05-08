generate_data <- function(n, p, tau, sigma, beta_pscore, beta_response, seed) {
    # n: sample size
    # p: number of covariates
    # tau: treatment effect
    # sigma: variance of normal error

    # TODO: need to accept formula to deal with high order case
    # beta_pscore: p * 1 coefficients on treatment status
    # beta_response: p * 1 coefficients on outcome response

    set.seed(seed)

    # generate covariates
    X <- matrix(rnorm(n * p), nrow = n, ncol = p) # n * p matrix

    # generate propensity score (normal cdf)
    pscore <- pnorm(X %*% beta_pscore)

    # generate treatment indicator
    D <- rbinom(n, 1, pscore)

    # generate realized outcome
    Y <- tau * D + X %*% beta_response + 5 * X[,1] * X[,2] + rnorm(n, 0, sigma)

    # construct data frame
    data <- data.frame(
        Y = Y, # Outcome
        D = D, # Treatment indicator
        X = X, # Convariates
        pscore = pscore # True propensity score
    )

    return(data)
}

