#' Negative log likelihood function
#'
#' @param par
#' @return
nll = function(par, weights, offset) {
    beta = par[-length(par)]
    theta = exp(par[length(par)])
    mu = exp(x %*% beta + offset)
    -sum(weights * stats::dnbinom(y, mu=mu, size=theta, log=TRUE))
}
