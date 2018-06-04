#' Negative log likelihood function
#'
#' @param par
#' @return
nll = function(par) {
    beta = par[-length(par)]
    theta = exp(par[length(par)])
    mu = exp(x %*% beta + offset)
    -sum(w * stats::dnbinom(y, mu=mu, size=theta, log=TRUE))
}
