#' Negative log likelihood function
#'
#' @param par  numeric vector of coefficients in ’x’ and theta
#' @param .x  design matrix of dimension ‘n * p’
#' @param .y  vector of observations of length ‘n’
#' @param weights  vector of weights of length ‘n’
#' @param offset  vector of coefficient offsets of length ‘n’
#' @return  negative log-likelihood of a set of paramters given the data
nll = function(par, .x, .y, weights, offset) {
    beta = par[-length(par)]
    theta = exp(par[length(par)])
    mu = .x %*% beta + offset
    -sum(weights * stats::dnbinom(.y, mu=mu, size=theta, log=TRUE))
}
