#' Negative log likelihood function
#'
#' We're maximizing the likelihood function, so minimizing the negative log
#' likelihood (which is the default in optim)
#'
#' @param par  numeric vector of coefficients in ’x’ and theta
#' @param .x  design matrix of dimension ‘n * p’
#' @param .y  vector of observations of length ‘n’
#' @param weights  vector of weights of length ‘n’
#' @param offset  vector of coefficient offsets of length ‘n’
#' @param ...  ignored
#' @return  negative log-likelihood of a set of paramters given the data
nll = function(par, .x, .y, weights, offset, ...) {
    beta = par[-length(par)]
    theta = exp(par[length(par)])
    mu = .x %*% beta + offset

    nb = suppressWarnings(stats::dnbinom(.y, mu=mu, size=theta, log=TRUE))
    nb[is.infinite(nb)] = -exp(1 + mu - .y)[is.infinite(nb)]
    nb[is.na(nb)] = -exp(1 + mu)[is.na(nb)]
    -sum(weights * nb)
}
