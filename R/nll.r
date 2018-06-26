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
#' @param verbose  print debug messages
#' @param ...  ignored
#' @return  negative log-likelihood of a set of paramters given the data
nll = function(par, .x, .y, weights=rep(1, nrow(.x)), offset=rep(0, nrow(.x)),
               .verbose=FALSE) {
    beta = par[-length(par)]
    theta = exp(par[length(par)])
    mu = .x %*% beta + offset

    if (.verbose)
        message(paste(sprintf("%.2f", par), collapse=", "))

    nb = suppressWarnings(stats::dnbinom(.y, mu=mu, size=theta, log=TRUE))
    nb[is.infinite(nb)] = sign(nb[is.infinite(nb)]) * 1000
    nb[is.na(nb)] = -mu[is.na(nb)] # penalize mu < 0
    -sum(weights * nb)
}
