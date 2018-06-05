#' Fit parameters using the optim function
#'
#' @param x  design matrix of dimension ‘n * p’
#' @param y  vector of observations of length ‘n’
#' @param weights  vector of weights of length ‘n’
#' @param offset  vector of coefficient offsets of length ‘n’
#' @param control  list of control paramters, e.g. ‘maxit’
#' @return  list with the following components:
#'   ‘par’ - best values for paramters found
#'   ‘value’ - negative log-likelihood at end of iterations
#'   ‘convergence’ - 0 if converged, other values indicate iteration limit or warning
#'   ‘message’ - character string with additional information
fit = function(x, y, weights=rep(1,ncol(x)), offset=0, control=list()) {
    mod = stats::glm.fit(x, y, weights=weights, family=poisson(),
                         offset=offset, control=control)
    start = c(mod$coefficients, theta=0)
    #todo: if start outside bounds, set it at bounds
    # needs globla params: w, offset,
    w = weights
    stats::optim(par=start, fn=nll, gr=gradient, method="L-BFGS-B", control=control)
}
