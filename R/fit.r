#' Fit parameters using the optim function
#'
#' @param x  design matrix of dimension ‘n * p’
#' @param y  vector of observations of length ‘n’
#' @param weights  vector of weights of length ‘n’
#' @param offset  vector of coefficient offsets of length ‘n’
#' @param control  list of control paramters, e.g. ‘maxit’
#' @param lower  vector of coefficient lower bounds
#' @param upper  vector of coefficient upper bounds
#' @param hessian  whether to return the hessian at the optimized nll
#' @return  named list with estimated coefficients
fit = function(x, y, weights=rep(1,nrow(x)), offset=0, control=list(),
               lower=-Inf, upper=Inf, hessian=FALSE) {
    mod = suppressWarnings(
        stats::glm.fit(x, y, weights=weights, offset=offset,
                       family=stats::poisson(), control=control))
    start = c(exp(mod$coefficients), theta=1)
    start[is.na(start)] = 0
    start = pmin(start, upper)
    start = pmax(start, lower)

    # @return  list with the following components:
    #   ‘par’ - best values for paramters found
    #   ‘value’ - negative log-likelihood at end of iterations
    #   ‘convergence’ - 0 if converged, other values encode condition
    #   ‘message’ - character string with additional information

    #TODO: only add 'lower', 'upper' to ui if limits set
    # also, BFGS-B boundary (maybe?) needs to be less stringent than outer
    # not passing bounds to optim leads to 0 in nll -> error w/ log
    ui = cbind(rbind(x, diag(ncol(x))), 0)
    ci = rep(1e-8, length(y)+ncol(x))
    stats::constrOptim(theta=start, f=nll, grad=gradient, method="L-BFGS-B",
        control=control, .x=x, .y=y, weights=weights, offset=offset,
        ui=ui, ci=ci, lower=lower, upper=upper, hessian=hessian)
}
