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
#' @param verbose  print debug messages
#' @return  named list with estimated coefficients
fit = function(x, y, weights=rep(1,nrow(x)), offset=0, control=list(),
               lower=-Inf, upper=Inf, hessian=FALSE, verbose=FALSE) {
    mod = tryCatch(error = function(e) list(converged=FALSE),
        suppressWarnings(stats::glm.fit(x, y, weights=weights, offset=offset,
                       family=stats::poisson(link=identity), control=control)))
    within_bounds = all(mod$coefficients >= lower[-length(lower)] &
                        mod$coefficients <= upper[-length(upper)])
    if (mod$converged && within_bounds)
        start = c(mod$coefficients, theta=1)
    else
        start = c(setNames(rep(1, ncol(x)), colnames(x)), theta=1)
#    start = pmin(start, upper)
#    start = pmax(start, lower)

    # @return  list with the following components:
    #   ‘par’ - best values for paramters found
    #   ‘value’ - negative log-likelihood at end of iterations
    #   ‘convergence’ - 0 if converged, other values encode condition
    #   ‘message’ - character string with additional information
    stats::optim(par=start, fn=nll, gr=gradient, method="L-BFGS-B",
        control=control, .x=x, .y=y, weights=weights, offset=offset,
        lower=lower, upper=upper, hessian=hessian, .verbose=verbose)
}
