#' Fit parameters using the optim function
#'
#' @param x  design matrix of dimension ‘n * p’
#' @param y  vector of observations of length ‘n’
#' @param weights  vector of weights of length ‘n’
#' @param offset  vector of coefficient offsets of length ‘n’
#' @param control  list of control paramters, e.g. ‘maxit’
#' @param lower  vector of coefficient lower bounds
#' @param upper  vector of coefficient upper bounds
#' @return  named list with estimated coefficients
fit = function(x, y, weights=rep(1,nrow(x)), offset=0, control=list(),
               lower=-Inf, upper=Inf) {
    mod = suppressWarnings(
        stats::glm.fit(x, y, weights=weights, family=stats::poisson(),
                       offset=offset, control=control))
    start = c(mod$coefficients, theta=0)
    start[is.na(start)] = 0
    #todo: if start outside bounds, set it at bounds

    # @return  list with the following components:
    #   ‘par’ - best values for paramters found
    #   ‘value’ - negative log-likelihood at end of iterations
    #   ‘convergence’ - 0 if converged, other values encode condition
    #   ‘message’ - character string with additional information
    res = stats::optim(par=start, fn=nll, gr=gradient, method="L-BFGS-B",
        control=control, .x=x, .y=y, weights=weights, offset=offset,
        lower=lower, upper=upper, hessian=TRUE)

    # we are minimizing the negative log likelihood, so the covariance matrix
    # is the hessian; diagonalizing and taking those elements, we get the
    # variance (which is the standard error squared)
    beta_se = sqrt(pmax(diag(solve(res$hessian)), 0))
    wald = res$par / beta_se
    pval = 2 * pnorm(abs(wald), lower.tail=FALSE)

    #todo: assemble results in list and return that
    # would also be nice if wald statistic, p-value
    data.frame(term=names(res$par), estimate=res$par, statistic=wald,
               p.value=pval, check.names=FALSE, row.names=NULL)
}
