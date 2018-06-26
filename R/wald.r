#' Wald test for model fits
#'
#' @inheritParams  fit
#' @return  a data.frame with terms, their fitted values, and wald statistic
wald = function(x, y, weights=rep(1,nrow(x)), offset=0, control=list(),
                lower=-Inf, upper=Inf, verbose=FALSE) {
    res = do.call(fit, c(as.list(match.call())[-1],
                  list(hessian=TRUE, verbose=verbose)),
                  envir=parent.frame())

    # we are minimizing the negative log likelihood, so the covariance matrix
    # is the hessian; diagonalizing and taking those elements, we get the
    # variance (which is the standard error squared)
    beta_se = tryCatch(sqrt(pmax(diag(solve(res$hessian)), 0)),
                       error = function(e) NA)
    wald = res$par / beta_se
    pval = 2 * stats::pnorm(abs(wald), lower.tail=FALSE)
    # pval = 2 * stats::pt(abs(wald), df=df, lower.tail=FALSE)

    # assemble results
    data.frame(term=names(res$par), estimate=res$par, statistic=wald,
               p.value=pval, check.names=FALSE, row.names=NULL)
}
