#' Likelihood ratio test for model fits
#'
#' @inheritParams  fit
#' @param  reduced  character vector of parameters used in the reduced model
#' @return  a data.frame with terms, their fitted values, and LRT result
lrt = function(x, y, reduced, weights=rep(1,nrow(x)), offset=0, control=list(),
                lower=-Inf, upper=Inf) {
    full = fit(x, y, weights, offset, control, lower, upper)
    red = fit(x[,reduced, drop=FALSE], y, weights, offset, control, lower, upper)

    df = length(y) - ncol(x)
    stat = 2 * (red$value - full$value)
    pval = stats::pchisq(stat, df=df, lower.tail=FALSE)

    # only report the term difference between full and reduced model
    data.frame(statistic=stat, p.value=pval)
}
