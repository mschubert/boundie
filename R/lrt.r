#' Likelihood ratio test for model fits
#'
#' @inheritParams  fit
#' @param  red_params  character vector of parameters used in the reduced model
#' @return  a data.frame with terms, their fitted values, and LRT result
lrt = function(x, y, weights=rep(1,nrow(x)), offset=0, control=list(),
                lower=-Inf, upper=Inf) {
    full = do.call(fit, as.list(match.call())[-1], envir=parent.frame())
    x = x[,reduced]
    red = do.call(fit, as.list(match.call())[-1], envir=parent.frame())

    df = length(y) - ncol(x)
    stat = 2 * (full$value - red$value)
    pval = pchisq(stat, df=df, lower.tail=FALSE)

    # only report the term difference between full and reduced model
    data.frame(...)
}
