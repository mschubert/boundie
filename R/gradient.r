#' Gradient function for negative binomial regression
#'
#' The following variables need to be available to the function in order to
#' be able to call `optim` using this as a gradient function:
#'
#' x - Regression coefficients matrix
#' beta - Regressors, such that beta * x = y
#' y - Observed count values
#'
#' @param par  Numeric vector of coefficient values
#' @return  Direction of steepest descent (the gradient)
gradient = function(par, weights=1, offset=0) {
    beta = par[-length(par)]
    theta = exp(par[length(par)])
    mu = exp(x %*% beta + offset)
    gr = drop(y - mu * (y + theta)/(mu + theta))
    colSums(-w * cbind(gr * x, theta * (digamma(y + theta) - digamma(theta) +
            log(theta) + 1 - log(mu + theta) - (y + theta)/(mu + theta))))
}
