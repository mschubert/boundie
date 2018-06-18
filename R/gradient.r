#' Gradient function for negative binomial regression
#'
#' The following variables need to be available to the function in order to
#' be able to call `optim` using this as a gradient function:
#'
#' x - Regression coefficients matrix
#' beta - Regressors, such that beta * x = y
#' y - Observed count values
#'
#' @param par  numeric vector of coefficients in ’x’ and theta
#' @param .x  design matrix of dimension ‘n * p’
#' @param .y  vector of observations of length ‘n’
#' @param weights  vector of weights of length ‘n’
#' @param offset  vector of coefficient offsets of length ‘n’
#' @return  direction of steepest descent (the gradient)
gradient = function(par, .x, .y, weights, offset) {
    beta = par[-length(par)]
    theta = exp(par[length(par)])
    mu = .x %*% beta + offset

    penalty = stats::pmin(mu, 0)
    mu = stats::pmax(mu, 0)

    grc = drop(.y - mu * (.y + theta)/(mu + theta))
    grt = digamma(.y + theta) - digamma(theta) +
        log(theta) + 1 - log(mu + theta) - (.y + theta)/(mu + theta)
    -colSums(weights * cbind(grc * .x + penalty, grt * theta))
}
