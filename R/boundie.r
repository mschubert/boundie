#' Bounded coefficient differential expression
#'
#' @param x  a gene expression matrix (genes x samples)
#' @param design  the model to fit for each gene, e.g. '~ condition'
#' @param weights  weight vector for all coefficients
#' @param offset  vector of coefficient offsets of length 'n'
#' @param lower  named vector of coefficient lower bounds (-Inf otherwise)
#' @param upper  named vector of coefficient upper bounds (Inf otherwise)
#' @param control  named list of control variables for fit (e.g. maxiter)
#' @param reduced  the reduced model for a likelihood ratio test
#' @param verbose  print debug messages
#' @export
boundie = function(x, design, weights=rep(1, ncol(x)), offset=rep(0, ncol(x)),
                   lower=NULL, upper=NULL, control=list(), reduced=NULL,
                   verbose=FALSE) {
    fit_one = function(gene) {
        assign("y", x[gene,], envir=ee)

        mf = stats::model.frame(fml, w=w, o=o)
        terms = attr(mf, "terms")
        x = stats::model.matrix(terms, mf)
        y = stats::model.response(mf)
        w = stats::model.extract(mf, "w")
        o = stats::model.extract(mf, "o")

        valid = c(colnames(x), "theta")
        lower = lower[names(lower) %in% valid]
        upper = upper[names(upper) %in% valid]
        # log(.Machine$double.min/max.exp) == 0, hence 100
        dn = stats::setNames(c(rep(-Inf, ncol(x)), -100), valid)
        up = stats::setNames(c(rep(Inf, ncol(x)), 100), valid)
        dn[names(lower)] = unlist(lower)
        up[names(upper)] = unlist(upper)

        tfun(x, y, weights=w, offset=o, control=control,
             lower=dn, upper=up)
    }

    if (!class(design) == "formula" || length(design) != 2)
        stop("'design' needs to be a right-handed formula")

    ee = new.env(parent=environment(design))
    fml = stats::as.formula(paste("y ~", as.character(design)[2]), env=ee)
    assign("w", weights, envir=ee)
    assign("o", offset, envir=ee)

    if (is.null(reduced))
        tfun = wald
    else
        tfun = function(...) lrt(..., reduced=all.vars(reduced))

    res = lapply(rownames(x), function(x) try(fit_one(x)))

    success = sapply(res, function(x) class(x) != "try-error")
    if (any(!success))
        warning(sum(!success), " fits failed: ",
                paste(rownames(x)[!success], collapse=", "))

    res = res[success]
    coefs = do.call(rbind, res)
    data.frame(gene = rownames(x)[success], coefs, check.names=FALSE)
}
