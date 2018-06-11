#' Bounded coefficient differential expression
#'
#' @param x  a gene expression matrix (genes x samples)
#' @param design  the model to fit for each gene, e.g. ‘~ condition‘
#' @param weights  weight vector for all coefficients
#' @param offset  vector of coefficient offsets of length ‘n’
#' @param lower  vector of coefficient lower bounds
#' @param upper  vector of coefficient upper bounds
#' @param control  named list of control variables for fit (e.g. maxiter)
#' @export
boundie = function(x, design, weights=rep(1, ncol(x)), offset=rep(0, ncol(x)),
                   lower=-Inf, upper=Inf, control=list()) {
    fit_one = function(gene) {
        assign("y", x[gene,], envir=ee)

        mf = stats::model.frame(fml, w=w, o=o)
        terms = attr(mf, "terms")
        x = stats::model.matrix(terms, mf)
        y = stats::model.response(mf)
        w = stats::model.extract(mf, "w")
        o = stats::model.extract(mf, "o")

        fit(x, y, weights=w, offset=o, control=control, lower=lower, upper=upper)
    }

    if (!class(design) == "formula" || length(design) != 2)
        stop("‘design‘ needs to be a right-handed formula")

    ee = new.env(parent=environment(design))
    fml = stats::as.formula(paste("y ~", as.character(design)[2]), env=ee)
    assign("w", weights, envir=ee)
    assign("o", offset, envir=ee)

    res = lapply(rownames(x), function(x) try(fit_one(x)))

    success = sapply(res, function(x) class(x) != "try-error")
    if (any(!success))
        warning(sum(!success), " fits failed: ",
                paste(rownames(x)[!success], sep=", "))

    res = res[success]
    coefs = do.call(rbind, res)
    data.frame(gene = rownames(x)[success], coefs, check.names=FALSE)
}
