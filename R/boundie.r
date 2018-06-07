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
        y = x[gene,]
        mf = stats::model.frame(design)
        terms = attr(mf, "terms")
        x = stats::model.matrix(terms, mf)
        fit(x, y, weights=weights, offset=offset, control=control,
            lower=lower, upper=upper)
    }

    res = lapply(rownames(x), fit_one)
    coefs = do.call(rbind, res)
    data.frame(genes = rownames(x), coefs)
}
