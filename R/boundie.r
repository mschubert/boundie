#' Bounded coefficient differential expression
#'
#' @param x  a gene expression matrix (genes x samples)
#' @param design  the model to fit for each gene, e.g. ‘~ condition‘
#' @param weights  weight vector for all coefficients
#' @param lower  named vector of coefficient lower bounds
#' @param upper  named vector of coefficient upper bounds
#' @param control  named list of control variables for fit (e.g. maxiter)
boundie = function(x, design, weights=NULL, lower=-Inf, upper=Inf, control=NULL) {
    fit_one = function(gene) {
        y = x[gene,]
        mf = model.frame(design)
        terms = attr(mf, "terms")
        x = model.matrix(terms, mf)
        y = model.response(mf)
        res = cbind(gene = gene,
            fit(x, y, weights=weights, offset=offset, control=control,
                lower=lower, upper=upper))
    }

    res = lapply(rownames(x), fit_one)
    do.call(rbind, res)
}
