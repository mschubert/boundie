#' Bounded coefficient differential expression
#'
#' @param ...
boundie = function(formula, data=NULL, weights=NULL, bounds=NULL, control=NULL) {
    # construct x and y matrices from formula/data
    mf = model.frame(formula, data=data)
    terms = attr(mf, "terms")
    x = model.matrix(terms, mf)
    y = model.response(mf)

    # run optim for all genes
    res = fit(x, y, weights=weights, offset=offset, control=control)

    # return association data.frame
}
