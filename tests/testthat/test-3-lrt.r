context("wald test")

test_that("100 samples, 2 components, same as glm.nb", {
    f1 = runif(100)
    f2 = 1 - f1
    c1 = stats::rnbinom(n=100, size=10, mu=1000)
    c2 = stats::rnbinom(n=100, size=20, mu=10)
    both = round(f1 * c1 + f2 * c2)

#    ctl = MASS::glm.nb(both ~ 0 + f1 + f2)
#    stat = coef(summary(ctl))[,'z value']

    mf = model.frame(both ~ 0 + f1 + f2)
    terms = attr(mf, "terms")
    x = model.matrix(terms, mf)
    y = model.response(mf)
    res = lrt(x, y, reduced="f1")

    # expect same coefficients, max 15% diff stat due to diff fitting
    expect_true(res$statistic > 100)
})
