context("wald test")

test_that("100 samples, 2 components, same as glm.nb", {
    f1 = runif(100)
    f2 = 1 - f1
    c1 = stats::rnbinom(n=100, size=10, mu=1000)
    c2 = stats::rnbinom(n=100, size=20, mu=10)
    both = round(f1 * c1 + f2 * c2)

    ctl = MASS::glm.nb(both ~ 0 + f1 + f2)
    stat = coef(summary(ctl))[,'z value']

    mf = model.frame(both ~ 0 + f1 + f2)
    terms = attr(mf, "terms")
    x = model.matrix(terms, mf)
    y = model.response(mf)
    res = wald(x, y)

    # expect same coefficients, max 15% diff stat due to diff fitting
    expect_true(all(res$estimate[1:2] - coef(ctl) < 1e-4))
    expect_true(abs(log2(res$statistic[1] / stat[1])) < log2(1.15))
    expect_true(abs(log2(res$statistic[2] / stat[2])) < log2(1.15))
})
