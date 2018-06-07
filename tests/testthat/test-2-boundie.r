context("boundie matrix")

test_that("fit using expression matrix", {
    f1 = runif(100)
    f2 = 1 - f1
    c1 = stats::rnbinom(n=100, size=10, mu=1000)
    c2 = stats::rnbinom(n=100, size=20, mu=10)
    both = round(f1 * c1 + f2 * c2)

    ctl = MASS::glm.nb(both ~ 0 + f1 + f2)

    both = matrix(both, nrow=1, dimnames=list("gene",NULL))
    res = boundie(both, ~ 0 + f1 + f2)

    expect_true(all(c(res$f1, res$f2) - coef(ctl) < 1e-4))
})
