context("boundie matrix")

test_that("fit using expression matrix", {
    f1 = runif(100)
    f2 = 1 - f1
    c1 = stats::rnbinom(n=100, size=10, mu=1000)
    c2 = stats::rnbinom(n=100, size=20, mu=10)
    mix1 = round(f1 * c1 + f2 * c2)
    mix2 = round(f2 * c1 + f1 * c2)

    ctl1 = MASS::glm.nb(mix1 ~ 0 + f1 + f2)
    ctl2 = MASS::glm.nb(mix2 ~ 0 + f1 + f2)

    both = rbind(mix1, mix2)
    rownames(both) = c("mix1", "mix2")
    res = boundie(both, ~ 0 + f1 + f2)

    expect_true(all(c(res[1,'f1'], res[1,'f2']) - coef(ctl1) < 1e-4))
    expect_true(all(c(res[2,'f1'], res[2,'f2']) - coef(ctl2) < 1e-4))
})
