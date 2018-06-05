context("resolve mixture")

test_that("100 samples, 2 components", {
    f1 = runif(100)
    f2 = 1 - f1
    c1 = stats::rnbinom(n=100, size=10, mu=1000)
    c2 = stats::rnbinom(n=100, size=20, mu=10)
    both = round(f1 * c1 + f2 * c2)

    ctl = MASS::glm.nb(both ~ 0 + f1 + f2) # exp(estimate) is if frac->1
    mod = fit()
    # sample fractions
    # sample from c1, c2
    # add them together in weighted fashion
})

# maybe also do constrained with fake negative component
