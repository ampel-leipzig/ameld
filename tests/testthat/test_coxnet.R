test_that("basehaz", {
    # taken from glmnet::cv.glmnet
    set.seed(10101)
    n <- 500
    p <- 30
    nzc <- trunc(p / 10)
    x <- matrix(rnorm(n * p), n, p)
    beta <- rnorm(nzc)
    fx <- x[, seq(nzc)] %*% beta / 3
    hx <- exp(fx)
    ty <- rexp(n, hx)
    tcens <- rbinom(n = n, prob = 0.3, size = 1)  # censoring indicator
    y <- Surv(ty, 1 - tcens)

    cp <- coxph(y ~ x)
    cn <- cv.glmnet(x = x, y = y, family = "cox", folds = 3)

    expect_snapshot(basehaz(cp))
    expect_snapshot(basehaz(cp, centered = TRUE))
    expect_snapshot(basehaz(cn, x = x, y = y))
    expect_snapshot(basehaz(cn, x = x, y = y, centered = TRUE))
    expect_snapshot(basehaz(cn, x = x, y = y, times = c(3, 5)))
})
