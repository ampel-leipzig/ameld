test_that("bootstrap", {
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
    y <- Surv(time = ty, event = 1 - tcens)

    b <- bootstrap(
        x, y, family = "cox",
        nboot = 2, nrepcv = 2, nfolds = 3, times = 2,
        trace.it = FALSE
    )
    expect_snapshot(print(b))

#' # each base::graphics plot function must be wrapped by an anonymous function
#' # that could be called by `vdiffr::expect_doppelganger()`
#' # use `testthat::snapshot_review()` to add new/verify changed plots
#' testthat::snapshot_review()
    vdiffr::expect_doppelganger(
        "boot.glmnet-plot-cal", function()plot(b, what = "calibration")
    )
    vdiffr::expect_doppelganger(
        "boot.glmnet-plot-sel", function()plot(b, what = "selected")
    )
})
