test_that("arcv.glmnet", {
    # taken from glmnet::cv.glmnet
    set.seed(1010)
    n <- 1000
    p <- 100
    nzc <- trunc(p/10)
    x <- matrix(rnorm(n * p), n, p)
    beta <- rnorm(nzc)
    fx <- x[, seq(nzc)] %*% beta
    eps <- rnorm(n) * 5
    y <- drop(fx + eps)
    cv <- list(
        {set.seed(1011); rcv.glmnet(x, y, alpha = 0, nrepcv = 2, nfolds = 3)},
        {set.seed(1011); rcv.glmnet(x, y, alpha = 1, nrepcv = 2, nfolds = 3)}
    )
    future::plan(sequential)
    set.seed(1011)
    arcv <- arcv.glmnet(
        x, y, alpha = c(0, 1), nrepcv = 2, nfolds = 3, trace.it = FALSE
    )

    ## calls are obviously different
    cv[[1]]$call <- cv[[2]]$call <- NULL
    cv[[1]]$glmnet.fit$call <- cv[[2]]$glmnet.fit$call <- NULL
    arcv.wo.call <- arcv
    arcv.wo.call$models[[1]]$call <- arcv.wo.call$models[[2]]$call <- NULL
    arcv.wo.call$models[[1]]$glmnet.fit$call <-
        arcv.wo.call$models[[2]]$glmnet.fit$call <- NULL
    expect_equal(arcv.wo.call$models, cv)

    expect_snapshot(print(arcv))

#' # each base::graphics plot function must be wrapped by an anonymous function
#' # that could be called by `vdiffr::expect_doppelganger()`
#' # use `testthat::snapshot_review()` to add new/verify changed plots
#' testthat::snapshot_review()
    vdiffr::expect_doppelganger(
        "arcv.glmnet-all-plot", function()plot(arcv)
    )
    vdiffr::expect_doppelganger(
        "arcv.glmnet-lambdamin-plot", function()plot(arcv, what = "lambda.min")
    )

    future::plan(tweak(multicore, workers = 2L))
    set.seed(1011)
    arcv.mc <- arcv.glmnet(
        x, y, alpha = c(0, 1), nrepcv = 2, nfolds = 3, trace.it = FALSE
    )
    future::plan(sequential)

    expect_equal(arcv, arcv.mc)
})
