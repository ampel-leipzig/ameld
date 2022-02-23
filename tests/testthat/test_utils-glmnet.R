test_that(".bfolds", {
    expect_error(.bfolds(1:3, nfolds = 2), ">= 3")
    expect_warning(
        .bfolds(rep(1:3, each = 2), nfolds = 5),
        "reducing to minimal group size"
    )

    set.seed(1)
    cl <- factor(rep(c("a", "b"), c(8, 4)))
    expect_equal(
        .bfolds(cl, 4),
        c(1, 4, 4, 2, 2, 3, 3, 1, 2, 3, 1, 4)
    )
})

test_that(".folds", {
    expect_error(.folds(1:3, nfolds = 2), ">= 3")

    set.seed(2)
    expect_equal(.folds(1:7), c(2:3, 1, 1, 1, 2:3))

    set.seed(1)
    cl <- factor(rep(c("a", "b"), c(8, 4)))
    expect_equal(.folds(cl, 4), c(1, 4, 3, 1, 2, 1, 3, 4, 2, 3, 4, 2))
})

test_that(".mfolds", {
    expect_error(.mfolds(1:3, nfolds = 2), ">= 3")

    set.seed(1)
    r <- do.call(
        rbind,
        lapply(integer(5), function(i)sample(rep_len(seq(4), 12)))
    )
    set.seed(1)
    cl <- factor(rep(c("a", "b"), c(8, 4)))
    expect_equal(.mfolds(cl, 4, 5),  r)
})

test_that(".s2numeric", {
    expect_error(.s2numeric("foo"), "cv.glmnet.* object")

    set.seed(1010)
    n <- 200
    p <- 30
    x <- matrix(rnorm(n * p), n, p)
    nzc <- trunc(p/10)
    beta <- rnorm(nzc)
    fx <- x[, seq(nzc)] %*% beta
    eps <- rnorm(n) * 5
    y <- drop(fx + eps)

    rcv <- rcv.glmnet(
        x, y, nrepcv = 1, nfolds = 3, trace.it = FALSE
    )
    expect_error(.s2numeric(rcv, "foo"))
    expect_equal(.s2numeric(rcv, "lambda.min"), rcv[["lambda.min"]])
    expect_equal(.s2numeric(rcv, c(0.1, 0.2)), c(0.1, 0.2))
})
