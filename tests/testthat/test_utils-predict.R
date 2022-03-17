test_that("cutpoints", {
    x <- seq(0, 1, length.out = 10)
    expect_equal(cutpoints(x, n = 5), c(0, 0.5, 1))
    expect_equal(cutpoints(x, n = 10), c(0, 1))
    expect_equal(as.vector(table(.cut(x, cutpoints(x, n = 2)))), rep(2, 5))
})
