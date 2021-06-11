test_that("as_si", {
    expect_equal(as_si(1:2, "bilirubin"), c(17.1, 34.2))
    expect_equal(as_si(1:2, "creatinine"), c(88.4, 176.8))
})

test_that("as_metric", {
    expect_equal(as_metric(c(17.1, 34.2), "bilirubin"), 1:2)
    expect_equal(as_metric(c(88.4, 176.8), "creatinine"), 1:2)
})
