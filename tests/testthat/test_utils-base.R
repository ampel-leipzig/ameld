test_that("groupmean", {
    expect_warning(groupmean(1:3, 1:2), "length")
    expect_identical(groupmean(1:3, 1:3), setNames(as.double(1:3), 1:3))
    expect_equal(groupmean(1:9, rep(1:3, 3)), setNames(as.double(4:6), 1:3))
    expect_equal(
        groupmean(c(1, NA, NA, 4:6), rep(1:2, each = 3)),
        c("1"=1, "2"=5)
    )
    expect_equal(
        groupmean(c(1, NA, NA, 4:6), rep(1:2, each = 3), na.rm = FALSE),
        c("1"=NA, "2"=5)
    )
})
