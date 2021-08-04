test_that("enum", {
    expect_identical(enum(1), "1")
    expect_identical(enum(1:2), "1 and 2")
    expect_identical(enum(1:3), "1, 2, and 3")
    expect_identical(enum(1:3, oxford = FALSE), "1, 2 and 3")
    expect_identical(enum(11:13, conjunction = "or"), "11, 12, or 13")
    expect_identical(enum(paste0("[foo", 1:3, ", bar", 1:3, "]")),
                     "[foo1, bar1], [foo2, bar2], and [foo3, bar3]")
    expect_identical(enum(c("foo", "bar", "x", "y")), "foo, bar, x, and y")
})
