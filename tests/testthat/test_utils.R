test_that("age", {
    expect_error(age(as.POSIXct("2022-09-10"),
                     as.POSIXct("2019-09-10 14:15:00")), "negative")
    expect_identical(age(as.POSIXct(c("2019-09-10", "2017-09-10", "1999-01-01")),
                         as.POSIXct(rep(("2019-09-10 14:15:00"), 3))),
                         c(0L, 2L, 20L))
})

test_that("daysAtRisk", {
    expect_identical(daysAtRisk(as.POSIXct("2019-09-10 14:15:00"),
                                as.POSIXct("2019-09-10")), 0L)
    expect_identical(daysAtRisk(as.POSIXct("2019-09-10 14:15:00"),
                                as.POSIXct("2019-09-13")), 3L)
    expect_identical(daysAtRisk(as.POSIXct("2019-09-10 14:15:00"),
                                as.POSIXct("2019-09-12"),
                                as.POSIXct("2019-09-13")), 3L)
    expect_identical(daysAtRisk(as.POSIXct("2019-09-10 14:15:00"),
                                as.POSIXct("2019-09-12"),
                                as.POSIXct("2019-09-01")), 2L)
    expect_identical(daysAtRisk(as.POSIXct("2019-09-10 14:15:00"),
                                as.POSIXct("2019-09-14"),
                                as.POSIXct("2019-09-09"),
                                as.POSIXct("2019-09-14")), 4L)
    expect_identical(daysAtRisk(as.POSIXct(c("2019-09-10 14:15:00",
                                             "2019-09-10 14:15:00")),
                                as.POSIXct(c("2019-09-12", "2019-10-01")),
                                as.POSIXct(c("2019-09-13", "2019-09-13"))),
                     c(3L, 3L))
})
