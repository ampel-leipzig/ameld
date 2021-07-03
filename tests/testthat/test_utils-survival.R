sv <- Surv(1:4, c(0, 1, 1, 0))
f <- factor(c("a", "c", "a", "c"), levels = c("a", "b", "c"))

test_that("observed_survival", {
    expect_equal(observed_survival(sv, times = 2), 2/3)
    expect_equal(observed_survival(sv, f, times = 2), c(a=1, b=NA, c=0.5))
    expect_equal(
        observed_survival(sv, f, times = 2:3),
        matrix(
            c(1, 0, NA, NA, 0.5, 0.5), nrow = 2, ncol = 3,
            dimnames = list(2:3, c("a", "b", "c"))
        )
    )
})

test_that("observed_mortality", {
    expect_equal(
        observed_mortality(sv, times = 2),
        1 - observed_survival(sv, times = 2)
    )
    expect_equal(
        observed_mortality(sv, f, times = 2),
        1 - observed_survival(sv, f, times = 2)
    )
    expect_equal(
        observed_mortality(sv, f, times = 2:3),
        1 - observed_survival(sv, f, times = 2:3)
    )
})

test_that("observed_events", {
    expect_equal(observed_events(sv, times = 2), 1)
    expect_equal(observed_events(sv, f, times = 2), c(a=0, b=NA, c=1))
    expect_equal(
        observed_events(sv, f, times = 2:3),
        matrix(
            c(0, 1, NA, NA, 1, 0), nrow = 2, ncol = 3,
            dimnames = list(2:3, c("a", "b", "c"))
        )
    )
})

test_that(".summary_per_strata", {
    a <- array(
        c(2/3, 1/3, 3, 2, 1, 1, 1, 0,
          0.272165, 0.272165, 0.299507, 0.067278, 1, 1),
        dim = c(2, 1, 7),
        dimnames = list(2:3, 1, c(
            "surv", "n.risk", "n.event", "n.censor", "std.err", "lower", "upper"
        ))
    )
    expect_equal(.summary_per_strata(sv, times = 2:3), a, tolerance = 1e-5)
    expect_equal(
        .summary_per_strata(sv, times = 2:3, type = "surv"),
        a[,, "surv", drop = FALSE],
        tolerance = 1e-5
    )
    expect_equal(
        .summary_per_strata(sv, f = rep(1, 4), times = 2:3), a,
        tolerance = 1e-5
    )
})
