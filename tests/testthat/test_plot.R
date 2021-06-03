test_that("plot_surv", {
    expect_error(plot_surv(1:3), "'survfit' class")

    srvft <- survfit(Surv(time, status) ~ 1, data = survival::aml)
    vdiffr::expect_doppelganger("plot_surv-all", function()plot_surv(srvft))

    srvft <- survfit(Surv(time, status) ~ x, data = survival::aml)
    vdiffr::expect_doppelganger("plot_surv-strata", function()
        plot_surv(srvft, main = "with Strata"))
})

test_that("plot_table", {
    m <- matrix(
        1:8, nrow = 4,
        dimnames = list(c(0, 30, 90, 365), LETTERS[1:2])
    )
    vdiffr::expect_doppelganger("plot_table", function()
        plot_table(m, main = "main"))
})
