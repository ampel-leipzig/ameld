test_that("plot_surv", {
    expect_error(plot_surv(1:3), "'survfit' object")

    srvft <- survfit(Surv(time, status) ~ 1, data = survival::aml)
    vdiffr::expect_doppelganger("plot_surv-all", function()plot_surv(srvfit))

    srvft <- survfit(Surv(time, status) ~ x, data = survival::aml)
    vdiffr::expect_doppelganger("plot_surv-strata" function()
        plot_surv(srvfit, main = "with Strata"))
})
