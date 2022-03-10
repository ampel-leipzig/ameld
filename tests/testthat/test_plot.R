test_that("plot_dots", {
    vdiffr::expect_doppelganger("plot_dots", function()
        plot_dots(c(Foo = 3, Bar = 5), xlim = c(0, 8), main = "FooBar"))
})

test_that("plot_surv", {
    expect_error(plot_surv(1:3), "'survfit' class")

    srvft <- survfit(Surv(time, status) ~ 1, data = survival::aml)
    vdiffr::expect_doppelganger("plot_surv-all", function()plot_surv(srvft))

    srvft <- survfit(Surv(time, status) ~ x, data = survival::aml)
    vdiffr::expect_doppelganger("plot_surv-strata", function()
        plot_surv(srvft, main = "with Strata"))
})

test_that("plot_surv_roc", {
    d <- data.frame(
        time = c(5, 7, 7, 14, 30),
        event = c(1, 0, 1, 0, 1),
        marker = c(30, 1, 25, 26, 10)
    )
    tr <- timeROC::timeROC(
        d$time, d$event, d$marker, time = c(7, 30), cause = 1
    )
    expect_error(plot_surv_roc(1:10))
    expect_error(plot_surv_roc(list(tr), 30))
    expect_error(plot_surv_roc(list(foo = tr), 100))
    expect_error(plot_surv_roc(list(foo = tr), c(5, 7)))
    expect_error(plot_surv_roc(list(foo = tr), 5, col = 1:2))
    expect_error(plot_surv_roc(list(foo = tr), 5, col = 1, lty = 1:2))
    expect_error(plot_surv_roc(list(foo = tr), 5, col = 1, lty = 1:2))
    expect_error(plot_surv_roc(list(foo = tr), 5, main = c("foo", "bar")))

    tr2 <- timeROC::timeROC(
        d$time, d$event, rev(d$marker), time = c(7, 30), cause = 1
    )
    vdiffr::expect_doppelganger(
        "plot_surv_roc", function() {
            plot_surv_roc(list(foo = tr, bar = tr2), timepoint = 7)
        }
    )
})

test_that("plot_table", {
    m <- matrix(
        1:8, nrow = 4,
        dimnames = list(c(0, 30, 90, 365), LETTERS[1:2])
    )
    vdiffr::expect_doppelganger("plot_table", function()
        plot_table(m, main = "main", col = 2:3))
})

test_that("rjlegend", {
    br <- function(){ plot(1:10); rjlegend(legend = c("AB", "CDE"), col = 1:2) }
    tl <- function(){
        plot(1:10); rjlegend("topleft", legend = c("AB", "CDE"), col = 1:2)
    }
    vdiffr::expect_doppelganger("rjlegend-br", br)
    vdiffr::expect_doppelganger("rjlegend-tl", tl)
})
