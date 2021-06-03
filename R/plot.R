#' Plot method for 'survfit' objects
#'
#' This is just a wrapper method around [`survival::plot.survfit()`] with
#' custom defaults.
#'
#' @param x `survfit` object.
#' @param main character, plot title.
#' @param xlab character, x-axis label.
#' @param ylab character, y-axis label.
#' @param mark.time logical, if `TRUE` censoring times are marked, see
#' [`survival::plot.survfit()`] for details.
#' @param conf.int logical, if `TRUE` confidence interval is plotted, see
#' [`survival::plot.survfit()`] for details.
#' @param col integer/character, specifying colors for each curve.
#' @param \dots further arguments passed to [`survival::plot.survfit()`].
#'
#' @return a list with `x` and `y` containing the coordinates of the last point
#' of each curves.
#'
#' @import survival
#' @importFrom viridisLite cividis
#' @export
#' @examples
#' library("survival")
#' srvfit <- survfit(Surv(time, status) ~ x, data = aml)
#' plot_surv(srvfit)
plot_surv <- function(
    x,
    main = character(),
    xlab = "Time",
    ylab = "Overall survival probability",
    mark.time = TRUE,
    conf.int = FALSE,
    col = cividis(max(1L, length(x$strata))),
    ...) {

    if (!is(x, "survfit"))
        stop("'x' has to be an object of the 'survfit' class.")

    p <- plot(
        x,
        mark.time = mark.time, conf.int = conf.int, col = col,
        axes = FALSE, ann = FALSE,
        ...
    )
    title(main = main, adj = 0L)
    title(ylab = ylab, adj = 1L)
    title(xlab = xlab, adj = 1L)
    axis(1, lwd.ticks = 0L, col = "#808080")
    axis(2, lwd.ticks = 0L, col = "#808080")
    invisible(p)
}
