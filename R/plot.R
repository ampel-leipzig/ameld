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
#' @param times integer, vector of times to print on the x-axis.
#' @param \dots further arguments passed to [`survival::plot.survfit()`].
#'
#' @return a list with `x` and `y` containing the coordinates of the last point
#' of each curves.
#'
#' @seealso [`survival::plot.survfit()`]
#'
#' @import survival
#' @importFrom graphics axTicks axis text title
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
    times,
    ...) {

    if (!inherits(x, "survfit"))
        stop("'x' has to be an object of the 'survfit' class.")

    p <- plot(
        x,
        mark.time = mark.time, conf.int = conf.int, col = col,
        axes = FALSE, ann = FALSE,
        ...
    )
    if (missing(times))
        times <- axTicks(1L)
    title(main = main, adj = 0L)
    title(xlab = xlab, adj = 1L)
    title(ylab = ylab, adj = 1L)
    axis(1L, at = times, lwd.ticks = 0L, col = "#808080")
    axis(2L, lwd.ticks = 0L, col = "#808080")
    invisible(p)
}

#' Plot a table
#'
#' Plot a table on the current graphic device. Useful for risk tables.
#'
#' @param x matrix, it is transposed on the graphic device. The column names
#' correspond to the y labels and the row names to the x labels.
#' @param main character(1), plot title.
#' @param xlab character(1), x-axis label.
#' @param ylab character(1), y-axis label.
#' @param at numeric, where to plot the rows of `x`.
#' @param xlim numeric(2), limits of the x-axis.
#' @param ylim numeric(2), limits of the y-axis.
#' @param ylabels logical(1), should the column names used to labels the y-axis
#' (default: `TRUE`)?
#' @param col integer/character, specifying the color for each y/column label.
#' @param xaxis logical(1), should the x-axis be plotted (default: `TRUE`)?
#' @param cex.xaxis numeric(1), character expansion factor for the x-axis
#' labels, see [`par()`] for details.
#' @param cex.yaxis numeric(1), character expansion factor for the y-axis
#' labels, see [`par()`] for details.
#' @param cex.text numeric(1), character expansion factor for the cell content
#' labels, see [`par()`] for details.
#' @param \dots further arguments passed to [`plot.default()`].
#'
#' @return nothing, used for its side-effects (plotting).
#'
#' @export
#' @examples
#' m <- matrix(
#'     1:8, nrow = 4,
#'     dimnames = list(c(0, 30, 90, 365), LETTERS[1:2])
#' )
#' plot_table(m, main = "Cumulative number of events")
plot_table <- function(
    x,
    main = character(),
    xlab = character(),
    ylab = character(),
    at = seq_len(nrow(x)) - 1L,
    xlim = range(at),
    ylim = c(0L, ncol(x)),
    ylabels = TRUE,
    col = rep_len(1L, ncol(x)),
    xaxis = TRUE,
    cex.xaxis = 3/4,
    cex.yaxis = 1.25,
    cex.text = 1.5,
    ...
    ) {

    plot(
        NA,
        xlim = xlim,
        ylim = ylim,
        axes = FALSE, ann = FALSE,
        ...
    )
    title(main = main, adj = 0L)
    title(xlab = xlab, adj = 1L)
    title(ylab = ylab, adj = 1L)

    if (xaxis)
        axis(1L, at = at, cex.axis = cex.xaxis, lwd.ticks = 0L, col = "#808080")

    nc <- ncol(x)

    if (ylabels) {
        nm <- colnames(x)

        for (i in seq_len(nc)) {
            axis(
                side = 2L,
                at = (i - 1L), labels = nm[i], col.axis = col[i],
                cex.axis = cex.yaxis, tick = FALSE, las = 1L
            )
        }
    }

    text(
        at, rep(seq_len(nc) - 1L, each = length(at)), pos = 3L,
        labels = x, cex = cex.text
    )
}
