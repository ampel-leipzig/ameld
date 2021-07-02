#' Plot dotchart
#'
#' This is a simplified and customized version of [`graphics::dotchart()`].
#'
#' @param x `numeric`, values to plot.
#' @param xlim `numeric(2)`, limits of the x-axis.
#' @param main `character(1)`, plot title.
#' @param xlab `character(1)`, x-axis label.
#' @param col `integer`/`character`, color of the dots.
#' @param pch `integer`/`character`, point character/symbol of the dots.
#'
#' @return nothing, used for its side-effects (plotting).
#'
#' @seealso [`graphics::dotchart()`]
#' @importFrom graphics abline mtext par points strwidth
#' @export
#' @examples
#' x <- c(Foo = 3, Bar = 5)
#' plot_dots(x, xlim = c(0, 8))
plot_dots <- function(x, xlim = c(0, max(x)),
                      main = "Dotchart", xlab = "Frequency",
                      col = palette.colors(2L)[2L], pch = 19L) {
    old.par <- par(no.readonly = TRUE)
    on.exit(par(old.par))

    n <- length(x)

    mai <- par("mai")
    w <- max(strwidth(names(x), "inch"), na.rm = TRUE) + 1/16
    if (mai[2L] < w)
        mai[2L] <- mai[4L] + w # taken from dotchart
    par(mai = mai)

    plot(NA, xlim = xlim, ylim = c(0L, n + 1L),
        axes = FALSE, ann = FALSE
    )
    title(main = main, adj = 0L)
    title(xlab = xlab, adj = 1L)
    y <- seq_len(n)
    mtext(
        names(x), at = y, adj = 0L, side = 2L, las = 2L,
        line = (w + 0.1) / par("csi"), cex = 0.8
    )
    abline(h = y, col = "#808080", lty = "dotted", lwd = 1L)
    points(x, y, col = col, pch = pch)
    axis(1L, lwd.ticks = 0L, col = "#808080")
    invisible(NULL)
}

#' Plot method for 'survfit' objects
#'
#' This is just a wrapper method around [`survival::plot.survfit()`] with
#' custom defaults.
#'
#' @param x `survfit` object.
#' @param main `character(1)`, plot title.
#' @param xlab `character(1)`, x-axis label.
#' @param ylab `character(1)`, y-axis label.
#' @param mark.time `logical(1)`, if `TRUE` censoring times are marked, see
#' [`survival::plot.survfit()`] for details.
#' @param conf.int `logical(1)`, if `TRUE` confidence interval is plotted, see
#' [`survival::plot.survfit()`] for details.
#' @param col `integer`/`character`, specifying colors for each curve.
#' @param times `integer`, vector of times to print on the x-axis.
#' @param \dots further arguments passed to [`survival::plot.survfit()`].
#'
#' @return a list with `x` and `y` containing the coordinates of the last point
#' of each curves.
#'
#' @seealso [`survival::plot.survfit()`]
#'
#' @import survival
#' @importFrom graphics axTicks axis text title
#' @importFrom grDevices palette.colors
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
    col = palette.colors(max(1L, length(x$strata))),
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
#' @param x `matrix`, it is transposed on the graphic device. The column names
#' correspond to the y labels and the row names to the x labels.
#' @param main `character(1)`, plot title.
#' @param xlab `character(1)`, x-axis label.
#' @param ylab `character(1)`, y-axis label.
#' @param at `numeric, where to plot the rows of `x`.
#' @param xlim `numeric(2)`, limits of the x-axis.
#' @param ylim `numeric(2)`, limits of the y-axis.
#' @param ylabels `logical(1)`, should the column names used to labels the
#' y-axis (default: `TRUE`)?
#' @param col `integer`/`character`, specifying the color for each y/column
#' label.
#' @param xaxis `logical(1)`, should the x-axis be plotted (default: `TRUE`)?
#' @param cex.xaxis `numeric(1)`, character expansion factor for the x-axis
#' labels, see [`par()`] for details.
#' @param cex.yaxis `numeric(1)`, character expansion factor for the y-axis
#' labels, see [`par()`] for details.
#' @param cex.text `numeric(1)`, character expansion factor for the cell content
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
                at = (i - 1L), padj = -1L, las = 1L, labels = nm[i],
                col.axis = col[i], cex.axis = cex.yaxis, tick = FALSE
            )
        }
    }

    text(
        at, rep(seq_len(nc) - 1L, each = length(at)), pos = 3L,
        labels = x, cex = cex.text
    )
}
