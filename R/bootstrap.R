#' Bootstrap Validation for Survival Model
#'
#' Boostrap validation for survival data as described in Harrell et al. 1996.
#'
#' @param x `matrix`, data/feature matrix.
#' @param y `Surv`, survival time and status as `Surv` object.
#' @param fun model function, e.g. [`rcv.glmnet()`].
#' @param nboot `integer` number of bootstrap samples
#' @param m `integer`, individuals/observations per interval
#' @param times `numeric` predict survival at `times`.
#' @param \ldots further params passed to `fun`.
#' @param s `character`/`numeric`, value(s) of the penality parameter `lambda`.
#' See [`glmnet::predict.cv.glmnet()`] for details.
#' @param verbose `logical`, if `TRUE` a progressbar is shown.
#'
#' @return A `list`, with the fitted model `fit` and the over-optimistic error.
#'
#' @references
#' Harrell Jr, Frank E., Kerry L. Lee, and Daniel B. Mark.
#' "Multivariable prognostic models: issues in developing models, evaluating
#' assumptions and adequacy, and measuring and reducing errors."
#' Statistics in medicine 15.4 (1996): 361-387.
#' \doi{10.1002/(SICI)1097-0258(19960229)15:4<361::AID-SIM168>3.0.CO;2-4}
#'
#' @rdname boot.glmnet
#' @importFrom stats predict
#' @export
#' @examples
#' # nboot should usually be higher but to keep the runtime of the example low
#' # we choose 2 here
#' data(eldd)
#' x <- na.omit(eldd)
#' y <- Surv(x$DaysAtRisk, x$Deceased)
#' x <- as.matrix(x[,c("Age", "ALB_S", "BILI_S", "CRE_S", "INR_C")])
#' boot <- bootstrap(
#'     x, y, rcv.glmnet, family = "cox",
#'     nboot = 2, nrepcv = 2, nfolds = 3
#' )
#' boot
bootstrap <- function(x, y, fun = rcv.glmnet,
                      nboot = 200L, m = 50, times = 90,
                      ..., s = "lambda.1se", verbose = interactive()) {
    fit <- do.call(match.fun(fun), list(x = x, y = y, ...))
    ps <- predict(
        fit, x = x, y = y, newx = x, type = "survival", times = times, s = s
    )
    ctpnts <- .cutpoints(ps, n = m)
    f <- .cut(ps, ctpnts)
    apparent.err <- .prediction_error(
        x = x, y = y, fit = fit, cutpoints = ctpnts, times = times, s = s
    )

    p <- progressr::progressor(nboot, enable = verbose)

    boot <- future.apply::future_lapply(
        seq_len(nboot),
        function(i) {
            bs <- .bootstep(
                x, y, fun = fun, ..., s = s, cutpoints = ctpnts, times = times
            )
            p()
            bs
        },
        future.seed = TRUE
    )

    optm.err <- colMeans(
        do.call(rbind, lapply(boot, "[[", "error")), na.rm = TRUE
    )

    l <- list(
        call = match.call(),
        fit = fit,
        s = s,
        f = f,
        models = boot,
        predicted.survival = groupmean(ps, f),
        observed.survival = observed_survival(y, f = f, times = times),
        observed.std.err =
            .summary_per_strata(
                y, f = f, times = times, c("std.err", "lower", "upper")
            )[1L,,],
        apparent.err = apparent.err,
        optimism.err = optm.err,
        calibrated = apparent.err + optm.err
    )
    class(l) <- c("boot.glmnet", class(fit))
    l
}

#' Bootstrap Step
#'
#' A single boostrap step for survival data as described in Harrell et al. 1996.
#'
#' @param x `matrix`, data/feature matrix.
#' @param y `Surv`, survival time and status as `Surv` object.
#' @param fun model function, e.g. [`rcv.glmnet()`].
#' @param s `character`/`numeric`, value(s) of the penality parameter `lambda`.
#' See [`glmnet::predict.cv.glmnet()`] for details.
#' @param cutpoints cutpoints determined in the full model.
#' @param times `numeric` predict survival at `times`.
#' @param \ldots further params passed to `fun`.
#'
#' @return A `list`, with the fitted model `fit` and the over-optimistic error.
#'
#' @references
#' Harrell Jr, Frank E., Kerry L. Lee, and Daniel B. Mark.
#' "Multivariable prognostic models: issues in developing models, evaluating
#' assumptions and adequacy, and measuring and reducing errors."
#' Statistics in medicine 15.4 (1996): 361-387.
#' \doi{10.1002/(SICI)1097-0258(19960229)15:4<361::AID-SIM168>3.0.CO;2-4}
#'
#' @noRd
.bootstep <- function(x, y, fun = rcv.glmnet, ...,
                      s = "lambda.1se", cutpoints, times = 90) {
    b <- sample(nrow(x), replace = TRUE)
    xb <- x[b,, drop = FALSE]
    yb <- y[b,, drop = FALSE]
    fit <- do.call(match.fun(fun), list(x = xb, y = yb, ...))
    boot.err <- .prediction_error(
        x = xb, y = yb, fit = fit, cutpoints = cutpoints, times = times, s = s
    )
    orig.err <- .prediction_error(
        x = x, y = y, fit = fit, cutpoints = cutpoints, times = times, s = s
    )
    list(
        fit = fit,
        error = boot.err - orig.err
    )
}

#' @rdname boot.glmnet
#' @method print boot.glmnet
#' @param digits `integer(1)`, number of digits shown in table.
#' @export
print.boot.glmnet <- function(x, digits = max(3L, getOption("digits") - 3L),
                              ...) {
    cat("\nCall:", deparse(x$call), "\n")
    cat("\nNumber of bootstrap samples:", length(x$models))
    cat("\nSelected variables:\n")
    cat(rownames(x$fit$glmnet.fit$beta)
          [predict(x$fit, type = "nonzero", s = x$s)[, 1L]], sep = ", ")
    cat("\n\nVariables selected in bootstrap samples:")
    print(.selvars(x$models, x$s))
    cat("\nCalibrated:\n")
    print(x$calibrated)
    invisible()
}

#' Plot the bootstrap results.
#'
#' This functions plots the bootstrap results.
#'
#' @param x `boot.glmnet` object.
#' @param col `character/numeric`, colours.
#' @param what `character(1)`, what to plot: `"selected"` select variables.
#' @param pch `character/numeric`, point character/symbol.
#' @param \dots further arguments passed to `plot`.
#'
#' @author Sebastian Gibb
#' @seealso [`rcv.glmnet()`], [`glmnet::cv.glmnet()`]
#' @importFrom graphics legend lines plot.new plot.window title
#' @method plot boot.glmnet
#' @export
plot.boot.glmnet <- function(x, col = head(viridisLite::viridis(3L), 2L),
                             what = c("calibration", "selected"),
                             pch = 19L, ...) {
    what <- match.arg(what)

    if (what == "calibration")
        .plot.cal(x, col = col, pch = pch, ...)
    else if (what == "selected")
        .plot.sel.var(x, col = col, pch = pch, ...)
}

.plot.cal <- function(x, col, pch, ...) {
    plot(NA, xlim = c(0L, 1L), ylim = c(0L, 1L), axes = FALSE, ann = FALSE)
    title(main = "90 Day Survival", adj = 0L)
    title(ylab = "Observed", adj = 1L)
    title(xlab = "Predicted", adj = 1L)
    abline(0L, 1L, col = "#808080", lty = 2L, lwd = 1L)
    axis(1, lwd.ticks = 0L, col = "#808080")
    axis(2, lwd.ticks = 0L, col = "#808080")

    lines(
        x$predicted.survival, x$observed.survival,
        col = col[1L], type = "b", pch = pch
    )
    lines(
        x$predicted.survival, x$observed.survival + x$optimism.err,
        col = col[2L], type = "b", pch = 4L
    )
    se <- x$observed.std.err
    .errorbars(
        x$predicted.survival, se[, "lower"], se[, "upper"], col = col[1L]
    )
    legend(
        "bottomright",
        col = col[1L:2L],
        pch = c(pch[1L], 4L),
        legend = c(
            "observed",
            paste0("resampling optimism error added ",
                   "(based on ", length(x$models), " bootstrap samples)")
        ),
        bty = "n"
    )
    abline(0L, 1L, col = "#808080", lty = 2L, lwd = 1L)
}

.plot.sel.var <- function(x, col, pch, ...) {
    var.final <- rownames(x$fit$glmnet.fit$beta)[
        predict(x$fit, type = "nonzero", s = x$s)[,1L]
    ]
    var.boot <- .selvars(x$models, x$s)
    var.boot <- c(var.boot[order(var.boot, rev(names(var.boot)))])
    n <- length(var.boot)

    clr <- rep_len(col[1L], n)
    clr[names(var.boot) %in% var.final] <- col[2L]

    plot.new()
    w <- max(strwidth(names(var.boot), "inch"), na.rm = TRUE) + 1/16
    mai <- par("mai")
    if (mai[2L] < w) {
        mai[2L] <- mai[4L] + w # taken from dotchart
        old.par <- par(mai = mai, no.readonly = TRUE)
        on.exit(par(old.par))
    }

    nboot <- length(x$models)
    plot.window(xlim = c(0L, nboot), ylim = c(0L, n + 1L))
    title(
        main = paste(
            "Frequency of Variable Selection across", nboot, "Bootstrap Samples"
        ),
        adj = 0L
    )
    title(xlab = "Frequency", adj = 1L)
    y <- seq_len(n)
    mtext(
        names(var.boot), at = y, adj = 0L, side = 2L, las = 2L,
        line = (w + 0.1) / par("csi"), col = clr
    )
    abline(h = y, col = "#808080", lty = "dotted", lwd = 1L)
    abline(v = nboot * 0.95, col = "#808080", lty = "dashed", lwd = 0.75)
    points(var.boot, y, col = clr, pch = pch)
    legend(
        "bottomright",
        legend = c("final variables", "bootstrap selected variables"),
        col = rev(col), pch = pch,
        bty = "n"
    )
    axis(1, lwd.ticks = 0L, col = "#808080")
}
