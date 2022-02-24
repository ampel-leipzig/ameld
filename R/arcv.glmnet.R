#' Elastic Net with Repeated Cross-Validation for cv.glmnet
#'
#' This functions evaluates elastic net repeated cross validation for alpha and
#' lambda based on [`glmnet::cv.glmnet()`].
#'
#' @param x `matrix`, as in `cv.glmnet`.
#' @param y response as in `cv.glmnet`.
#' @param lambda `numeric`, optional user-supplied lambda sequence; default is
#' `NULL` and `glmnet` chooses its own sequence.
#' @param alpha `numeric`, different `alpha` values that should evaluated (0 =
#' ridge regression, 1 = lasso regression).
#' @param nrepcv `integer(1)`, number of repeated cross-validations (outer
#' loop).
#' @param nfolds `integer`, number of folds, same as in `cv.glmnet`.
#' @param foldid `matrix`, an optional matrix with `nrepcv` rows and
#' `nrow(x)` columns containing ids from 1 to `nfolds` identifying what fold
#' each observation is in. If given `nrepcv` and `nfolds` are ignored.
#' @param balanced `logical`, should classes/status be balanced in the folds
#' (default: FALSE)?
#' @param \dots further arguments passed to `cv.glmnet`.
#' @param trace.it `integer`, if `trace.it = 1`, then a progress bar is
#' displayed.
#'
#' @return An object of class `arcv.glmnet` that extends the `rcv.glmnet` and
#' `cv.glmnet` class.
#'
#' @author Sebastian Gibb
#' @seealso [`glmnet::cv.glmnet()`]
#' @references
#'  Jerome Friedman, Trevor Hastie, Robert Tibshirani (2010).
#'  Regularization Paths for Generalized Linear Models via Coordinate
#'  Descent. Journal of Statistical Software, 33(1), 1-22. URL
#'  \url{https://www.jstatsoft.org/v33/i01/}.
#'
#'  Noah Simon, Jerome Friedman, Trevor Hastie, Rob Tibshirani (2011).
#'  Regularization Paths for Cox's Proportional Hazards Model via
#'  Coordinate Descent. Journal of Statistical Software, 39(5), 1-13. URL
#'  \url{https://www.jstatsoft.org/v39/i05/}.
#'
#' @usage arcv.glmnet(
#'     x, y,
#'     lambda = NULL,
#'     alpha = seq(0L, 1L, by = 0.1),
#'     nrepcv = 100L, nfolds = 10L, foldid = NULL, balanced = FALSE,
#'     ...,
#'     trace.it = interactive()
#' )
#' @export arcv.glmnet
#' @examples
#' # Examples taken from ?"glmnet::cv.glmnet"
#' set.seed(1010)
#' n <- 1000
#' p <- 100
#' nzc <- trunc(p/10)
#' x <- matrix(rnorm(n * p), n, p)
#' beta <- rnorm(nzc)
#' fx <- x[, seq(nzc)] %*% beta
#' eps <- rnorm(n) * 5
#' y <- drop(fx + eps)
#' set.seed(1011)
#' # nrepcv should usually be higher but to keep the runtime of the example low
#' # we choose 2 here
#' arcvob <- arcv.glmnet(x, y, alpha = c(0, 0.5, 1), nrepcv = 2, nfolds = 3)
#' plot(arcvob)
#' title("Gaussian Family", line = 2.5)
#' plot(arcvob, what = "lambda.min")
arcv.glmnet <- function(x, y, lambda = NULL,
                        alpha = seq(0L, 1L, by = 0.1),
                        nrepcv = 100L, nfolds = 10L, foldid = NULL,
                        balanced = FALSE, ...,
                        trace.it = interactive()) {
    if (is.null(foldid)) {
        if (is.matrix(y))
            foldid <- .mfolds(
                y[, "status"], nfolds = nfolds, nrep = nrepcv,
                balanced = balanced
            )
        else
            foldid <- .mfolds(
                y, nfolds = nfolds, nrep = nrepcv, balanced = balanced
            )
    } else {
        nrepcv <- nrow(foldid)
        nfolds <- max(foldid)
    }

    arcv <- future.apply::future_lapply(
        alpha,
        function(a) {
            rcv <- rcv.glmnet(
                x = x, y = y, alpha = a, lambda = lambda,
                nfolds = nfolds, nrepcv = nrepcv, foldid = foldid,
                ...
            )
            rcv
        },
        future.seed = TRUE
    )

    out <- list(
        call = match.call(),
        models = arcv,
        alpha = alpha,
        nrepcv = nrepcv,
        nfolds = nfolds
    )
    class(out) <- c("arcv.glmnet", class(arcv[[1L]]))
    out
}

#' Collect Measurement Data
#'
#' Helper function to fetch measurement data from all models
#' @param x `arcv.glmnet` object.
#' @return `list` for with two elements (min, 1se) each containing a matrix with
#' 5 columns (Lambda, Index, Measure, SE, Nonzero).
#' @importFrom stats setNames
#'
#' @noRd
.collect.measures.arcv.glmnet <- function(x) {
    m <- lapply(x$models, function(m) {
        l <- c(m$lambda.min, m$lambda.1se)
        i <- match(l, m$lambda)
        m <- cbind(
            Lambda = l, Index = i,
            Measure = m$cvm[i], SE = m$cvsd[i],
            Nonzero = m$nzero[i]
        )
        rownames(m) <- c("lambda.min", "lambda.1se")
        m
    })
    setNames(lapply(
        c("lambda.min", "lambda.1se"),
        function(w)
            cbind(Alpha = x$alpha, do.call(rbind, lapply(m, "[", w,)))
    ), c("lambda.min", "lambda.1se"))
}

#' Find Minimal Measurement Error
#'
#' @param x `arcv.glmnet` object.
#' @param s `character`/`numeric`, value(s) of the penality parameter `lambda`.
#' See [`glmnet::predict.cv.glmnet()`] for details.
#' @param maxnnzero `numeric(1)`, maximum number of allowed non-zero beta
#' coefficients. Default is `Inf` which selects the model with the minimal error
#' (the measurement error is chosen from all `"lambda.min"` or `"lambda.1se"`
#' results depending on `s`). If a number is given the model with the lowest
#' (local) error that has at the most `maxnnzero` non-zero beta coefficents
#' is chosen (also based on the given `s`, as described above). If no model has
#' less than `maxnnzero` coefficients the simplest model is chosen and a warning
#' given.
#' @return `numeric` index of model with minimal error.
#' @noRd
.which.min.error <- function(x, s = c("lambda.1se", "lambda.min"),
                             maxnnzero = Inf) {
    s <- match.arg(s)
    m <- .collect.measures.arcv.glmnet(x)[[s]]
    minnnzero <- min(m[, "Nonzero"])

    if (minnnzero > maxnnzero) {
        warning(
            "Lowest number of non-zero coefficients is larger than ",
            "'maxnnzero'. Setting 'maxnnzero' to minimal number of non-zero ",
            "coefficents."
        )
        maxnnzero <- minnnzero
    }

    m[, "Index"] <- seq_len(nrow(m))
    m <- m[m[, "Nonzero"] <= maxnnzero, c("Index", "Measure"), drop = FALSE]
    ## reverse matrix to choose smallest nnzero if multiple minima exists
    m <- m[rev(seq_len(nrow(m))),, drop = FALSE]
    unname(m[which.min(m[, "Measure"]), "Index"])
}

#' Predictions for a `arcv.glmnet` object
#'
#' Compute fitted values for a model fitted by `arcv.glmnet`.
#'
#' @param object `arcv.glmnet` object.
#' @param newx `matrix`, of new values for `x` at which predictions are to be
#' made.
#' @param s `character`/`numeric`, value(s) of the penality parameter `lambda`.
#' See [`glmnet::predict.cv.glmnet()`] for details.
#' @param maxnnzero `numeric(1)`, maximum number of allowed non-zero beta
#' coefficients. Default is `Inf` which selects the model with the minimal error
#' (the measurement error is chosen from all `"lambda.min"` or `"lambda.1se"`
#' results depending on `s`). If a number is given the model with the lowest
#' (local) error that has at the most `maxnnzero` non-zero beta coefficents
#' is chosen (also based on the given `s`, as described above). If no model has
#' less than `maxnnzero` coefficients the simplest model is chosen and a warning
#' given.
#' @param \dots further arguments passed to `predict.rcv.glmnet`.
#'
#' @return The object returned depends on the \dots arguments.
#' See [`predict.rcv.glmnet()`] for details.
#' @author Sebastian Gibb
#' @seealso [`predict.rcv.glmnet()`], [`glmnet::predict.cv.glmnet()`],
#' @method predict arcv.glmnet
#' @rdname arcv.glmnet
#' @export
predict.arcv.glmnet <- function(object,
                                newx,
                                s = c("lambda.1se", "lambda.min"),
                                maxnnzero = Inf, ...) {
    sel <- .which.min.error(object, s = s, maxnnzero = maxnnzero)
    predict(object$models[[sel]], newx = newx, s = s, ...)
}

#' @rdname arcv.glmnet
#' @method print arcv.glmnet
#' @param digits `integer(1)`, number of digits shown in table.
#' @export
print.arcv.glmnet <- function(x, digits = max(3L, getOption("digits") - 3L),
                              ...) {
    cat("\nCall:", deparse(x$call), "\n")
    cat("\nModels:", length(x$models))
    cat("\nAlpha:", x$alpha)
    cat("\nNumber of CV for Lambda:", x$nfolds)
    cat("\nNumber of repeated CV for Lambda:", x$nrepcv)
    cat("\n\n\nMeasure:", x$models[[1L]]$name, "\n\n")
    m <- .collect.measures.arcv.glmnet(x)
    cat("Lambda min:\n")
    print(m[["lambda.min"]], digits = digits)
    cat("\nLambda 1se:\n")
    print(m[["lambda.1se"]], digits = digits)
    invisible()
}

#' Plot the elastic net cross-validation curves
#'
#' This functions plots the aggregated cross-validation curves produced by
#' [`arcv.glmnet()`].
#'
#' @param x `arcv.glmnet` object.
#' @param col `character/numeric`, colours.
#' @param what `character(1)`, what to plot: `"all"` plot all cross-validated
#' loss errors vs lambda of all alpha values, `"lambda.min"`/`"lambda.1se"`
#' plots the "best" lambda for each alpha.
#' @param pch `character/numeric`, point character/symbol.
#' @param \dots further arguments passed to `plot`.
#'
#' @author Sebastian Gibb
#' @seealso [`glmnet::cv.glmnet()`]
#' @importFrom graphics legend lines title
#' @method plot arcv.glmnet
#' @export
plot.arcv.glmnet <- function(x, col = viridisLite::cividis(length(x$alpha)),
                             what = c("all", "lambda.min", "lambda.1se"),
                             pch = 20L, ...) {
    cvm <- lapply(x$models, "[[", "cvm")
    lmbd <- lapply(x$models, "[[", "lambda")
    ylim <- range(cvm)

    what <- match.arg(what)

    if (what == "all") {
        xlim <- log(range(lmbd))
        plot(
            NA,
            xlim = xlim, ylim = ylim,
            xlab = expression(Log(lambda)), ylab = x$models[[1L]]$name,
            type = "n", ...
        )
        for (i in seq(along = cvm))
            lines(log(lmbd[[i]]), cvm[[i]], col = col[i])
        legend("bottomright", legend = x$alpha, col = col, lty = 1L, bty = "n")
    } else {
        w <- vapply(x$models, "[[", NA_real_, what)
        p <- mapply(function(w, m, l) {
            m[w == l]
        }, w = w, m = cvm, l = lmbd)
        plot(
            x$alpha, p,
            xlab = "Alpha", ylab = paste("CV", x$models[[1L]]$name),
            col = col[1L], pch = pch, type = "b", ...
        )
    }
    title(
        sub = paste(
            "Averaged across", x$nrepcv, "repeated cross-validations",
            "each with", x$nfolds, "folds."
        ),
        adj = 0L
    )
}
