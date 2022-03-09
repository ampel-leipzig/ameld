#' Repeated Cross-Validation for cv.glmnet
#'
#' This functions returns the best lambda of repeated [`glmnet::cv.glmnet()`]
#' calls.
#'
#' @param x `matrix`, as in `cv.glmnet`.
#' @param y response as in `cv.glmnet`.
#' @param lambda `numeric`, optional user-supplied lambda sequence; default is
#' `NULL` and `glmnet` chooses its own sequence.
#' @param alpha `numeric(1)`, the elasticnet mixing parameter, default is `1`
#' (lasso penality); see [`glmnet::glmnet()`] for details.
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
#' @return An object of class `rcv.glmnet` that extends the `cv.glmnet`
#' class.
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
#' @importFrom glmnet cv.glmnet
#' @usage rcv.glmnet(
#'     x, y,
#'     lambda = NULL, alpha = 1,
#'     nrepcv = 100L, nfolds = 10L, foldid = NULL, balanced = FALSE,
#'     ...,
#'     trace.it = interactive()
#' )
#' @export rcv.glmnet
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
#' rcvob <- rcv.glmnet(x, y, nrepcv = 2, nfolds = 3)
#' plot(rcvob)
#' title("Gaussian Family", line = 2.5)
#' coef(rcvob)
#' predict(rcvob, newx = x[1:5, ], s = "lambda.min")
rcv.glmnet <- function(x, y, lambda = NULL, alpha = 1,
                       nrepcv = 100L, nfolds = 10L, foldid = NULL,
                       balanced = FALSE,
                       ...,
                       trace.it = interactive()) {

    if (is.null(foldid)) {
        if (is.matrix(y))
            foldid <- .mfolds(
                y[, "status"], nfolds, nrep = nrepcv, balanced = balanced
            )
        else
            foldid <- .mfolds(
                y, nfolds = nfolds, nrep = nrepcv, balanced = balanced
            )
    } else {
        nrepcv <- nrow(foldid)
    }

    p <- progressr::progressor(nrepcv, enable = trace.it)

    rcv <- future.apply::future_lapply(
        seq_len(nrepcv),
        function(i) {
            doFuture::registerDoFuture()
            cv <- cv.glmnet(
                x = x, y = y, lambda = lambda, alpha = alpha,
                foldid = foldid[i, ], ...,
                parallel = TRUE
            )
            p()
            cv
        },
        future.seed = TRUE
    )

    cvm <- cvsd <- matrix(
        NA_real_, nrow = length(rcv[[1L]]$lambda), ncol = nrepcv
    )

    for (i in seq(along=rcv)) {
        cvm[, i] <- rcv[[i]]$cvm
        cvsd[, i] <- rcv[[i]]$cvsd
    }

    cvm <- rowMeans(cvm)
    cvsd <- rowMeans(cvsd)
    i <- which.min(cvm)

    index <- matrix(
        c(i, which(cvm < (cvm[i] + cvsd[i]))[1L]),
        nrow = 2L, ncol = 1L, dimnames = list(c("min", "1se"), "Lambda")
    )

    out <- list(
        call = match.call(),
        alpha = alpha,
        cvm = cvm,
        cvsd = cvsd,
        cvup = cvm + cvsd,
        cvlo = cvm - cvsd,
        index = index,
        nrepcv = nrepcv,
        nfolds = nfolds,
        lambda.min = rcv[[1L]]$lambda[index["min",]],
        lambda.1se = rcv[[1L]]$lambda[index["1se",]]
    )
    out <- c(out, rcv[[1L]][c("glmnet.fit", "lambda", "name", "nzero")])
    class(out) <- c("rcv.glmnet", class(rcv[[1L]]))
    out
}

#' Plot the cross-validation curve
#'
#' This functions plots the aggregated cross-validation curve produced by
#' [`rcv.glmnet()`].
#'
#' @param x `rcv.glmnet` object.
#' @param \dots further arguments passed to `plot.cv.glmnet`.
#'
#' @author Sebastian Gibb
#' @seealso [`glmnet::cv.glmnet()`]
#' @importFrom graphics title
#' @method plot rcv.glmnet
#' @export
plot.rcv.glmnet <- function(x, ...) {
    NextMethod()
    title(
        sub = paste(
            "Averaged across", x$nrepcv, "repeated cross-validations",
            "each with", x$nfolds, "folds."
        ),
        adj = 0L
    )
}

#' Predictions for a `rcv.glmnet` object
#'
#' Compute fitted values for a model fitted by `rcv.glmnet`.
#'
#' @param object `rcv.glmnet` object.
#' @param newx `matrix`, of new values for `x` at which predictions are to be
#' made.
#' @param s `character`/`numeric`, value(s) of the penality parameter `lambda`.
#' See [`glmnet::predict.cv.glmnet()`] for details.
#' @param type `character`, type of prediction. For `"survival"`
#' the predicted survival is returned for each timepoint in `times`. All other
#' types are passed to `predict.cv.glmnet`
#' See [`glmnet::predict.cv.glmnet()`] for details.
#' @param times `numeric`, vector of times, the returned matrix will contain one
#' row for each time. See [`survival::summary.survfit()`] for details.
#' @param \dots further arguments passed to `predict.cv.glmnet`.
#'
#' @return The object returned depends on the \dots arguments.
#' See [`glmnet::predict.cv.glmnet()`] for details. For `type = "survival"` the
#' returned value is a `matrix` with one row per `times` element and one column
#' for each row in `newx`.
#' @author Sebastian Gibb
#' @seealso [`rcv.glmnet()`], [`glmnet::predict.cv.glmnet()`],
#' [`survival::summary.survfit()`]
#' @importFrom methods is
#' @method predict rcv.glmnet
#' @export
#' @examples
#' # Example adapted from ?"glmnet::cv.glmnet"
#' set.seed(10101)
#' n <- 500
#' p <- 30
#' nzc <- trunc(p / 10)
#' x <- matrix(rnorm(n * p), n, p)
#' beta <- rnorm(nzc)
#' fx <- x[, seq(nzc)] %*% beta / 3
#' hx <- exp(fx)
#' ty <- rexp(n, hx)
#' tcens <- rbinom(n = n, prob = 0.3, size = 1)  # censoring indicator
#' # y <- Surv(ty, 1-tcens) with library("survival")
#' y <- cbind(time = ty, status = 1 - tcens)
#' # nrepcv should usually be higher but to keep the runtime of the example low
#' # we choose 2 here
#' rcvob <- rcv.glmnet(x, y, family = "cox", nrepcv = 2, nfolds = 3)
#' predict(
#'     rcvob,
#'     newx = x[1:5,], x = x, y = survival::Surv(y[, "time"], y[, "status"]),
#'     type = "survival", times = c(0, 7), s = "lambda.1se"
#' )
predict.rcv.glmnet <- function(object, newx, s = c("lambda.1se", "lambda.min"),
                               type = c("link", "response", "coefficients",
                                        "nonzero", "class", "survival"),
                               times, ...) {
    type <- match.arg(type)

    if (type == "survival") {
        if (!is(object$glmnet.fit, "coxnet"))
            stop(
                "Survival prediction is just supported for ",
                "'rcv.glmnet(..., family = \"cox\")."
            )
        requireNamespace("survival")
        s <- .s2numeric(object, s)
        if (length(s) != 1L)
            stop("'s' has to be an 'numeric' or 'character' of length 1.")
        args <- list(...)
        srvft <- survival::survfit(object, newx = newx, s = s, ...)
        summaryargs <-
            args[names(args) %in% c("censored", "scale", "extend", "rmean")]

        if (!missing(times))
            summaryargs <- append(summaryargs, list(times = times))
        do.call(
            summary,
            append(list(object = srvft), summaryargs)
        )$surv
    } else
        NextMethod()
}
