#' Calculate error between predicted and observed survival
#'
#' This function calculates errors between predicted and observed survival for
#' user given groups.
#'
#' @param x `matrix`, data/feature matrix.
#' @param y `Surv`, survival time and status as `Surv` object.
#' @param fit fitted model, e.g. [`rcv.glmnet()`].
#' @param s `character`/`numeric`, value(s) of the penality parameter `lambda`.
#' See [`glmnet::predict.cv.glmnet()`] for details.
#' @param cutpoints cutpoints determined in the full model.
#' @param times `numeric` predict survival at `times`.
#' @return `double`, prediction error per cutpoint interval
#' @noRd
.prediction_error <- function(x, y, fit, s = "lambda.1se",
                             cutpoints, times = 90) {
    ps <- predict(
        fit, x = x, y = y, newx = x, type = "survival", times = times, s = s
    )
    f <- .cut(ps, cutpoints)
    groupmean(ps, f = f) - observed_survival(y, f = f, times = times)
}

#' cut with different default arguments
#' @noRd
.cut <- function(x, ..., include.lowest = TRUE)
    cut(x, ..., include.lowest = include.lowest)

#' Calculate cutpoints/breaks
#'
#' Calculate cutpoints/breaks for `cut` to have an equal number of
#' patients/observations per group.
#'
#' @param x `double` predicted survival
#' @param n `integer(1)` number of patients/observation per interval
#' @return `double`, cutpoints
#' @noRd
#' @examples
#' x <- seq(0, 1, length.out = 10)
#' .cutpoints(x, n = 2)
.cutpoints <- function(x, n = 50L) {
    unique(
        quantile(
            c(0L, x, 1L),
            seq(0L, 1L, length.out = (floor(length(x) / n) + 1L))
        )
    )
}
