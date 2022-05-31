#' Baseline hazard
#'
#' Make `[survival::basehaz()]` available for `coxnet` objects.
#'
#' @param fit fitted model.
#' @param \dots further arguments passed to `basehaz`.
#'
#' @return `data.frame` with variable names `hazard`, `time` and optionally
#' `strata`.
#' @author Sebastian Gibb
#' @seealso [`survival::basehaz()`]
#' @rdname basehaz
#' @export
#' @examples
#'
basehaz <- function(fit, ...)UseMethod("basehaz")

#' @param fit fitted model.
#' @param centered `logical(1)`, see [`survival::basehaz()`].
#' @rdname basehaz
#' @importFrom survival basehaz
#' @method basehaz coxph
#' @export
basehaz.default <- basehaz.coxph <- function(fit, centered = TRUE) {
    survival::basehaz(fit, centered = centered)
}

#' @param x `matrix`, model matrix used to fit `fit`.
#' @param y `Surv`, Survival object used to fit `fit`.
#' @param s `numeric(1)`, lambda penality parameter.
#' @rdname basehaz
#' @method basehaz cv.glmnet
#' @export
basehaz.cv.glmnet <- function(fit, x, y,
                              s = c("lambda.1se", "lambda.min"),
                              centered = TRUE) {
    basehaz(
        fit$glmnet.fit,
        x = x, y = y,
        s = .s2numeric(fit, s),
        centered = centered
    )
}

#' @rdname basehaz
#' @method basehaz coxnet
#' @export
basehaz.coxnet <- function(fit, x, y, s = NULL, centered = TRUE) {
    if (is.null(s))
        stop("'s' has to be a valid `numeric` lambda value.")

    srvfit <- survfit(fit, x = x, y = y, s = s)

    if (!centered) {
        lp <- predict(fit, newx = x, type = "link", s = s)
        chaz <- srvfit$cumhaz * exp(-mean(lp))
    } else
        chaz <- srvfit$cumhaz

    d <- data.frame(hazard = chaz, time = srvfit$time)

    strata <- srvfit$strata
    if (!is.null(strata)) {
        nms <- names(strata)
        d$strata <- factor(rep(nms, strata), levels = nms)
    }
    d
}
