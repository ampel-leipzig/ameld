#' Calculate observed survival informations
#'
#' This functions calculate some survival information (e.g. survival rate,
#' mortality rate).
#'
#' @param x `Surv`, object
#' @param f `factor`, strata/splitting factor
#' @param times `numeric`, information at times
#' @return `double`, with an element for each level in `f`,
#' if times is a `numeric` of length 1;  or a `matrix` with
#' as much rows as elements in `times` and a column for each level in `f`.
#'
#' @details
#' In contrast to [`survival::summary.survfit()`] these functions do not remove
#' empty strata (e.g. important for bootstrapping).
#'
#' @importFrom survival Surv survfit
#' @rdname observed_survival
#' @export
#' @examples
#' sv <- Surv(1:4, c(0, 1, 1, 0))
#' f <- factor(c("a", "c", "a", "c"), levels = c("a", "b", "c"))
#' observed_survival(sv, times = 3)
#' observed_survival(sv, f = f, times = 3)
#' observed_survival(sv, f = f, times = 1:4)
observed_survival <- function(x, f, times) {
    .summary_per_strata(x, f, times, "surv")[,,1L]
}

#' @rdname observed_survival
#' @export
#' @examples
#'
#' observed_mortality(sv, f = f, times = 3)
observed_mortality <- function(x, f, times) {
    1 - .summary_per_strata(x, f, times, "surv")[,,1L]
}

#' @rdname observed_survival
#' @export
#' @examples
#'
#' observed_events(sv, f = f, times = 3)
observed_events <- function(x, f, times) {
    .summary_per_strata(x, f, times, "n.event")[,,1L]
}

#' Calculate observed survival summary per strata
#'
#' This function uses [`survival::summary.survfit()`] to calculate survival data
#' per strata. For bootstrapping and calibration curves empty strata are
#' necessary which are not possible with the original `summary` function.
#'
#' @param x `Surv` object.
#' @param f `factor`, strata/splitting factor.
#' @param times `numeric`, survival at times.
#' @param type `character`, summary type to extract.
#' @return `matrix`, a row for each timepoint in `times` and a column for each
#' strata provided by `f`.
#' @importFrom survival Surv survfit
#' @noRd
#' @examples
#' sv <- Surv(1:4, c(0, 1, 1, 0))
#' .summary_per_strata(sv, times = 3)
#' .summary_per_strata(sv, f = factor(c(1, 3, 1, 3), levels = 1:3), times = 3)
.summary_per_strata <- function(x, f, times,
    type = c("surv", "n.risk", "n.event", "n.censor",
             "std.err", "lower", "upper")) {

    type <- match.arg(type, several.ok = TRUE)
    ntype <- length(type)

    if (missing(f))
        f <- factor(rep_len(1L, length(x)))
    if (!is.factor(f))
        f <- as.factor(f)

    ntimes <- length(times)
    sm <- summary(survfit(x ~ f), times = times, extend = TRUE)

    a <- array(
        data = NA_real_,
        dim = c(ntimes, nlevels(f), ntype),
        dimnames = list(times, levels(f), type)
    )
    if (nlevels(f) > 1)
        a[, paste0("f=", levels(f)) %in% sm$strata, ] <- unlist(sm[type])
    else
        a[, , ] <- unlist(sm[type])
    a
}
