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

#' @param cumulative `logical`, if `TRUE` (default `FALSE`) and multiple `times`
#' are given the cumulative events are reported instead of the events in the
#' time periods.
#' @rdname observed_survival
#' @export
#' @examples
#'
#' observed_events(sv, f = f, times = 3)
#' observed_events(sv, f = f, times = 1:4)
#' observed_events(sv, f = f, times = 1:4, cumulative = TRUE)
observed_events <- function(x, f, times, cumulative = FALSE) {
    e <- .summary_per_strata(x, f, times, "n.event")[,,1L]

    if (isTRUE(cumulative))
        drop(apply(as.matrix(e), 2, cumsum))
    else
        e
}

#' Calculate observed vs expected mortality
#'
#' This function creates a table to compare the observed and the expected
#' mortality.
#'
#' @inheritParams observed_survival
#' @param time `numeric(1)`, observed/expected survival at `time` timepoint.
#' @param expected `numeric`, expected mortality rate, has to be of the same
#' length as levels in `f`.
#' @return `data.frame`, with observed/expected mortality rates and events.
#' @export
#' @examples
#' sv <- Surv(1:8, c(0, 1, 1, 0, 1, 1, 1, 0))
#' f <- factor(rep(c("a", "c"), 4), levels = c("a", "b", "c"))
#' observed_vs_expected_mortality(sv, f, time = 3, expected = c(0.5, 0.3, 0.2))
observed_vs_expected_mortality <- function(x, f, time, expected) {
    if (length(time) != 1L || !is.numeric(time))
        stop("'time' has to be a numeric of length 1.")

    y <- .summary_per_strata(x, f, time, c("surv", "n.event", "n.censor"))

    if (ncol(y) != length(expected))
        stop("Length of 'expected' has to be the same as the levels of 'f'.")

    nexpected <- (as.vector(table(f)) - y[,, "n.censor"]) * expected

    data.frame(
        ObservedDeaths = y[,, "n.event"],
        ExpectedDeaths = nexpected,
        StandardizedMortalityRatio = y[,, "n.event"] / nexpected,
        ObservedMortality = 1 - y[,, "surv"],
        ExpectedMortality = expected,
        row.names = colnames(y),
        stringsAsFactors = FALSE
    )
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
