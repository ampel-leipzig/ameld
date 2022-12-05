#' Sort unique thresholds
#'
#' Sort unique thresholds for ROC curves calculation
#'
#' @param x `numeric`, marker value
#' @return `numeric`, sorted unique marker values
#' @noRd
.thresholds <- function(x) {
    x <- sort(unique(x))
    (c(-Inf, x) + c(x, Inf)) / 2
}

#' TP/FP (Se/Sp) for thresholds
#'
#' Calculate TP/FP based on timeROC algorithm.
#' Original code: Paul Blanche <paulfblanche@gmail.com>, SeSpPPVNPV.R
#'
#' @param T `numeric`, time, see [`timeROC::timeROC()`].
#' @param delta `numeric`, status, see [`timeROC::timeROC()`].
#' @param marker `numeric`, marker, see [`timeROC::timeROC()`].
#' @param thresholds `numeric`, thresholds, see [`timeROC::timeROC()`].
#' @param timepoint `numeric(1)`, timepoint to evaluate.
#' @param cause `numeric`, cause, see [`timeROC::timeROC()`].
#' @return `matrix`, 2 rows (TP and FP), `length(threshold)` columns.
#' @noRd
.tpfp <- function(T, delta, marker, thresholds, timepoint, cause = 1) {
    ## ipcw function needs values ordered by time
    order_T <- order(T)
    T <- T[order_T]
    delta <- delta[order_T]
    marker <- marker[order_T]

    weights <- pec::ipcw(
        Surv(failure_time, status) ~ 1,
        data = data.frame(failure_time = T, status = as.numeric(delta != 0)),
        method = "marginal", times = c(0, timepoint),
        subjectTimes = T, subjectTimesLag = 1
    )

    ## reorder by marker values (in order to compute Se and Sp)
    order_marker <- order(-marker)
    dup_marker <- duplicated(marker[order_marker])
    m <- cbind(T, delta, marker)[order_marker,]
    n <- nrow(m)

    weights_cases_all <- 1 / weights$IPCW.subjectTimes[order_marker] * n

    ## event
    cases <- m[, "T"] < timepoint & m[, "delta"] == cause
    ## no event til now
    controls1 <- m[, "T"] > timepoint

    weights_cases <- weights_cases_all
    weights_controls1 <- rep( 1 / weights$IPCW.times[2] * n, times = n)

    weights_cases[!cases] <- 0
    weights_controls1[!controls1] <- 0

    m <- sapply(thresholds, function(th) {
        c(TP = sum(weights_cases[m[, "marker"] > th]),
          FP = sum(weights_controls1[m[, "marker"] > th]))
    })
    m / c(sum(weights_cases), sum(weights_controls1))
}

#' Bootstrap Step
#'
#' A single boostrap step for .tpfp.
#'
#' @param T `numeric`, time, see [`timeROC::timeROC()`].
#' @param delta `numeric`, status, see [`timeROC::timeROC()`].
#' @param marker `numeric`, marker, see [`timeROC::timeROC()`].
#' @param thresholds `numeric`, thresholds, see [`timeROC::timeROC()`].
#' @param timepoint `numeric(1)`, timepoint to evaluate.
#' @param cause `numeric`, cause, see [`timeROC::timeROC()`].
#' @return `matrix`, 2 rows (TP and FP), `length(threshold)` columns.
#' @noRd
.bootstep.tpfp <- function(T, delta, marker, timepoint = 365, cause = 1,
                           thresholds) {
    ## stratified bootstrap
    b <- unlist(
        lapply(split(seq_along(delta), delta), sample, replace = TRUE)
    )

    .tpfp(
        T = T[b], delta = delta[b], marker = marker[b], thresholds = thresholds,
        timepoint = timepoint, cause = cause
    )
}

#' Confidence bands
#'
#' Calculate confidence bands for a single timepoint timeROC ROC
#' based on threshold averaging as described in Fawcett 2004.
#'
#' @param T `numeric`, time, see [`timeROC::timeROC()`].
#' @param delta `numeric`, status, see [`timeROC::timeROC()`].
#' @param marker `numeric`, marker, see [`timeROC::timeROC()`].
#' @param timepoint `numeric(1)`, timepoint to evaluate.
#' @param cause `numeric`, cause, see [`timeROC::timeROC()`].
#' @param \ldots further arguments passed to `timeROC`.
#' @param nboot `numeric(1)`, number of boostrap samples.
#' @param conf.level `numeric(1)`, confidence level.
#' @param verbose `logical`, if `TRUE` a progressbar is shown.
#' @return a `list` with 2 elements (TP, FP), each one a `matrix`,
#' with 3 rows (lower/median/upper) band, and  `length(threshold)` columns.
#' @references
#' Fawcett, T. (2004).
#' ROC graphs: Notes and practical considerations for researchers.
#' Machine learning, 31(1), 1-38.
#' @export
confidencebands <- function(T, delta, marker, timepoint, cause = 1, ...,
                            nboot = 200, conf.level = 0.95,
                            verbose = interactive()) {
    conf.level <- (1 - conf.level) / 2
    probs <- c(conf.level, 0.5, 1 - conf.level)
    thresholds <- .thresholds(marker)

    p <- progressr::progressor(nboot, enable = verbose)

    m <- simplify2array(future.apply::future_lapply(
        seq_len(nboot),
        function(i) {
            bs <- .bootstep.tpfp(
                T = T, delta = delta, marker = marker, timepoint = timepoint,
                cause = cause, thresholds = thresholds, ...
            )
            p()
            bs
        },
        future.seed = TRUE
    ))
    ## i: 1:2 (TP, FP), j: 1:length(thresholds), k: 1:nboot
    tp <- apply(m[1,,], 1, quantile, probs = probs)
    fp <- apply(m[2,,], 1, quantile, probs = probs)
    list(TP = tp, FP = fp)
}
