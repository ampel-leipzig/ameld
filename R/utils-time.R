#' Calculate age
#'
#' Calculates the age depending on the day of birth and a given date.
#'
#' @param birth `POSIXct`, day of birth
#' @param to `POSIXct`, reference day
#' @return `integer` age
#' @noRd
#' @examples
#' age(as.POSIXct("2017-09-10"), as.POSIXct("2019-09-10 14:15:00"))
age <- function(birth, to) {
    if (any(birth > to))
        stop("Age can't be negative.")
    ## use "human" years (incl. leap years)
    ## instead of 365 * 24 * 60 * 60 seconds (would be `dyears`)
    as.integer(floor(lubridate::interval(birth, to) / lubridate::years()))
}

#' Calculate daysAtRisk
#'
#' Calulate days between last follow up or LTx whatever happens first.
#'
#' @param ref `POSIXct`, reference day
#' @param last `POSIXct`, last follow up day
#' @param ltx `POSIXct`, ltx day
#' @param reltx `POSIXct`, reltx day
#' @return `integer` days
#' @noRd
#' @examples
#' daysAtRisk(as.POSIXct("2019-09-10"), as.POSIXct("2019-09-13"))
daysAtRisk <- function(ref, last, ltx = as.POSIXct(NA), reltx = as.POSIXct(NA)) {
    ltxBeforeRef <- !is.na(reltx) & !is.na(ltx) &
        ltx < ref & (reltx + lubridate::days()) >= ref
    ltx[ltxBeforeRef] <- reltx[ltxBeforeRef]

    ltxValid <- !is.na(ltx) & (ltx + lubridate::days()) >= ref
    last[ltxValid] <- ltx[ltxValid]

    as.integer(ceiling(lubridate::interval(ref, last) / lubridate::days()))
}
