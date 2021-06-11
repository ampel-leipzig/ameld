#' Convert laboratory metric to SI units
#'
#' Converts some laboratory metric to SI units.
#'
#' @param x `numeric`, values to convert
#' @param type `character`, type of laboratory value
#' @return `numeric`
#'
#' @rdname si
#' @export
#' @examples
#' as_si(4.2, "creatinine")
as_si <- function(x, type = c("bilirubin", "creatinine")) {
    type <- match.arg(type)
    x * switch(type,
        "bilirubin" = 17.1,
        "creatinine" = 88.4
    )
}

#' @rdname si
#'
#' @export
#' @examples
#' as_metric(175, "creatinine")
as_metric <- function(x, type = c("bilirubin", "creatinine")) {
    x / as_si(1L, type)
}
