#' Groupwise mean
#'
#' This function calculates the groupwise mean.
#'
#' @param x `double`, values
#' @param f `factor`, splitting factor/grouping variable.
#' @param na.rm `logical(1)`, see [mean()] for details.
#' @return Name `double` with mean values per split/group.
#' @export
#' @examples
#' groupmean(1:9, rep(1:3, 3))
groupmean <- function(x, f, na.rm = TRUE) {
    vapply(split(x, f), mean, NA_real_, na.rm = na.rm)
}
