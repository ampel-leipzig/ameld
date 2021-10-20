#' Create (balanced) CV folds
#'
#' @param y `factor`, classes
#' @param nfolds `integer(1)`, number of folds
#' @param balanced `logical`, should classes be balanced in the folds?
#' @noRd
.bfolds <- function(y, nfolds = 3L) {
    grpn <- table(y)

    if (nfolds < 3L)
        stop("'nfolds' has to be >= 3.")
    if (any(nfolds > grpn))
        warning("'nfolds' > than the groups, reducing to minimal group size.")

    nfolds <- min(nfolds, grpn)
    s <- seq_len(nfolds)
    unlist(lapply(grpn, function(n)sample(rep_len(s, n))), use.names = FALSE)
}

#' Create CV folds
#'
#' @param y `factor`, classes
#' @param nfolds `integer(1)`, number of folds
#' @param balanced `logical`, should classes be balanced in the folds?
#' @noRd
.folds <- function(y, nfolds = 3L) {
    if (nfolds < 3L)
        stop("'nfolds' has to be >= 3.")
    sample(rep_len(seq_len(nfolds), length(y)))
}

#' Convert s into its numeric equivalent
#'
#' This function converts s/lambda to its numeric equivalent.
#'
#' @param object `cv.glmnet`, object.
#' @param s `numeric`/`character`.
#' @return `numeric`:
#' @importFrom methods is
#' @noRd
.s2numeric <- function(object, s = c("lambda.1se", "lambda.min")) {
    if(!is(object, "cv.glmnet"))
        stop("'object' has to be an 'cv.glmnet' object.")
    if (is.character(s))
        object[[match.arg(s)]]
    else
        s
}
