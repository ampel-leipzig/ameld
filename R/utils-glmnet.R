#' Create balanced CV folds
#'
#' @param y `factor`, classes
#' @param nfolds `integer(1)`, number of folds
#' @return integer(length(y))
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
#' @return integer(length(y))
#' @noRd
.folds <- function(y, nfolds = 3L) {
    if (nfolds < 3L)
        stop("'nfolds' has to be >= 3.")
    sample(rep_len(seq_len(nfolds), length(y)))
}

#' Create (balanced) CV fold id matrix
#'
#' @param y `factor`, classes
#' @param nfolds `integer(1)`, number of folds
#' @param nrep `integer(1)`, repititions
#' @param balanced `logical(1)`, balanced folds?
#' @return matrix, nrows == nrep, ncol == length(y)
#' @noRd
.mfolds <- function(y, nfolds = 3L, nrep = 5L, balanced = FALSE) {
    f <- if (isTRUE(balanced)) .bfolds else .folds
    do.call(rbind, lapply(integer(nrep), function(i)f(y, nfolds = nfolds)))
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

#' find selected variables in a list of glmnet models
#' @param x `list` of glmnet objects.
#' @param s `character`/`numeric`, value(s) of the penality parameter `lambda`.
#' See [`glmnet::predict.cv.glmnet()`] for details.
#' @return `table` of selected variables
#' @noRd
.selvars <- function(x, s = "lambda.1se") {
    table(unlist(lapply(x, function(m) {
        fit <- m$fit
        rownames(fit$glmnet.fit$beta)[
            predict(
                fit$glmnet.fit,
                type = "nonzero",
                s = .s2numeric(fit, s)
            )[, 1L]
        ]
    })))
}
