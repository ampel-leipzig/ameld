% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rcv.glmnet.R
\name{rcv.glmnet}
\alias{rcv.glmnet}
\title{Repeated Cross-Validation for cv.glmnet}
\usage{
rcv.glmnet(
    x, y,
    lambda = NULL, alpha = 1,
    nrepcv = 100L, nfolds = 10L, foldid = NULL, balanced = FALSE,
    ...,
    trace.it = interactive()
)
}
\arguments{
\item{x}{\code{matrix}, as in \code{cv.glmnet}.}

\item{y}{response as in \code{cv.glmnet}.}

\item{lambda}{\code{numeric}, optional user-supplied lambda sequence; default is
\code{NULL} and \code{glmnet} chooses its own sequence.}

\item{alpha}{\code{numeric(1)}, the elasticnet mixing parameter, default is \code{1}
(lasso penality); see \code{\link[glmnet:glmnet]{glmnet::glmnet()}} for details.}

\item{nrepcv}{\code{integer(1)}, number of repeated cross-validations (outer
loop).}

\item{nfolds}{\code{integer}, number of folds, same as in \code{cv.glmnet}.}

\item{foldid}{\code{matrix}, an optional matrix with \code{nrepcv} rows and
\code{nrow(x)} columns containing ids from 1 to \code{nfolds} identifying what fold
each observation is in. If given \code{nrepcv} and \code{nfolds} are ignored.}

\item{balanced}{\code{logical}, should classes/status be balanced in the folds
(default: FALSE)?}

\item{\dots}{further arguments passed to \code{cv.glmnet}.}

\item{trace.it}{\code{integer}, if \code{trace.it = 1}, then a progress bar is
displayed.}
}
\value{
An object of class \code{rcv.glmnet} that extends the \code{cv.glmnet}
class.
}
\description{
This functions returns the best lambda of repeated \code{\link[glmnet:cv.glmnet]{glmnet::cv.glmnet()}}
calls.
}
\examples{
# Examples taken from ?"glmnet::cv.glmnet"
set.seed(1010)
n <- 1000
p <- 100
nzc <- trunc(p/10)
x <- matrix(rnorm(n * p), n, p)
beta <- rnorm(nzc)
fx <- x[, seq(nzc)] \%*\% beta
eps <- rnorm(n) * 5
y <- drop(fx + eps)
set.seed(1011)
# nrepcv should usually be higher but to keep the runtime of the example low
# we choose 2 here
rcvob <- rcv.glmnet(x, y, nrepcv = 2, nfolds = 3)
plot(rcvob)
title("Gaussian Family", line = 2.5)
coef(rcvob)
predict(rcvob, newx = x[1:5, ], s = "lambda.min")
}
\references{
Jerome Friedman, Trevor Hastie, Robert Tibshirani (2010).
Regularization Paths for Generalized Linear Models via Coordinate
Descent. Journal of Statistical Software, 33(1), 1-22. URL
\url{https://www.jstatsoft.org/v33/i01/}.

Noah Simon, Jerome Friedman, Trevor Hastie, Rob Tibshirani (2011).
Regularization Paths for Cox's Proportional Hazards Model via
Coordinate Descent. Journal of Statistical Software, 39(5), 1-13. URL
\url{https://www.jstatsoft.org/v39/i05/}.
}
\seealso{
\code{\link[glmnet:cv.glmnet]{glmnet::cv.glmnet()}}
}
\author{
Sebastian Gibb
}
