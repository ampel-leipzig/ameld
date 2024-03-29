% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-predict.R
\name{cutpoints}
\alias{cutpoints}
\title{Calculate cutpoints/breaks}
\usage{
cutpoints(x, n = 50L)
}
\arguments{
\item{x}{\code{double} predicted survival}

\item{n}{\code{integer(1)} number of patients/observation per interval}
}
\value{
\code{double}, cutpoints
}
\description{
Calculate cutpoints/breaks for \code{cut} to have an equal number of
patients/observations per group.
}
\examples{
x <- seq(0, 1, length.out = 10)
cutpoints(x, n = 2)
}
