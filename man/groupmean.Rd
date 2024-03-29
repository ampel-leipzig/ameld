% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-base.R
\name{groupmean}
\alias{groupmean}
\title{Groupwise mean}
\usage{
groupmean(x, f, na.rm = TRUE)
}
\arguments{
\item{x}{\code{double}, values}

\item{f}{\code{factor}, splitting factor/grouping variable.}

\item{na.rm}{\code{logical(1)}, see \code{\link[=mean]{mean()}} for details.}
}
\value{
Name \code{double} with mean values per split/group.
}
\description{
This function calculates the groupwise mean.
}
\examples{
groupmean(1:9, rep(1:3, 3))
}
