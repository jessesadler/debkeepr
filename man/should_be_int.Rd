% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{should_be_int}
\alias{should_be_int}
\title{Deal with floating point problems}
\usage{
should_be_int(x, tol = .Machine$double.eps^0.5)
}
\description{
Should the value be a whole number or should it have a decimal value.
Used in \code{decimal_check()}.
}
\keyword{internal}
