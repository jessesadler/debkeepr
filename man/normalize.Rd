% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lsd-normalize.R
\name{normalize}
\alias{normalize}
\alias{deb_normalize}
\alias{deb_normalize.default}
\alias{deb_normalize.deb_lsd}
\alias{deb_normalize.numeric}
\title{Normalization function}
\usage{
normalize(l, s, d, bases)

deb_normalize(x, ...)

\method{deb_normalize}{default}(x, ...)

\method{deb_normalize}{deb_lsd}(x, ...)

\method{deb_normalize}{numeric}(x, bases = c(20, 12), ...)
}
\arguments{
\item{bases}{Used only if \code{x} is a numeric vector. A Numeric vector of
length 2 used to specify the bases for the shillings or s and pence or
d units. Default is \code{c(20, 12)}, which conforms to the most widely used
system of 1 pound = 20 shillings and 1 shilling = 12 pence.}

\item{x}{Either an vector of class \code{deb_lsd} or a numeric vector of
length 3 representing the values to be normalized.}

\item{...}{Arguments passed on to further methods.}
}
\value{
Returns a vector of class \code{deb_lsd} with normalized solidus and
denarius units.
}
\description{
Function that actually performs the normalization of lsd value

Normalize pounds, shillings, and pence values to given bases of solidus
and denarius units.
}
\examples{

# Normalize a deb_lsd vector
x <- deb_lsd(12, 93, 78)
y <- deb_lsd(12, 93, 78, bases = c(60, 16))
deb_normalize(x)
deb_normalize(y)

# Normalize a numeric vector of length 3
deb_normalize(c(12, 93, 78), bases = c(20, 12))
deb_normalize(c(12, 93, 78), bases = c(60, 16))

}
\keyword{internal}
