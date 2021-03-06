% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/conversion.R
\name{convert-bases}
\alias{convert-bases}
\alias{deb_convert_bases}
\alias{deb_convert_bases.deb_lsd}
\alias{deb_convert_bases.deb_decimal}
\title{Convert bases of \code{deb_lsd} and \code{deb_decimal} vectors}
\usage{
deb_convert_bases(x, to)

\method{deb_convert_bases}{deb_lsd}(x, to)

\method{deb_convert_bases}{deb_decimal}(x, to)
}
\arguments{
\item{x}{A vector of class \code{deb_lsd} or \code{deb_decimal}.}

\item{to}{Numeric vector of length 2, representing the bases for the
solidus and denarius units to be converted to.}
}
\value{
A vector of the same class as \code{x} with converted \code{bases} attribute.
}
\description{
Convert the bases of the solidus and denarius units of \code{deb_lsd} or
\code{deb_decimal} vectors.
}
\details{
\code{deb_convert_bases()} is the only way to change the bases of the solidus
and denarius units associated with vectors of class \code{deb_lsd} or \code{deb_lsd}.
}
\examples{

x <- deb_lsd(5, 3, 8)
y <- deb_decimal(8.825)

deb_convert_bases(x, to = c(60, 16))
deb_convert_bases(y, to = c(60, 16))

}
