\name{indexType}
\alias{indexType}
\alias{indexType<-}
\title{ Extracting/Replacing the Class of an xts Index }
\description{
Generic functions to extract and replace the class of the index
of an xts object.
}
\usage{
indexType(x, ...)
indexType(x) <- value
}
\arguments{
  \item{x}{ xts object }
  \item{value}{ desired new class. see details }
  \item{\dots}{ unused }
}
\details{
It is possible to view and set the class of the time-index
of a given \code{xts} object via the \code{indexType} function.

The specified format must be one of \code{Date}, \code{POSIXct},
\code{POSIXlt}, or \code{timeDate}.

Changing the index type may alter the behavior of \pkg{xts}
functions expecting a different index, as well as the functionality
of additional methods.  Use with caution.
}
\author{ Jeffrey A. Ryan }
\keyword{ utilities }
