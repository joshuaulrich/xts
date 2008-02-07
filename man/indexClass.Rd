\name{indexClass}
\alias{indexClass}
\alias{convertIndex}
\alias{indexClass<-}
\title{ Extracting/Replacing the Class of an xts Index }
\description{
Generic functions to extract and replace the class of the index
of an xts object.
}
\usage{
indexClass(x)
indexClass(x) <- value

convertIndex(x,value)
}
\arguments{
  \item{x}{ xts object }
  \item{value}{ desired new class. See details }
}
\details{
It is possible to view and set the class of the time-index
of a given \code{xts} object via the \code{indexClass} function.

The specified format must be a character string containing
one of the following: \code{Date}, \code{POSIXct},
\code{chron}, \code{yearmon}, \code{yearqtr} or \code{timeDate}.

\code{convertIndex} returns a modified \code{xts} object, and
does \emph{not} alter the original.

Changing the index type may alter the behavior of \pkg{xts}
functions expecting a different index, as well as the functionality
of additional methods.  Use with caution.
}
\author{ Jeffrey A. Ryan }
\keyword{ utilities }
