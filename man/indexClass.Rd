\name{indexClass}
\alias{indexClass}
\alias{indexFormat}
\alias{convertIndex}
\alias{indexClass<-}
\alias{indexFormat<-}
\alias{index.xts}
\alias{index.xts<-}
\alias{rindex.xts}
\alias{rindex.xts<-}
\title{ Extracting/Replacing the Class of an xts Index }
\description{
Generic functions to extract, replace, and format the class of the index
of an xts object.
}
\usage{
\method{index}{xts}(x, ...)
\method{index<-}{xts}(x, value)

rindex(x, ...)
rindex(x) <- value

indexClass(x)
indexClass(x) <- value

indexFormat(x)
indexFormat(x) <- value

convertIndex(x,value)
}
\arguments{
  \item{x}{ xts object }
  \item{value}{ desired new class or format. See details }
  \item{\dots}{ additional arguments (unused) }
}
\details{
The main accessor methods to an \code{xts} object's index
is via the \code{index} and \code{index<-} replacement method.
The structure of the index internally is now a numeric
value corresponding to seconds since the epoch (POSIXct converted to numeric).
A call to \code{index} translates to the desired class on-the-fly.

To retrieve the raw numeric data a new accessor function (and replacement) has been
added \code{rindex}. This is primarily for internal use, but may be used
by end-users.

It is possible to view and set the class of the time-index
of a given \code{xts} object via the \code{indexClass} function.

The specified format must be a character string containing
one of the following: \code{Date}, \code{POSIXct},
\code{chron}, \code{yearmon}, \code{yearqtr} or \code{timeDate}.

\code{indexFormat} only manages the manner in which the object
is displayed via \code{print} (also called automatically
when the object is returned).  The valid values
for indexFormat are the same for \code{strptime} and \code{format}.

\code{convertIndex} returns a modified \code{xts} object, and
does \emph{not} alter the original.

Changing the index type may alter the behavior of \pkg{xts}
functions expecting a different index, as well as the functionality
of additional methods.  Use with caution.
}
\author{ Jeffrey A. Ryan }
\keyword{ utilities }
