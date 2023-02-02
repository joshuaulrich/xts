\name{tclass}
\alias{tclass}
\alias{tclass<-}
\alias{tclass.xts}
\alias{tclass<-.xts}
\alias{indexClass}
\alias{indexClass<-}
\title{ Get or Replace the Class of an xts Object's Index }
\description{
Generic functions to get or replace the class of an xts object's index.
}
\usage{
tclass(x, \dots)
tclass(x) <- value

\method{tclass}{xts}(x, \dots)
\method{tclass}{xts}(x) <- value

##### The functions below are DEPRECATED #####
indexClass(x)
indexClass(x) <- value
}
\arguments{
  \item{x}{ an \code{xts} object }
  \item{value}{ new index class (see Details for valid values) }
  \item{\dots}{ arguments passed to other methods }
}
\details{
Internally, an xts object's index is a \emph{numeric} value corresponding to
seconds since the epoch in the UTC timezone. The index class is stored as the
\code{tclass} attribute on the internal index. This is used to convert the
internal index values to the desired class when the \code{index} function is
called.

The \code{tclass} function retrieves the class of the internal index, and the
\code{tclass<-} function sets it. The specified value for \code{tclass<-} must
be one of the following character strings: \code{"Date"}, \code{"POSIXct"},
\code{"chron"}, \code{"yearmon"}, \code{"yearqtr"}, or \code{"timeDate"}.
}
\value{
A vector containing the class of the object's index.
}
\note{
Both \code{indexClass} and \code{indexClass<-} are deprecated in favor of
\code{tclass} and \code{tclass<-}, respectively.

Replacing the \code{tclass} \emph{does not} change the values of the internal
index. See the examples.
}
\seealso{
\code{\link{index}} has more information on the xts index, \code{\link{tformat}}
details how the index values are formatted when printed, \code{\link{tzone}}
has more information about the index timezone settings.

The following help pages describe the characteristics of the valid index
classes: \code{\link{POSIXct}}, \code{\link{Date}}, \code{\link[chron]{chron}},
\code{\link[zoo]{yearmon}}, \code{\link[zoo]{yearqtr}},
\code{\link[timeDate]{timeDate}}.
}
\author{ Jeffrey A. Ryan }
\examples{
x <- timeBasedSeq('2010-01-01/2010-01-02 12:00')
x <- xts(seq_along(x), x)


y <- timeBasedSeq('2010-01-01/2010-01-03 12:00/H')
y <- xts(seq_along(y), y, tzone = "America/New_York")

# Changing the tclass does not change the internal index values, but it
# does change how the index is printed!
head(y)    # the index has times
.index(y)
tclass(y) <- "Date"
head(y)    # the index prints without times, but
.index(y)  # the internal index is not changed!
}
\keyword{ts}
\keyword{utilities}
