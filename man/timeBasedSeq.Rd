\name{timeBasedSeq}
\alias{timeBasedSeq}
\alias{timeBasedRange}
\title{ Create a Sequence or Range of Times }
\description{
A function to create a vector of time-based objects
suitable for indexing an \emph{xts} object,
given a string conforming to the ISO 8601 time
and date standard for range-based specification. The
resultant series can be of any class supported by
\emph{xts}, including POSIXct, Date, chron, timeDate,
yearmon, and yearqtr.

\code{timeBasedRange} creates a vector of length 1 or 2
as seconds since the epoch (1970-01-01) for use
internally.
}
\usage{
timeBasedSeq(x, retclass = NULL, length.out = NULL)

timeBasedRange(x, ...)
}
\arguments{
  \item{x}{ a string representing the time-date range desired }
  \item{retclass}{ the return class desired }
  \item{length.out}{ passed to \code{seq} internally }
  \item{\ldots}{ unused }
}
\details{
Designed to provide uniform creation of valid
time-based objects for use within \emph{xts}, the
interface conforms (mostly) to the ISO recommended
format for specifying ranges.

In general, the format is a string specifying
a time and/or date \emph{from}, \emph{to}, and
optionally \emph{by} delineated by
either \sQuote{"/"} or \sQuote{"::"}.

The first argument need not be quoted, as it is
converted internally if need be.

The general form is \emph{from/to/by} or
\emph{from::to::by}, where \emph{to} and \emph{by}
are optional if the length.out arg is specified.

The \code{from} and \code{to} elements of the string
must be left-specified with respect to the standard
\emph{CCYYMMDD HHMMSS} form.  All dates-times
specified will be set to either the earliest point (from)
or the latest (to), given the level of specificity.

For example \sQuote{1999} in the \emph{from} field
would set the start to the beginning of 1999. The opposite
occurs in the \emph{to} field.

The level of detail in the request is interpretted as the
level of detail in the result.  The maximum detail of either
\emph{from} or \emph{to} is the basis of the sequence, unless
the optional \emph{by} element is specified, which will be covered later.

To request a yearly series, it is only necessary to
use \sQuote{"1999/2008"}.  Alternately, one could
request a monthly series (returned by default as
class \code{yearmon}) with \sQuote{"199901/2008"} or
\sQuote{"1999-01/2008"}, or even \sQuote{"1999/2008-01"}.
As the level of granularity increases, so does the
resultant sequence granularity - as does its length.

Using the optional third \emph{by} field (the third
delimited element to the string), will override the
granularity intepretation and return the requested
periodicity.  The acceptable arguments include
\code{Y} for years, \code{m} for months, \code{d} for
days, \code{H} for hours, \code{M} for minutes and \code{S} for
seconds.
}
\value{
A sequence or range of time-based objects.

If \code{retclass} is \code{NULL}, the result is a named
list of from, to, by and length.out.
}
\references{ International Organization for Standardization: ISO 8601
             \url{http://www.iso.org}}
\author{ Jeffrey A. Ryan }
\seealso{ \code{\link{timeBased}}, \code{\link{xts}} }
\examples{
timeBasedSeq('1999/2008')
timeBasedSeq('199901/2008')
timeBasedSeq('199901/2008/d')
timeBasedSeq('20080101 0830',length=100) # 100 minutes
timeBasedSeq('20080101 083000',length=100) # 100 seconds
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ utilities }
