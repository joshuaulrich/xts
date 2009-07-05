\name{[.xts}
\Rdversion{1.1}
\alias{[.xts}
\alias{subset.xts}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extract Subsets of xts Objects
}
\description{
Details on efficient subsetting of \code{xts} objects
for maximum performance and compatibility.
}
\usage{
\method{[}{xts}(x, i, j, drop = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    xts object
}
  \item{i}{
    the rows to extract, numeric, timeBased or ISO-8601 style (see details)
}
  \item{j}{
    the columns to extract, numeric or by name
}
  \item{drop}{
    should dimension be dropped, if possible
}
  \item{\dots}{
    additional arguments (unused)
}
}
\details{
One of the primary motivations, and key points of differentiation
of the time series class xts, is the ability to subset rows
by specifying ISO-8601 compatible character strings.  This allows
for natural range-based time queries without requiring prior
knowledge of the underlying time object used in construction.

When a raw character vector is used for the \code{i}
subset argument, it is processed as if it was ISO-8601
compliant.  This means that it is parsed from left to
right, according to the following specification:

CCYYMMDD HH:MM:SS.ss+

A full description will be expanded from a left-specified
truncated one.

Additionally, one may specify range-based queries
by simply supplying two time descriptions seperated
by a forward slash:

CCYYMMDD HH:MM:SS.ss+/CCYYMMDD HH:MM:SS.ss

Native support for character string subsetting
can be enabled by wrapping your character
time vector with the \code{I} function call.

It is also possible to subset by timeBased objects,
using the same class as used during the object
construction, or as shown by \code{indexClass}.  Mixing class
types in this manner may work, but should not be
relied upon as there will be a severe performance
penalty as well as the increased likelihood
of date or time errors.
}
\value{
An extraction of the original xts object.
}
\references{
ISO 8601: Date elements and interchange formats -
Information interchange - Representation of dates and time
\url{http://www.iso.org}
}
\author{
Jeffrey A. Ryan
}


\seealso{
\code{\link{xts}},
}
\examples{
x <- xts(1:1000, as.Date("2000-01-01")+1:1000)
y <- xts(1:1000, as.POSIXct(as.Date("2000-01-01")+1:1000))

x.subset <- index(x)[1:20]
x[x.subset] # by original index type
x[as.character(x.subset)] # by character string. Beware!

x['200001'] # January 2000
x['1999/2000'] # All of 2000 (note there is no need to use the exact start)
x['1999/200001'] # January 2000 

x['2000/200005'] # 2000-01 to 2000-05
x['2000/2000-04-01'] # through April 01, 2000
y['2000/2000-04-01'] # through April 01, 2000 (using POSIXct series)
}
\keyword{ utilities }
