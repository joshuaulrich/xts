\name{[.xts}
\Rdversion{1.1}
\alias{[.xts}
\alias{subset.xts}
\alias{.subset.xts}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extract Subsets of xts Objects
}
\description{
Details on efficient subsetting of \code{xts} objects
for maximum performance and compatibility.
}
\usage{
\method{[}{xts}(x, i, j, drop = FALSE, which.i=FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    xts object
}
  \item{i}{
    the rows to extract. Numeric, timeBased or ISO-8601 style (see details)
}
  \item{j}{
    the columns to extract, numeric or by name
}
  \item{drop}{
    should dimension be dropped, if possible. See NOTE.
}
  \item{which.i}{
    return the \sQuote{i} values used for subsetting. No
subset will be performed.
}
  \item{\dots}{
    additional arguments (unused)
}
}
\details{
One of the primary motivations, and key points of differentiation
of the time series class xts, is the ability to subset rows
by specifying ISO-8601 compatible range strings.  This allows
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

The algorithm to parse the above is \code{.parseISO8601} from
the \pkg{xts} package.

ISO-style subsetting, given a range type query, makes use
of a custom binary search mechanism that allows for
very fast subsetting as no linear search though the index
is required.  ISO-style character vectors may be longer than
length one, allowing for multiple non-contiguous ranges
to be selected in one subsetting call.

If a character \emph{vector} representing time is used in place of 
numeric values, ISO-style queries, or timeBased
objects, the above parsing will be carried out on
each element of the i-vector.  This overhead can
be very costly. If the character approach is used when
no ISO range querying is needed, it is
recommended to wrap the \sQuote{i} character vector with the \code{I()}
function call, to allow for more efficient internal processing.
Alternately converting character vectors to POSIXct objects will
provide the most performance efficiency.

As \code{xts} uses POSIXct time representations
of all user-level index classes internally, the fastest
timeBased subsetting will always be from POSIXct objects,
regardless of the \code{tclass} of the original
object.  All non-POSIXct time classes
are converted to character first to preserve
consistent TZ behavior.
}
\value{
An extraction of the original xts object.  If \code{which.i}
is TRUE, the corresponding integer \sQuote{i} values used to
subset will be returned.
}
\note{
By design, drop=FALSE in the default case.  This preserves the basic
underlying type of \code{matrix} and the \code{dim()} to be non-NULL.
This is different from both matrix and \code{zoo} behavior as \R
uses \code{drop=TRUE}.  Explicitly passing \code{drop=TRUE} may
be required when performing certain matrix operations.
}
\references{
ISO 8601: Date elements and interchange formats -
Information interchange - Representation of dates and time
\url{https://www.iso.org}
}
\author{
Jeffrey A. Ryan
}
\seealso{
\code{\link{xts}},
\code{\link{.parseISO8601}},
\code{\link{.index}}
}
\examples{
x <- xts(1:3, Sys.Date()+1:3)
xx <- cbind(x,x)

# drop=FALSE for xts, differs from zoo and matrix
z <- as.zoo(xx)
z/z[,1]

m <- as.matrix(xx)
m/m[,1]

# this will fail with non-conformable arrays (both retain dim)
tryCatch(
  xx/x[,1], 
  error=function(e) print("need to set drop=TRUE")
)

# correct way
xx/xx[,1,drop=TRUE]

# or less efficiently
xx/drop(xx[,1])
# likewise
xx/coredata(xx)[,1]


x <- xts(1:1000, as.Date("2000-01-01")+1:1000)
y <- xts(1:1000, as.POSIXct(format(as.Date("2000-01-01")+1:1000)))

x.subset <- index(x)[1:20]
x[x.subset] # by original index type
system.time(x[x.subset]) 
x[as.character(x.subset)] # by character string. Beware!
system.time(x[as.character(x.subset)]) # slow!
system.time(x[I(as.character(x.subset))]) # wrapped with I(), faster!

x['200001'] # January 2000
x['1999/2000'] # All of 2000 (note there is no need to use the exact start)
x['1999/200001'] # January 2000 

x['2000/200005'] # 2000-01 to 2000-05
x['2000/2000-04-01'] # through April 01, 2000
y['2000/2000-04-01'] # through April 01, 2000 (using POSIXct series)


### Time of day subsetting 

i <- 0:60000
focal_date <- as.numeric(as.POSIXct("2018-02-01", tz = "UTC"))
x <- .xts(i, c(focal_date + i * 15), tz = "UTC", dimnames = list(NULL, "value"))

# Select all observations between 9am and 15:59:59.99999:
w1 <- x["T09/T15"] # or x["T9/T15"]
head(w1)

# timestring is of the form THH:MM:SS.ss/THH:MM:SS.ss

# Select all observations between 13:00:00 and 13:59:59.9999 in two ways:
y1 <- x["T13/T13"]
head(y1)

x[.indexhour(x) == 13]

# Select all observations between 9:30am and 30 seconds, and 4.10pm:
x["T09:30:30/T16:10"]

# It is possible to subset time of day overnight.
# e.g. This is useful for subsetting FX time series which trade 24 hours on week days

# Select all observations between 23:50 and 00:15 the following day, in the xts time zone
z <- x["T23:50/T00:14"]
z["2018-02-10 12:00/"] # check the last day


# Select all observations between 7pm and 8.30am the following day:
z2 <- x["T19:00/T08:29:59"]
head(z2); tail(z2)


}
\keyword{ utilities }
