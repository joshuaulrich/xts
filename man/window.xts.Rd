\name{window.xts}
\Rdversion{1.1}
\alias{window.xts}

\title{Extract time windows from an \code{xts} series}
\description{
Method for extracting time windows from \code{xts} objects.
}

\usage{
\method{window}{xts}(x, index. = NULL, start = NULL, end = NULL, \dots)
}

\arguments{
  \item{x}{an object.}
  \item{index.}{a user defined time index. This defaults to the \code{xts}
    index for the series via \code{.index(x)}. When supplied, this is typically
    a subset of the dates in the full series.\cr
    The \code{index.} must be a set of dates that are convertible to
    \code{POSIXct}. If you want fast lookups, then \code{index.} should be
    sorted and of class \code{POSIXct}.\cr
    If an unsorted \code{index.} is passed in, \code{window} will sort it.}
  \item{start}{a start time. Extract \code{xts} rows where \code{index. >=
    start}. \code{start} may be any class that is convertible to
    \code{POSIXct} such as a character variable in the format
    \sQuote{YYYY-MM-DD}.\cr
    If \code{start} is \code{NULL} then all \code{index.} dates are matched.}
  \item{end}{an end time. Extract \code{xts} rows where \code{index. <= end}.
    \code{end} must be convertible to \code{POSIXct}. If \code{end} is
    \code{NULL} then all \code{index.} dates are matched.}
  \item{\dots}{currently not used.}
}

\value{
The matching time window is extracted.
}

\details{
The point of having \code{window} in addition to the regular subset function
is to have a fast way of extracting time ranges from an \code{xts} series. In
particular, this method will convert \code{start} and \code{end} to
\code{POSIXct} then do a binary lookup on the internal \code{xts} index to
quickly return a range of matching dates. With a user supplied \code{index.},
a similarly fast invocation of \code{findInterval} is used so that large sets
of sorted dates can be retrieved quickly.
}

\author{ Corwin Joy }

\seealso{
\code{\link{subset.xts}},
\code{\link[base]{findInterval}},
\code{\link{xts}}
}

\examples{
## xts example
x.date <- as.Date(paste(2003, rep(1:4, 4:1), seq(1,19,2), sep = "-"))
x <- xts(matrix(rnorm(20), ncol = 2), x.date)
x

window(x, start = "2003-02-01", end = "2003-03-01")
window(x, start = as.Date("2003-02-01"), end = as.Date("2003-03-01"))
window(x, index = x.date[1:6], start = as.Date("2003-02-01"))
window(x, index = x.date[c(4, 8, 10)])

## Assign to subset
window(x, index = x.date[c(4, 8, 10)]) <- matrix(1:6, ncol = 2)
x
}
\keyword{ts}
