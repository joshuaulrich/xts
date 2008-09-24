\name{diff.xts}
\alias{diff.xts}
\alias{lag.xts}
\title{ Lags and Differences of xts Objects }
\description{
Methods for computing lags and differences on \code{xts} objects.  This
matches most of the functionality of \pkg{zoo} methods, with some default
argument changes.
}
\usage{
\method{lag}{xts}(x, k = 1, na.pad = TRUE, ...)

\method{diff}{xts}(x, lag = 1, differences = 1, arithmetic = TRUE, log = FALSE, na.pad = TRUE, ...)
}
\arguments{
  \item{x}{ an \code{xts} object }
  \item{lag}{ period to lag or difference over }
  \item{differences}{ order of differencing }
  \item{arithmetic}{ should arithmetic or geometric differencing be used }
  \item{log}{ should (geometric) log differences be returned }
  \item{na.pad}{ pad vector back to original size }
  \item{\dots}{ additional arguments }
}
\details{
The primary motivation for having methods specific to \code{xts}
was to make use of faster C-level code within xts.  Additionally,
it was decided that \code{lag}'s default behavior should
match the common time-series interpretation of that operator --- specifically
that a value at time \sQuote{t} should be the value at time \sQuote{t-1}
for a positive lag. This is different than \code{lag.zoo} as well
as \code{lag.ts}.

Another notable difference is that \code{na.pad} is set to
TRUE by default, to better reflect the transformation visually
and within functions requiring positional matching of data.
}
\value{
An \code{xts} object reflected the desired lag and/or differencing.
}
\references{ \url{http://en.wikipedia.org/wiki/Lag }
\author{ Jeffrey A. Ryan }
\examples{
x <- xts(1:10, Sys.Date()+1:10)
lag(x)
lag(x, k=2)

lag(x, k=-1, na.pad=FALSE) # matches lag.zoo(x, k=1)

diff(x)
diff(x, lag=1)
diff(x, diff=2)
diff(diff(x))
}
\keyword{ manip }% __ONLY ONE__ keyword per line
\keyword{ chron }% __ONLY ONE__ keyword per line
