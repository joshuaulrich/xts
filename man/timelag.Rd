\name{timelag}
\alias{timelag}
\title{
Time lags of an xts Object
}
\description{
Lag an \code{xts} object by \code{k} seconds.
}
\usage{
timelag(x, k = 1, na.pad = TRUE)
}
\arguments{
  \item{x}{ an \code{xts} object }
  \item{k}{ number of seconds to lag }
  \item{na.pad}{ pad vector back to original size }
}
\details{
This is similar to \code{lag.xts}, but the key difference is shifting by units 
of time (e.g. seconds) rather than a discrete index. This is particularly useful
for irregular time series such as high frequency market data. This function 
allows one to answer the question, "For a given observation at time \sQuote{t}, 
what is the value \code{k} seconds from time \sQuote{t}?". For a positive 
\code{k}, the value at time \sQuote{t} should be the value at time 
\sQuote{t - k seconds}. This matches the behavior of \code{lag.xts}.
}
\value{
An \code{xts} object with values lagged by \code{k} seconds.
}
\author{
Ross Bennett
}

\seealso{
\code{\link{lag.xts}}
}
\examples{
# same as lag for a regularly spaced time series
x <- .xts(1:5, 1:5 + 0.0)
lag(x, k = 1)
timelag(x, k = 1)

idx <- c(1, 2, 2.4, 2.5, 4, 4.2, 4.5, 4.6, 5.2, 6)
x <- .xts(matrix(1:10, ncol = 1), idx, dimnames = list(NULL, "p1"))
# value of p1 2 seconds in the future
x$p1.future <- timelag(x, -2)
# value of p1 2 seconds in the past
x$p1.past <- timelag(x, 2)
}
