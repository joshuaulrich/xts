\name{endpoints}
\alias{endpoints}
\title{ Locate Endpoints by Time }
\description{
Extract index locations for an \code{xts} object that correspond to the
\emph{last} observation in each period specified by \code{on}.
}
\usage{
endpoints(x, on="months", k=1)
}
\arguments{
  \item{x}{ an xts object }
  \item{on}{ the periods endpoints to find as a character string }
  \item{k}{ along every k-th element - see notes }
}
\details{
\code{endpoints} returns a numeric vector corresponding to the \emph{last}
observation in each period. The vector always begins with zero and ends with
the last observation in \code{x}.

Periods are always based on the distance from the UNIX epoch (midnight
1970-01-01 UTC), \emph{not the first observation in \code{x}}. The examples
illustrate this behavior.

Valid values for the argument \code{on} include: \dQuote{us} (microseconds),
\dQuote{microseconds}, \dQuote{ms} (milliseconds), \dQuote{milliseconds},
\dQuote{secs} (seconds), \dQuote{seconds}, \dQuote{mins} (minutes),
\dQuote{minutes}, \dQuote{hours}, \dQuote{days}, \dQuote{weeks},
\dQuote{months}, \dQuote{quarters}, and \dQuote{years}.
}
\value{
A numeric vector of beginning with 0 and ending with the value equal to the
number of observations in the \code{x} argument.
}
\author{ Jeffrey A. Ryan }
\examples{
data(sample_matrix)

endpoints(sample_matrix)
endpoints(sample_matrix, "weeks")

### example of how periods are based on the UNIX epoch,
### *not* the first observation of the data series
x <- xts(1:38, yearmon(seq(2018 - 1/12, 2021, 1/12)))
# endpoints for the end of every other year
ep <- endpoints(x, "years", k = 2)
# Dec-2017 is the end of the *first* year in the data. But when you start from
# Jan-1970 and use every second year end as your endpoints, the endpoints are
# always December of every odd year.
x[ep, ]

}
\keyword{ utilities }
