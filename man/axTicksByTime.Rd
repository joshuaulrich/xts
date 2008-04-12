\name{axTicksByTime}
\alias{axTicksByTime}

\title{ Compute x-Axis Tickmark Locations by Time }
\description{
Compute x-axis tickmarks like \code{axTicks} in base
but with respect to time.  Additionally the first
argument is the object indexed by time which you
are looking to derive tickmark locations for.

It is possible to specify the detail you are seeking,
or by passing 'auto' to the \code{ticks.on} argument,
to get a best heuristic fit.
}
\usage{
axTicksByTime(ival, ticks.on='auto', k = 1,
              labels=TRUE, ends=TRUE, gt = 2, lt = 30)
}
\arguments{
  \item{ival}{ the object indexed by time }
  \item{ticks.on}{ what to break on }
  \item{k}{ frequency of breaks }
  \item{labels}{ should a labeled vector be returned }
  \item{ends}{ should the ends be adjusted }
  \item{gt}{ lower bound on number of breaks }
  \item{lt}{ upper bound on number of breaks }
}
\details{
This function is written for internal use, and documented
for those wishing to use outside of the internal function
uses. In general it is most unlikely that the end user will
call this function directly.
}
\value{
A vector of index points to break on, possibly with the index names.
}
\author{ Jeffrey A. Ryan }
\seealso{ \code{\link{endpoints}} }
\examples{
data(sample_matrix)
axTicksByTime(as.xts(sample_matrix),'auto')
axTicksByTime(as.xts(sample_matrix),'weeks')
axTicksByTime(as.xts(sample_matrix),'months',7)
}
\keyword{ utilities }
