\name{periodicity}
\alias{periodicity}
\title{ Approximate Series Periodicity }
\description{
Estimate the periodicity of a time-series-like
object by calculating the median time between
observations in days.
}
\usage{
periodicity(x, ...)
}
\arguments{
  \item{x}{ time-series-like object }
  \item{\dots}{ unused }
}
\details{
A simple wrapper to quickly estimate the periodicity
of a given data. Returning an object of type \code{periodicity}.

This calculates the
median number of days between observations
as a difftime object, the numerical difference,
the units of measurement,
and the derived scale of the data as a string. 

The time index currently must be of either \code{Date}
or \code{POSIX} class, or coercible to such. 

The only list item of note is the \code{scale}.
This is an estimate of the periodicity of the data
in common terms - e.g. 7 day dialy data is best
described as \sQuote{weekly}, and would be returned as
such.

Possible \code{scale} values are:

\sQuote{minute},\sQuote{hourly},
\sQuote{daily},\sQuote{weekly},
\sQuote{monthly},\sQuote{quarterly},
and \sQuote{yearly}.
}
\value{
An object containing a list containing the \code{difftime} object,
frequency, units, and suitable scale.
}
\note{
This function is only a \emph{good estimate} for
the underlying periodicity.  If the series is too
short, or has \emph{no} real periodicity, the return
values will obviously be wrong. That said, it is quite
robust and used internally within \pkg{xts}.
}
\author{ Jeffrey A. Ryan }
\seealso{ \code{\link{difftime}} }
\examples{
zoo.ts <- zoo(rnorm(231),as.Date(13514:13744,origin="1970-01-01"))
periodicity(zoo.ts)
}
\keyword{ utilities }
