\name{endpoints}
\alias{endpoints}
\title{ Locate Endpoints by Time }
\description{
Extract index values of a given \code{xts} object corresponding
to the \emph{last} observations given a period specified by \code{on}
}
\usage{
endpoints(x, on="months", k=1)
}
\arguments{
  \item{x}{ an xts object }
  \item{on}{ the periods endpoints to find as a character string }
  \item{k}{ the number of aggregation points??? }
}
\details{
\code{endpoints} returns a numeric vector corresponding to the
\emph{last} observation in each period specified by \code{on}, with a
zero added to the beginning of the vector, and the index of the last
observation in \code{x} at the end.

Valid values for the argument \code{on} include: \dQuote{secs} (seconds),
\dQuote{seconds}, \dQuote{mins} (minutes), \dQuote{minutes},
\dQuote{hours}, \dQuote{days}, \dQuote{weeks}, \dQuote{months}, \dQuote{quarters},
and \dQuote{years}.
}
\value{
A numeric vector of endpoints.
}
\author{ Jeffrey A. Ryan }
\examples{
}
\keyword{ utilities }
