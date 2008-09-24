\name{merge.xts}
\alias{merge.xts}
\alias{cbind.xts}
\title{ Merge xts Objects }
\description{
Used to perform fast merging operation on \code{xts} objects.  Given the
inherent ordered nature of \code{xts} time-series, a
merge-join style merge allows for optimally efficient
joins.
}
\usage{
merge.xts(x,
          y,
          ...,
          all = TRUE,
          fill = NA,
          suffixes = NULL,
          join = "outer",
          retclass = "xts")
}
\arguments{
  \item{x}{ an \sQuote{xts} object }
  \item{y}{ an object coercible to \sQuote{xts} }
  \item{\dots}{ additional objects to be merged }
  \item{all}{ a logical vector indicating merge type }
  \item{fill}{ values to be used for missing elements }
  \item{suffixes}{ to be added to merged column names }
  \item{join}{ type of database join }
  \item{retclass}{ object to return }
}
\details{
Implemented entirely in custom C-level code,
it is possible using either the \code{all} argument or
the \code{join} argument to implement all common
database join operations: \sQuote{outer} (full outer - all rows), 
\sQuote{inner} (only rows with common indexes),
\sQuote{left} (all rows in the left object, and those that match in the right),
and \sQuote{right} (all rows in the right object, and those that match in the left).

The above join types can also be expressed as a vector of
logical values passed to \code{all}.  c(TRUE,TRUE) or TRUE for \sQuote{join="outer"},
c(FALSE,FALSE) or FALSE for \sQuote{join="inner"}, c(TRUE, FALSE) for \sQuote{join="left"},
and c(FALSE,TRUE) for \sQuote{join="right"}.

If \code{retclass} is \code{NULL}, the joined objects will be split
and reassigned silently back to the original environment they are called
from.  This is for backward compatibility with zoo, though unused
by xts.
}
\value{
A new \code{xts} object containing the appropriate
elements of the objects passed in to be merged.
}
\references{ Merge Join Discussion: \url{http://blogs.msdn.com/craigfr/archive/2006/08/03/687584.aspx} }
\author{ Jeffrey A. Ryan }
\note{
This is a highly optimized merge, specifically designed for ordered data.
At present the \ldots argument is used in a recursive style loop
structure, and as such is not optimized so highly.  While on merges
of two objects speed and memory improvements on the order of
100x over zoo are possible, merging 100's of objects causing
significant performance loss.
}
\examples{
(x <- xts(1:10, Sys.Date()+1:10))
(y <- x[1:5])

merge(x,y)
merge(x,y, join='inner')
merge(x,y, join='left')
merge(x,y, join='right')
}
\keyword{ manip }
\keyword{ utilities }
