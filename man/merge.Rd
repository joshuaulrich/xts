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
merge.xts(...,
          all = TRUE,
          fill = NA,
          suffixes = NULL,
          join = "outer",
          retside = TRUE,
          retclass = "xts")
}
\arguments{
  \item{\dots}{ one or more xts objects, or objects coercible to class xts }
  \item{all}{ a logical vector indicating merge type }
  \item{fill}{ values to be used for missing elements }
  \item{suffixes}{ to be added to merged column names }
  \item{join}{ type of database join }
  \item{retclass}{ object to return }
}
\details{
Implemented almost entirely in custom C-level code,
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

Note that the \code{all} and \code{join} arguments imply a two case scenario.  For merging
more than two objects, they will simply fall back to a full outer, or full inner join, as
left and right can be ambiguous with respect to sides.

To do something along the lines of merge.zoo's method of joining based on
an all argument of the same length of the arguments to join, see the example.  A solution
to provide this automatically is in development.

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

}
\examples{
(x <- xts(4:10, Sys.Date()+4:10))
(y <- xts(1:6, Sys.Date()+1:6))

merge(x,y)
merge(x,y, join='inner')
merge(x,y, join='left')
merge(x,y, join='right')

merge.zoo(zoo(x),zoo(y),zoo(x), all=c(TRUE, FALSE, TRUE))
merge(merge(x,x),y,join='left')[,c(1,3,2)]
}
\keyword{ manip }
\keyword{ utilities }
