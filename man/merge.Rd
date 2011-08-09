\name{merge.xts}
\alias{merge.xts}
\alias{cbind.xts}
\title{ Merge xts Objects }
\description{
Used to perform merge operation on \code{xts} objects
by \emph{time} (index).  Given the
inherent ordered nature of \code{xts} time-series, a
merge-join style merge allows for optimally efficient
joins.
}
\usage{
\method{merge}{xts}(...,
      all = TRUE,
      fill = NA,
      suffixes = NULL,
      join = "outer",
      retside = TRUE,
      retclass = "xts",
      tzone = NULL)
}
\arguments{
  \item{\dots}{ one or more xts objects, or objects coercible to class xts }
  \item{all}{ a logical vector indicating merge type }
  \item{fill}{ values to be used for missing elements }
  \item{suffixes}{ to be added to merged column names }
  \item{join}{ type of database join }
  \item{retside}{ which side of the merged object should be returned (2-case only) }
  \item{retclass}{ object to return }
  \item{tzone}{ time zone of merged object }
}
\details{
This is an xts method compatible with merge.zoo, as xts extends zoo.
That documentation should also be referenced.
Difference are noted where applicable.

Implemented almost entirely in custom C-level code,
it is possible using either the \code{all} argument or
the \code{join} argument to implement all common
database join operations along the to-be-merged objects 
time-index: \sQuote{outer} (full outer - all rows), 
\sQuote{inner} (only rows with common indexes),
\sQuote{left} (all rows in the left object, and those that match in the right),
and \sQuote{right} (all rows in the right object, and those that match in the left).

The above join types can also be expressed as a vector of
logical values passed to \code{all}.  c(TRUE,TRUE) or TRUE for \sQuote{join="outer"},
c(FALSE,FALSE) or FALSE for \sQuote{join="inner"}, c(TRUE, FALSE) for \sQuote{join="left"},
and c(FALSE,TRUE) for \sQuote{join="right"}.

Note that the \code{all} and \code{join} arguments imply a two case scenario.  For merging
more than two objects, they will simply fall back to a full outer or full inner join,
depending on the first position of all, as
left and right can be ambiguous with respect to sides.

To do something along the lines of merge.zoo's method of joining based on
an all argument of the same length of the arguments to join, see the example.  

The resultant object will have the timezone of the leftmost
argument if available. Use \code{tzone} to override. 

If \code{retclass} is \code{NULL}, the joined objects will be split
and reassigned silently back to the original environment they are called
from.  This is for backward compatibility with zoo, though unused
by xts.

If \code{retclass} is \code{FALSE} the object will be stripped of
its class attribute.  This is for internal use.
}
\value{
A new \code{xts} object containing the appropriate
elements of the objects passed in to be merged.
}
\references{ Merge Join Discussion: \url{http://blogs.msdn.com/craigfr/archive/2006/08/03/687584.aspx} }
\author{ Jeffrey A. Ryan }
\note{
This is a highly optimized merge, specifically designed for ordered data. The
only supported merging is based on the underlying time index.
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

# zero-width objects (only index values) can be used
xi <- xts( , index(x))
merge(y, xi)
}
\keyword{ manip }
\keyword{ utilities }
