\name{rbind.xts}
\alias{rbind.xts}
\alias{c.xts}
\title{ Concatenate Two or More xts Objects by Row }
\description{
Concatenate or bind two or more xts objects
along a time-based index.
}
\usage{
\method{c}{xts}(x, y, ...)

\method{rbind}{xts}(x, y, ..., deparse.level = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ an \code{xts} object }
  \item{y}{ an object coercible to class \code{xts} or numeric }
  \item{\dots}{ additional objects to bind }
  \item{deparse.level}{ not implemented }
}
\details{
This function calls a fast merge-join C routine
to bind objects along a sequential index.

By default, there may be non-unique index
values in either the original series, or
the resultant series.

For traditional merge operations, see \code{merge.xts}.
}
\value{
An \code{xts} object with one row per row
for each object concatenated.

}
\author{ Jeffrey A. Ryan }
\note{ 
This differers from merge.zoo in that non-unique values are allowed.

All operations may not behave as expected on objects with non-unique
indices.  You have been warned.
}
\seealso{ \code{\link{merge.xts}} }
\examples{
x <- xts(1:10, Sys.Date()+1:10)
str(x)

merge(x,x)
rbind(x,x)
rbind(x[1:5],x[6:10])

c(x,x)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ utilities }
