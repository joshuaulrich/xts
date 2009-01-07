\name{rbind.xts}
\alias{rbind.xts}
\alias{c.xts}
\title{ Concatenate Two or More xts Objects by Row }
\description{
Concatenate or bind by row two or more xts objects
along a time-based index.
}
\usage{
\method{c}{xts}(...)

\method{rbind}{xts}(..., deparse.level = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{ objects to bind }
  \item{deparse.level}{ not implemented }
}
\details{
Implemented in C, these functions bind \code{xts} objects
by row, resulting in another \code{xts} object

There may be non-unique index
values in either the original series, or
the resultant series. 

Identical indexed series are bound in the order
or the arguments passed to rbind. See examples.

All objects must have the same number of
columns, as well as be \code{xts} objects
or coercible to such.

\code{rbind} and \code{c} are aliases.

For traditional merge operations, see \code{merge.xts} and
\code{cbind.xts}.
}
\value{
An \code{xts} object with one row per row
for each object concatenated.
}
\author{ Jeffrey A. Ryan }
\note{ 
This differs from rbind.zoo in that non-unique index values are allowed,
in addition to the completely different algorithms used internally.

All operations may not behave as expected on objects with non-unique
indices.  You have been warned.

\code{rbind} is a .Primitive function in \R. As such
method dispatch occurs at the C-level, and may not be
consistent with expectations.  See the details section
of the base function, and if needed call rbind.xts directly
to avoid dispatch ambiguity.
}
\seealso{ \code{\link{merge.xts}}
          \code{\link{rbind}}   }
\examples{
x <- xts(1:10, Sys.Date()+1:10)
str(x)

merge(x,x)
rbind(x,x)
rbind(x[1:5],x[6:10])

c(x,x)

# this also works on non-unique index values
x <- xts(rep(1,5), Sys.Date()+c(1,2,2,2,3))
y <- xts(rep(2,3), Sys.Date()+c(1,2,3))

# overlapping indexes are appended
rbind(x,y)
rbind(y,x)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ utilities }
