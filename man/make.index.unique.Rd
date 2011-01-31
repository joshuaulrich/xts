\name{make.index.unique}
\alias{make.index.unique}
\alias{make.time.unique}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Force Time Values To Be Unique
}
\description{
A generic function to force sorted time vectors to be
unique. Useful for high-frequency time-series where original
time-stamps may have identical values. For the case
of xts objects, the default \code{eps} is set to ten
microseconds. In practice this advances each subsequent
identical time by \code{eps} over the previous (possibly also
advanced) value.
}
\usage{
make.index.unique(x, eps = 1e-05, ...)

make.time.unique(x, eps = 1e-05, ...)
}
\arguments{
  \item{x}{
An xts object, or POSIXct vector.
}
  \item{eps}{
value to add to force uniqueness.
}
  \item{\dots}{
unused
}
}
\details{
The returned time-series object will have new time-stamps
so that \code{isOrdered( .index(x) )} evaluates to TRUE.
}
\value{
A modified version of x.
}
\author{
Jeffrey A. Ryan
}
\note{
Incoming values must be pre-sorted, and no check is done to
make sure that this is the case.  If the index values are
of storage.mode \sQuote{integer}, they will be coerced
to \sQuote{double}.
}
\seealso{
\code{\link{align.time}}
}
\examples{

ds <- options(digits.secs=6) # so we can see the change

x <- xts(1:10, as.POSIXct("2011-01-21") + c(1,1,1,2:8)/1e3)
x
make.index.unique(x)

options(ds)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
