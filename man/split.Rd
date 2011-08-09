\name{split.xts}
\Rdversion{1.1}
\alias{split.xts}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Divide into Groups by Time
}
\description{
Creates a list of xts objects split along
time periods.
}
\usage{
\method{split}{xts}(x, f = "months", drop=FALSE, k = 1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
an xts object
}
  \item{f}{
a 'character' vector describing the period to split by
}
  \item{drop}{
ignored by split.xts
}
  \item{k}{
number of periods to aggregate into each split. See Details.
}
  \item{\dots}{
further args to non-xts method
}
}
\details{
A quick way to break up a large xts
object by standard time periods; e.g. 'months', 'qaurters', etc.

\code{endpoints} is used to find the start and end of
each period (or k-periods).  See that function for valid
arguments.

If \code{f} is not a character vector, the NextMethod is
called, which would in turn dispatch to the split.zoo method.
}
\value{
A list of xts objects.
}
\author{
Jeffrey A. Ryan
}
\note{
\code{aggregate.zoo} would be more flexible, though not as fast
for xts objects.
}


\seealso{
\code{\link{endpoints}}, 
\code{\link[zoo]{split.zoo}}, 
\code{\link[zoo]{aggregate.zoo}} 
}
\examples{
data(sample_matrix)
x <- as.xts(sample_matrix)


split(x)
split(x, f="weeks")
split(x, f="weeks", k=4)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ utilities }
