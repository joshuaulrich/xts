\name{align.time}
\Rdversion{1.1}
\alias{align.time}
\alias{align.time.xts}
\alias{adj.time}
\alias{shift.time}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Align seconds, minutes, and hours to beginning of next period.
}
\description{
Change timestamps to the start of the next period, specified
in multiples of seconds.
}
\usage{
align.time(x, ...)

\method{align.time}{xts}(x, n=60, \dots)

shift.time(x, n=60, ...)

adj.time(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  object to align
}
  \item{n}{
  number of seconds to adjust by
}
  \item{\dots}{
  additional arguments. See details.
}
}
\details{
This function is an S3 generic. 
The result is to round up to the next
period determined by \code{n modulo x}.
}
\value{
A new object of class(x)
}
\author{
Jeffrey A. Ryan with input from Brian Peterson
}

\seealso{
\code{\link{to.period}}
}
\examples{
x <- Sys.time() + 1:1000

# every 10 seconds
align.time(x, 10)

# align to next whole minute
align.time(x, 60)

# align to next whole 10 min interval
align.time(x, 10 * 60)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ chron }% __ONLY ONE__ keyword per line
\keyword{ manip }% __ONLY ONE__ keyword per line
\keyword{ ts }% __ONLY ONE__ keyword per line
\keyword{ misc }% __ONLY ONE__ keyword per line
