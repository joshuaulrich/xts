\name{isOrdered}
\alias{isOrdered}
\title{ Check If A Vector Is Ordered }
\description{
Performs check to determine if a vector is strictly increasing,
strictly decreasing, not decreasing, or not increasing.
}
\usage{
isOrdered(x, increasing = TRUE, strictly = TRUE)
}
\arguments{
  \item{x}{ a numeric vector }
  \item{increasing}{ test for increasing/decreasing values }
  \item{strictly}{ are duplicates OK }
}
\details{
Designed for internal use with \pkg{xts}, this provides
highly optimized tests for ordering.
}
\value{
Logical
}
\author{ Jeffrey A. Ryan }
\seealso{ \code{\link{is.unsorted}} }
\examples{
# strictly increasing
isOrdered(1:10, increasing=TRUE)
isOrdered(1:10, increasing=FALSE)
isOrdered(c(1,1:10), increasing=TRUE)
isOrdered(c(1,1:10), increasing=TRUE, strictly=FALSE)

# decreasing
isOrdered(10:1, increasing=TRUE)
isOrdered(10:1, increasing=FALSE)

}
\keyword{ misc }
