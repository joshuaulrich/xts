\name{period.max}
\alias{period.max}
\title{ Calculate Max By Period }
\description{
Calculate a maximum for each period of INDEX. Essentially a rolling
application of maximum over a series of non-overlapping sections.
}
\usage{
period.max(x, INDEX)
}
\arguments{
  \item{x}{ a univariate data object }
  \item{INDEX}{ a numeric vector of endpoints to calculate maximum on }
}
\details{
Used to calculate a maximum per period given an arbitrary
index of sections to be calculated over. This is an optimized function
for maximum.  There are additional optimized versions for min, sum, and
prod. 

For xts-coercible objects, an appropriate INDEX can be derived
from a call to 'endpoints'.
}
\value{
An xts or zoo object of maximums, indexed by the period endpoints.
}
\author{ Jeffrey A. Ryan }
\seealso{ \code{\link{endpoints}}, \code{\link{period.sum}},
          \code{\link{period.min}}, \code{\link{period.prod}} }
\examples{
period.max(c(1,1,4,2,2,6,7,8,-1,20),c(0,3,5,8,10))

data(sample_matrix)
period.max(sample_matrix[,1],endpoints(sample_matrix))
period.max(as.xts(sample_matrix)[,1],endpoints(sample_matrix))
}
\keyword{ utilities }% __ONLY ONE__ keyword per line
