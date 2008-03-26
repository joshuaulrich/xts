\name{period.sum}
\alias{period.sum}
\title{ Calculate Sum By Period }
\description{
Calculate a sum for each period of INDEX. Essentially a rolling
application of sum over a series of non-overlapping sections.
}
\usage{
period.sum(x, INDEX)
}
\arguments{
  \item{x}{ a univariate data object }
  \item{INDEX}{ a numeric vector of endpoints to calculate sum on }
}
\details{
Used to calculate a sum per period given an arbitrary
index of sections to be calculated over. This is an optimized function
for sum.  There are additionally optimized versions for min, max, and
prod. 

For xts-coercible objects, an appropriate INDEX can be derived
from a call to \code{endpoints}.
}
\value{
An \code{xts} or \code{zoo} object of sums,
indexed by the period endpoints.
}
\author{ Jeffrey A. Ryan }
\seealso{ \code{\link{endpoints}}, \code{\link{period.max}},
          \code{\link{period.min}}, \code{\link{period.prod}} }
\examples{
period.sum(c(1,1,4,2,2,6,7,8,-1,20),c(0,3,5,8,10))

data(sample_matrix)
period.sum(sample_matrix[,1],endpoints(sample_matrix))
period.sum(as.xts(sample_matrix)[,1],endpoints(sample_matrix))
}
\keyword{ utilities }% __ONLY ONE__ keyword per line
