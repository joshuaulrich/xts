\name{period.min}
\alias{period.min}
\title{ Calculate Min By Period }
\description{
Calculate a minimum for each period of INDEX. Essentially a rolling
application of minimum over a series of non-overlapping sections.
}
\usage{
period.min(x, INDEX)
}
\arguments{
  \item{x}{ a univariate data object }
  \item{INDEX}{ a numeric vector of endpoints to calculate maximum on }
}
\details{
Used to calculate a minimum per period given an arbitrary
index of sections to be calculated over. This is an optimized function
for minimum. There are additional optimized versions for max, sum, and
prod. 

For xts-coercible objects, an appropriate INDEX
can be derived from a call to \code{endpoints}.
}
\value{
An xts or zoo object of minimums, indexed by the period endpoints.
}
\author{ Jeffrey A. Ryan }
\seealso{ \code{\link{endpoints}}, \code{\link{period.sum}},
          \code{\link{period.max}}, \code{\link{period.prod}} }
\examples{
period.min(c(1,1,4,2,2,6,7,8,-1,20),c(0,3,5,8,10))

data(sample_matrix)
period.min(sample_matrix[,1],endpoints(sample_matrix))
period.min(as.xts(sample_matrix)[,1],endpoints(sample_matrix))
}
\keyword{ utilities }% __ONLY ONE__ keyword per line
