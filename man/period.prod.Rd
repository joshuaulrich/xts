\name{period.prod}
\alias{period.prod}
\title{ Calculate Product By Period }
\description{
Calculate a product for each period of INDEX. Essentially a rolling
application of prod over a series of non-overlapping sections.
}
\usage{
period.prod(x, INDEX)
}
\arguments{
  \item{x}{ a univariate data object }
  \item{INDEX}{ a vector of breakpoints to calculate product on }
}
\details{
Used to calculate a product per period given an arbitrary
index of sections to be calculated over. This is an optimized function
for product.  There are additionally optimized versions for min, max, and
sum. 

For xts-coercible objects, an appropriate INDEX can be
derived from a call to \code{endpoints}.
}
\value{
An \code{xts} or \code{zoo} object of products,
indexed by the period endpoints.
}
\author{ Jeffrey A. Ryan }
\seealso{ \code{\link{endpoints}}, \code{\link{period.sum}}, 
          \code{\link{period.min}}, \code{\link{period.max}} }
\examples{
period.prod(c(1,1,4,2,2,6,7,8,-1,20),c(0,3,5,8,10))

data(sample_matrix)
period.prod(sample_matrix[,1],endpoints(sample_matrix))
period.prod(as.xts(sample_matrix)[,1],endpoints(sample_matrix))
}
\keyword{ utilities }% __ONLY ONE__ keyword per line
