\name{period.apply}
\alias{period.apply}
\title{ Apply Function Over Specified Interval }
\description{
Apply a specified function to data over intervals specified by \code{INDEX}.
The intervals are defined as the observations from \code{INDEX[k]+1} to
\code{INDEX[k+1]}, for \code{k = 1:(length(INDEX)-1)}.
}
\usage{
period.apply(x, INDEX, FUN, ...)
}
\arguments{
  \item{x}{ The data that FUN will be applied to. }
  \item{INDEX}{ A numeric vector of index breakpoint locations. The vector
      should begin with 0 and end with \code{NROW(x)}. }
  \item{FUN}{ A \code{function} to apply to each interval in \code{x}. }
  \item{\dots}{ Additional arguments for \code{FUN}. }
}
\details{
Similar to the rest of the apply family, \code{period.apply()} calculates the
specified function's value over a subset of data. The primary difference is
that \code{period.apply()} applies the function to non-overlapping intervals
of a vector or matrix.

Useful for applying functions over an entire data object by any non-overlapping
intervals. For example, when \code{INDEX} is the result of a call to
\code{endpoints()}.

\code{period.apply()} checks that \code{INDEX} is sorted, unique, starts with
0, and ends with \code{NROW(x)}. All those conditions are true of vectors
returned by \code{endpoints()}.
}
\value{
An object with \code{length(INDEX) - 1} observations (assuming \code{INDEX}
starts with 0 and ends with \code{NROW(x)}).
}
\author{ Jeffrey A. Ryan, Joshua M. Ulrich }
\seealso{ \code{\link{endpoints}} \code{\link{apply.monthly}}}
\examples{
zoo.data <- zoo(rnorm(31)+10,as.Date(13514:13744,origin="1970-01-01"))
ep <- endpoints(zoo.data,'weeks')
period.apply(zoo.data, INDEX=ep, FUN=function(x) mean(x))
period.apply(zoo.data, INDEX=ep, FUN=mean)                  #same

period.apply(letters,c(0,5,7,26), paste0)
}
\keyword{ utilities }% __ONLY ONE__ keyword per line
