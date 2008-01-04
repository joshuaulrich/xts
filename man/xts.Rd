\name{xts}
\alias{xts}
\alias{is.xts}
\title{ Create Or Test For An xts Time-Series Object }
\description{
Constructor function for creating an extensible time-series object.

\code{xts} is used to create an \code{xts} object from raw data inputs.
}
\usage{
xts(x, order.by = index(x), frequency = NULL, ...)

is.xts(x)
}

\arguments{
  \item{x}{ an object containing the time series data }
  \item{order.by}{ a corresponding vector of unique times/dates - must be
of class \code{Date} or \code{POSIXct}. }
  \item{frequency}{ numeric indicating frequency of \code{order.by}. See details. }
  \item{\dots}{ additional attributes to be added. See details. }
}
\details{
An \code{xts} object extends the S3 class \code{zoo} from the package of the
same name.

The first difference in this extension provides for a requirement
that the index values not only be unique, but also must be of class
\code{POSIXct} or \code{Date}, or coercible to such.

The second difference is that the object may now carry additional
attributes that may be desired in individual time-series handling.
This includes the ability to augment the objects data with
meta-data otherwise not cleanly attachable to a standard zoo object.

Examples of usage from finance may include the addition of data
for keeping track of data sources, last update times, financial
instrument descriptions or details.

The idea behind \code{xts} is to offer the user the ability to utilize
a standard zoo object, while providing an mechanism to customise
the object's meta-data, as well as create custom methods to handle
the object in a manner required by the user.

Users may also extend the \code{xts} class to new classes to
allow for method overloading.

Additional benefits derive from the use of \code{as.xts} and
\code{reclass}, which allow for lossless two-way conversion
between common R time-series classes and the \code{xts} object
structure. See those functions for more detail. 


}
\value{
An S3 object of class \code{xts}. As it inherits and extends
the zoo class, all zoo methods remain valid.  Additional
attributes assigned may be extracted via the R function
\code{attr}.
}
\references{ \pkg{zoo}: }
\author{ Jeffrey A. Ryan and Josh M. Ulrich }
\note{ 
Most users will benefit the most by using the
\code{as.xts} and \code{reclass} functions to
automagically handle \emph{all} data abjects
as one would handle a \code{zoo} object.
}
\seealso{ \code{\link{as.xts}}, \code{\link{reclass}} }
\examples{

library(tseries)
yhoo <- get.hist.quote("YHOO")

sample.xts <- xts(yhoo,order.by=index(yhoo),descr="my ts object")

class(sample.xts)
str(sample.xts)

head(sample.xts)  # attribute 'descr' hidden from view
attr(sample.xts,'descr')
}
\keyword{ utilities }
