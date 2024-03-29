\name{as.xts.methods}
\alias{as.xts.methods}
\alias{as.xts.timeSeries}
\alias{as.timeSeries.xts}
\alias{as.xts.xts}
\alias{as.xts.zoo}
\alias{as.xts.ts}
\alias{as.xts.data.frame}
\alias{as.xts.matrix}
\title{ Convert Object To And From Class xts }
\description{
Conversion S3 methods to coerce data objects of
arbitrary classes to class \code{xts} and back, without
losing any attributes of the original format.
}
\usage{
\method{as.xts}{xts}(x,...,.RECLASS=FALSE)


\method{as.xts}{timeSeries}(x, dateFormat="POSIXct", FinCenter, recordIDs,
    title, documentation, ..., .RECLASS=FALSE)

\method{as.xts}{zoo}(x, order.by=index(x), frequency=NULL, ..., .RECLASS=FALSE)

\method{as.xts}{ts}(x, dateFormat,...,.RECLASS=FALSE)

\method{as.xts}{data.frame}(x, order.by, dateFormat="POSIXct",
    frequency=NULL, ...,.RECLASS=FALSE) 

\method{as.xts}{matrix}(x, order.by, dateFormat="POSIXct",
    frequency=NULL, ..., .RECLASS=FALSE) 
}
\arguments{
  \item{x}{ data object to convert. See details for supported types }
  \item{dateFormat}{what format should the dates be converted to}
  \item{FinCenter}{see timeSeries help}
  \item{recordIDs}{see timeSeries help}
  \item{title}{see timeSeries help}
  \item{documentation}{see timeSeries help}
  \item{order.by}{see \link[zoo]{zoo} help }
  \item{frequency}{see \link[zoo]{zoo} help }
  \item{\dots}{ additional parameters or attributes }
  \item{.RECLASS}{ should conversion be reversible? }
}
\details{
A simple and reliable way to convert many different
objects into a uniform format for use within \R.

It is possible with a call to \code{as.xts} to
convert objects of class \code{timeSeries}, \code{ts},
\code{matrix}, \code{data.frame}, and \code{zoo}.

Additional name=value pairs may be passed to the function
to be added to the new object. A special print.xts method
will assure that the attributes are hidden from view, but will
be available via \R's standard \code{attr} function.

If \code{.RECLASS=TRUE}, the returned object will preserve all relevant attribute/slot
data within itself, allowing for temporary conversion to use
zoo and xts compatible methods. A call to \code{reclass}
returns the object to its original class, with all original
attributes intact - unless otherwise changed.  This is the default
behavior when \code{try.xts} is used for conversion, and should
not be altered by the user; i.e. don't touch it unless you are
aware of the consequences.

It should be obvious, but any attributes added via the \dots
argument will not be carried back to the original data object,
as there would be no available storage slot/attribute.
}
\value{
An S3 object of class \code{xts}.
}
\author{ Jeffrey A. Ryan }
\seealso{ \code{\link{xts}}, \code{\link[zoo]{zoo}} }
\examples{
  \dontrun{
  # timeSeries
  library(timeSeries)
  x <- timeSeries(1:10, 1:10)

  str( as.xts(x) )
  str( reclass(as.xts(x)) )
  str( try.xts(x) )
  str( reclass(try.xts(x)) )
  }
}
\keyword{ utilities }
