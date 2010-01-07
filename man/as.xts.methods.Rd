\name{as.xts.methods}
\alias{as.xts.methods}
\alias{as.xts.timeSeries}
\alias{as.timeSeries.xts}
\alias{as.xts.its}
\alias{as.xts.xts}
\alias{as.xts.zoo}
\alias{as.xts.ts}
\alias{as.xts.fts}
\alias{as.xts.data.frame}
\alias{as.xts.matrix}
\alias{as.fts.xts}
\title{ Convert Object To And From Class xts }
\description{
Conversion S3 methods to coerce data objects of
arbitrary classes to class \code{xts} and back, without
losing any attributes of the original format.
}
\usage{
\method{as.xts}{xts}(x,...)


\method{as.xts}{timeSeries}(x, dateFormat="POSIXct", FinCenter, recordIDs,
    title, documentation, ...)

\method{as.xts}{its}(x,...)

\method{as.xts}{zoo}(x, order.by=index(x), frequency=NULL, ...) 

\method{as.xts}{ts}(x, dateFormat,...)

\method{as.xts}{data.frame}(x, order.by, dateFormat="POSIXct",
    frequency=NULL, ...) 

\method{as.xts}{matrix}(x, order.by, dateFormat="POSIXct",
    frequency=NULL, ...) 


as.fts.xts(x)
}
\arguments{
  \item{x}{ data object to convert. See details for supported types }
  \item{dateFormat}{what format should the dates be converted to}
  \item{FinCenter}{see timeSeries help}
  \item{recordIDs}{see timeSeries help}
  \item{title}{see timeSeries help}
  \item{documentation}{see timeSeries help}
  \item{order.by}{see zoo help}
  \item{frequency}{see zoo help}
  \item{\dots}{ additional parameters or attributes }
}
\details{
A simple and reliable way to convert many different
objects into a uniform format for use within \R.

It is possible with a call to \code{as.xts} to
convert objects of class \code{timeSeries}, \code{ts},
\code{its}, \code{matrix}, \code{data.frame}, and \code{zoo}.

Additional name=value pairs may be passed to the function
to be added to the new object. A special print.xts method
will assure that the attributes are hidden from view, but will
be available via \R's standard \code{attr} function.

The returned object will preserve all relevant attribute/slot
data within itself, allowing for temporary conversion to use
zoo and xts compatible methods. A call to \code{reclass}
returns the object to its original class, with all original
attributes intact - unless otherwise changed.

It should be obvious, but any attributes added via the \dots
argument will not be carried back to the original data object,
as there would be no available storage slot/attribute.
}
\value{
An S3 object of class \code{xts}.
}
\author{ Jeffrey A. Ryan }
\seealso{ \code{\link{xts}} }
\keyword{ utilities }
