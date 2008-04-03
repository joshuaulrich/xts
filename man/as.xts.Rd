\name{as.xts}
\alias{as.xts}
\alias{xtsible}
\alias{reclass}
\title{ Convert Object To And From Class xts }
\description{
Conversion functions to coerce data objects of
arbitrary classes to class \code{xts} and back, without
losing any attributes of the original format.
}
\usage{
as.xts(x, ...)
xtsible(x)

reclass(x)
}
\arguments{
  \item{x}{ data object to convert. See details for supported types }
  \item{\dots}{ additional parameters or attributes }
}
\details{
A simple and reliable way to convert many different
objects into a uniform format for use within \R.

It is possible with a call to \code{as.xts} to
convert objects of class \code{timeSeries}, \code{ts},
\code{its}, \code{matrix}, \code{data.frame}, and \code{zoo}.

\code{xtsible} safely checks whether an object can be converted to
an \code{xts} object; returning TRUE on success and FALSE
otherwise.

The help file \code{as.xts.methods} lists all available
xts methods and arguments specific to each coercible type.

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
\seealso{ \code{\link{xts}},\code{\link{as.xts.methods}} }
\examples{

}
\keyword{ utilities }
