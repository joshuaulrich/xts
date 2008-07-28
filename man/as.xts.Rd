\name{as.xts}
\alias{as.xts}
\alias{xtsible}
\alias{use.xts}
\alias{try.xts}
\alias{use.reclass}
\alias{Reclass}
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

Reclass(x)

try.xts(x, ..., error = TRUE)
reclass(x, match.to, error = FALSE, ...)
}
\arguments{
  \item{x}{ data object to convert. See details for supported types }
  \item{match.to}{ \code{xts} object whose attributes will be passed to \code{x}}
  \item{error}{ error handling option. See Details. }
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

\code{Reclass} is designed for top-level use, where it is
desirable to have the object returned from an arbitrary
function in the same class as the object passed in.  Most
functions within \R are unable to return objects
matching the original object's class.  While this tool is
highly experimental at present, it attempts to handle
conversion and reconversion transparently.  The caveats are
that the original object must be coercible to \code{xts},
the returned object must be of the same row length as the
original object, and that the object to reconvert to is
the first argument to the function being wrapped.

\code{try.xts} and \code{reclass} are functions that enable
external developers access to the reclassing tools within
\pkg{xts} to help speed development of time-aware functions,
as well as provide a more robust and seemless end-user experience,
regardless of the end-user's choice of data-classes.

See the accompanying vignette for more details on the above usage
and the package in general.
}
\value{
An S3 object of class \code{xts}.

In the case of \code{Reclass} and \code{reclass}, the object
returned will be of the original class as identified by \code{CLASS}.
}
\author{ Jeffrey A. Ryan }
\seealso{ \code{\link{xts}},\code{\link{as.xts.methods}} }
\keyword{ utilities }
