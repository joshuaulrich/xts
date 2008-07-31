\name{coredata.xts}
\alias{coredata.xts}
\alias{xcoredata}
\alias{xcoredata<-}
\title{ Extract/Replace Core Data of an xts Object }
\description{
Mechanism to extract and replace the core data of an
\code{xts} object.
}
\usage{
\method{coredata}{xts}(x, fmt=FALSE, ...)

xcoredata(x,...)
xcoredata(x) <- value
}
\arguments{
  \item{x}{ an \code{xts} object }
  \item{fmt}{ should the rownames be formated in a non-standard way }
  \item{value}{ non-core attributes to assign }
  \item{\dots}{ further arguments [unused] }
}
\details{
Extract coredata of an \code{xts} object - removing 
all attributes except \code{dim} and \code{dimnames} and
returning a matrix object with rownames converted from
the index of the \code{xts} object.

The \code{fmt} argument, if TRUE, allows the internal
index formatting specified by the user to be used. Alternatively,
it may be a valid formatting string to be passed to \code{format}.
Setting to FALSE will return the row names by simply
coercing the index class to a character string in the default
manner.

\code{xcoredata} is the functional complement to
\code{coredata}, returning all of the attributes
normally removed by \code{coredata}.  Its purpose,
along with the replacement function \code{xcoredata<-}
is primarily for use by developers using \pkg{xts}
to allow for internal replacement of values
removed during use of non xts-aware functions.
}
\value{
Returns either a matrix object for coredata, or
a list of named attributes.

The replacement functions are called for their side-effects.
}
\author{ Jeffrey A. Ryan }
\seealso{ \code{\link[zoo:zoo-package]{coredata}}, \code{\link{xtsAttributes}} }
\examples{
data(sample_matrix)
x <- as.xts(sample_matrix, myattr=100)
coredata(x)
xcoredata(x)
}
\keyword{ utilities }
