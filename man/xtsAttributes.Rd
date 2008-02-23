\name{xtsAttributes}
\alias{xtsAttributes}
\alias{xtsAttributes<-}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Extract and Replace xts Attributes }
\description{
Extract and replace non-core \code{xts} attributes.
}
\usage{
xtsAttributes(x, user=NULL)

xtsAttributes(x) <- value
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ an xts object }
  \item{user}{ logical; should user-defined attributes be returned?
    The default of \code{NULL} returns all \code{xts} attributes. }
  \item{value}{ a list of new name=value attributes }
}
\details{
Since \code{xts} objects are S3 objects with special
attributes, a method is necessary to properly assign
and view the user-added attributes.

A call to \code{attributes} from the \pkg{base} package
will return all attributes, including those specific
to the \code{xts} class.
}
\value{
A named list of user settable attributes.
}
\author{ Jeffrey A. Ryan }
\seealso{ \code{\link{attributes}}}
\examples{
x <- xts(matrix(1:(9*6),nc=6),
         order.by=as.Date(13000,origin="1970-01-01")+1:9,
         a1='my attribute')

xtsAttributes(x)
xtsAttributes(x) <- list(a2=2020)

xtsAttributes(x)
xtsAttributes(x) <- list(a1=NULL)
xtsAttributes(x)
}
\keyword{ utilities }
