\name{dimnames.xts}
\alias{dimnames.xts}
\alias{dimnames<-.xts}
\title{ Dimnames of an xts Object }
\description{
Get or set dimnames of an xts object.
}
\usage{
\method{dimnames}{xts}(x)

\method{dimnames}{xts}(x) <- value
}
\arguments{
  \item{x}{ an xts object }
  \item{value}{ a list object of length two. See Details. }
}
\details{
The functions \code{dimnames.xts} and \code{dimnames<-.xts} are
methods for the base functions \code{dimnames} and \code{dimnames<-}.

\code{xts} objects by design are intended for lightweight management
of time-indexed data. 

Rownames are redundant in this design, as well
as quite burdensome with respect to memory consumption and
internal copying costs.

\code{rownames} and \code{colnames} in \R make use of \code{dimnames} method
dispatch internally, and thus require only modifications
to dimnames to enforce the \code{xts} no rownames requirement.

To prevent accidental setting of rownames, \code{dimnames<-}
for \code{xts} will simply set the rownames to \code{NULL}
when invoked, regardless of attempts to set otherwise.

This is done for internal compatibility reasons, as well as
to provide consistency in performance regardless of object use.

User level interaction with either dimnames or rownames will
produce a character vector of the index, formatted based
on the current specification of \code{indexFormat}. This
occurs within the call by converting the results
of calling \code{index(x)} to a character string, which itself
first creates the object type specified internally from the
underlying numeric time representation.
}
\value{
A list or character string containing coerced row names
and/or actual column names.

Attempts to set rownames on xts objects via rownames or dimnames
will silently fail.  This is your warning.
}
\author{ Jeffrey A. Ryan }
\note{ 
All \code{xts} objects have dimension.  There are
no \code{xts} objects representable as named or unnamed
vectors.
}
\seealso{ \code{\link{xts}} }
\examples{
x <- xts(1:10, Sys.Date()+1:10)
dimnames(x)
rownames(x)
rownames(x) <- 1:10
rownames(x)
str(x)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ misc }
