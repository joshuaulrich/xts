\name{CLASS}
\alias{CLASS}
\alias{unCLASS}
\alias{CLASS<-}
\title{ Extract and Set .CLASS Attribute }
\description{
Simple extraction and replacement function
to access \code{xts} .CLASS attribute.  The .CLASS
attribute is used by \code{reclass} to transform
an \code{xts} object back to its original class.
}
\usage{
CLASS(x)
unCLASS(x)

CLASS(x) <- value
}
\arguments{
  \item{x}{ an xts object }
  \item{value}{ the new .CLASS value to assign }
}
\details{
\code{unClass} removes the .CLASS attribute of an \code{xts} object.

It is not recommended that CLASS be called in
daily use.  While it may be possible to coerce objects
to other classes than originally derived from, there is
little, if any, chance that the \code{reclass} function
will perform as expected.

It is best to use the traditional \code{as} methods.
}
\value{
Called for its side-effect of changing the .CLASS attribute
}
\author{ Jeffrey A. Ryan }
\seealso{ \code{\link{as.xts}},\code{\link{reclass}} }
\keyword{ utilities }
