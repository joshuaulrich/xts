\name{tformat}
\alias{tformat}
\alias{tformat<-}
\alias{tformat.xts}
\alias{tformat<-.xts}
\alias{indexFormat}
\alias{indexFormat<-}
\title{ Get or Replace the Format of an xts Object's Index }
\description{
Generic functions to get or replace the format that determines how an xts
object's index is printed.
}
\usage{
tformat(x, \dots)
tformat(x) <- value

\method{tformat}{xts}(x, \dots)
\method{tformat}{xts}(x) <- value

##### The functions below are DEPRECATED #####
indexFormat(x)
indexFormat(x) <- value
}
\arguments{
  \item{x}{ an \code{xts} object }
  \item{value}{ new index format string (see Details for valid values) }
  \item{\dots}{ arguments passed to other methods }
}
\details{
  Valid values for the \code{value} argument are the same as specified in the
  \emph{Details} section of \code{\link{strptime}}.

  An xts object's \code{tformat} is \code{NULL} by default, so the index will be
  formatted according to its \code{\link{tclass}} (e.g. \code{Date},
  \code{POSIXct}, \code{timeDate}, \code{yearmon}, etc.).

  \code{tformat} only changes how the index is \emph{printed} and how the row
  names are formatted when xts objects are converted to other classes (e.g.
  \code{matrix} or \code{data.frame}. It does not affect the internal index
  in any way.
}
\value{
A vector containing the format for the object's index.
}
\note{
Both \code{indexFormat} and \code{indexFormat<-} are deprecated in favor of
\code{tformat} and \code{tformat<-}, respectively.
}
\seealso{
\code{\link{index}} has more information on the xts index, \code{\link{tclass}}
details how \pkg{xts} handles the class of the index, \code{\link{tzone}} has
more information about the index timezone settings.
}
\author{ Jeffrey A. Ryan }
\examples{
  x <- timeBasedSeq('2010-01-01/2010-01-02 12:00')
  x <- xts(seq_along(x), x)

  # set a custom index format
  head(x)
  tformat(x) <- "\%Y-\%b-\%d \%H:\%M:\%OS3"
  head(x)
}
\keyword{ts}
\keyword{utilities}
