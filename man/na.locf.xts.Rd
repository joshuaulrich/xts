\name{na.locf.xts}
\alias{na.locf.xts}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Last Observation Carried Forward
}
\description{
\pkg{xts} method replace \sQuote{NA} with most recent non-\sQuote{NA}
}
\usage{
na.locf.xts(object, na.rm = FALSE, fromLast = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
an xts object
}
  \item{na.rm}{
logical. Should leading/trailing \sQuote{NA}'s be removed?
The default for xts \code{FALSE} is different than the
default S3 method in the \pkg{zoo} package.
}
  \item{fromLast}{
logical. Cause observations to be carried backward rather
than forward. Default is \code{FALSE}.
}
  \item{\dots}{
unused
}
}
\details{
This is the \pkg{xts} method for the S3 generic \code{na.locf}.
The primary difference to note is that after the \sQuote{NA}
fill action is carried out, the default it to leave
trailing or leading \sQuote{NA}'s in place. This is different
than \pkg{zoo} behavior.
}
\value{
See the documentation in zoo.
}
\references{
\sQuote{zoo}
}
\author{
Jeffrey A. Ryan
}

\examples{
x <- xts(1:10, Sys.Date()+1:10)
x[c(1,2,5,9,10)] <- NA

x
na.locf(x)
na.locf(x, fromLast=TRUE)
na.locf(x, na.rm=TRUE, fromLast=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ misc }
