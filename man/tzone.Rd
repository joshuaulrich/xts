\name{tzone}
\alias{tzone}
\alias{tzone<-}
\alias{tzone.xts}
\alias{tzone<-.xts}
\alias{indexTZ}
\alias{indexTZ<-}
\alias{TimeZone}
\title{ Get or Replace the Timezone of an xts Object's Index }
\description{
Generic functions to get or replace the timezone of an xts object's index.
}
\usage{
tzone(x, \dots)
tzone(x) <- value

\method{tzone}{xts}(x, \dots)
\method{tzone}{xts}(x) <- value

##### The functions below are DEPRECATED #####
indexTZ(x, \dots)
indexTZ(x) <- value
}
\arguments{
  \item{x}{ an \code{xts} object }
  \item{value}{ a valid timezone value (see \code{OlsonNames()}) }
  \item{\dots}{ arguments passed to other methods }
}
\details{
Internally, an xts object's index is a \emph{numeric} value corresponding to
seconds since the epoch in the UTC timezone. When an xts object is created, all
time index values are converted internally to \code{\link{POSIXct}} (which is
also in seconds since the UNIX epoch), using the underlying OS conventions and
the \env{TZ} environment variable. The \code{xts()} function manages timezone
information as transparently as possible.

The \code{tzone<-} function \emph{does not} change the internal index values
(i.e. the index will remain the same time in the UTC timezone).
}
\note{
Both \code{indexTZ} and \code{indexTZ<-} are deprecated in favor of
\code{tzone} and \code{tzone<-}, respectively.

Problems may arise when an object that had been created under one timezone are
used in a session using another timezone. This isn't usually a issue, but when
it is a warning is given upon printing or subsetting. This warning may be
suppressed by setting \code{options(xts_check_TZ = FALSE)}.
}
\value{
A one element named vector containing the timezone of the object's index.
}
\note{
Both \code{indexTZ} and \code{indexTZ<-} are deprecated in favor of
\code{tzone} and \code{tzone<-}, respectively.

Timezones are a difficult issue to manage. It's best to set the system \env{TZ}
environment variable to "GMT" or "UTC" (via \code{Sys.setenv(TZ = "UTC")} at
the beginning of your scripts if you do not need intra-daily resolution.
}
\seealso{ \code{\link{POSIXt}}
\code{\link{index}} has more information on the xts index,
\code{\link{tformat}} describes how the index values are formatted when printed,
and \code{\link{tclass}} provides details how \pkg{xts} handles the class of
the index.
}
\author{ Jeffrey A. Ryan }
\examples{
# Date indexes always have a "UTC" timezone
x <- xts(1, Sys.Date())
tzone(x)
str(x)
print(x)

# The default 'tzone' is blank -- your machine's local timezone,
# determined by the 'TZ' environment variable.
x <- xts(1, Sys.time())
tzone(x)
str(x)

# now set 'tzone' to different values
tzone(x) <- "UTC"
str(x)

tzone(x) <- "America/Chicago"
str(x)

y <- timeBasedSeq('2010-01-01/2010-01-03 12:00/H')
y <- xts(seq_along(y), y, tzone = "America/New_York")

# Changing the tzone does not change the internal index values, but it
# does change how the index is printed!
head(y)
head(.index(y))
tzone(y) <- "Europe/London"
head(y)          # the index prints with hours, but
head(.index(y))  # the internal index is not changed!
}
\keyword{ts}
\keyword{utilities}
