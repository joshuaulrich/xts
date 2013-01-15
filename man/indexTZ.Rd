\name{indexTZ}
\alias{TimeZone}
\alias{indexTZ}
\alias{indexTZ<-}
\alias{tzone}
\alias{tzone<-}
\title{ Query the TimeZone of an xts object }
\description{
Get the TimeZone of an \code{xts} object.
}
\usage{
indexTZ(x, ...)
tzone(x, ...)

indexTZ(x) <- value
tzone(x) <- value
}
\arguments{
  \item{x}{ an \code{xts} object }
  \item{value}{ a valid TZ object }
  \item{\dots}{ unused }
}
\details{
As of version 0.6-4 all objects carry the time zone
under which they were created in a hidden
attribute names \code{.indexTZ}.

Going forward from 0.7-4, the TZ variable is now
also stored in the index itself, in the \code{tzone}
attribute. This is to facilitate the transition to
removing the xts-specific attributes referenced by
\code{indexTZ}, \code{indexFormat}, and \code{indexClass}.
These accessor functions will continue to behave the
same under the new internals. Additionally, there is a new
getter/setter method with \code{tzone} and \code{tzone<-}.

Internally, all time indexing is converted to
POSIXct, seconds since the epoch as defined
by a combination of the underlying OS
and the TZ variable setting at creation.  The current
implementation of xts manages time zone information
as transparently as possible, delegating all
management to R, which is in turn managed
in most instances by the underlying operating system.

During printing, and subsetting by time strings
the internal POSIX representation is used to
identify in human-friendly terms the time
at each position.

This is different than previous versions of \pkg{xts},
where the index was stored in its native format (i.e. class).

The ability to create an index using any of the supported
timeBased classes (POSIXct, Date, dates, chron, timeDate,
yearmon, yearqtr) is managed at the user-interaction point,
and the class is merely stored in another hidden attribute
names \code{.indexCLASS}.  This is accessible via
the \code{indexClass} and \code{indexClass(x)<-} functions.

In most cases, all of this makes the subsetting by time strings
possible, and also allows for consistent and fast manipulation
of the series internally.

Problems may arise when an object that had been created under
one TZ (time zone) are used in a session using another TZ. This
isn't usually a issue, but when it is a warning is given upon printing
or subsetting. This warning may be controlled with \code{option("xts_check_TZ")}.
}
\value{
A named vector of length one, giving the objects TZ at creation.
}
\author{ Jeffrey A. Ryan }
\note{
Timezones are a difficult issue to manage.  If intraday granularity
is not needed, it is often best to set the system TZ to "GMT" or
"UTC".
}
\seealso{ \code{\link{POSIXt}} }
\examples{
x <- xts(1:10, Sys.Date()+1:10)
indexTZ(x)

# same, preferred as of 0.9-1
tzone(x)
str(x)
x
# now set TZ to something different...
\dontrun{
Old.TZ <- Sys.getenv("TZ")
Sys.setenv(TZ="America/Chicago")
x
Sys.setenv(TZ=Old.TZ)
}
}
\keyword{ misc }
