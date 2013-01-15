\name{indexClass}
\alias{indexClass}
\alias{tclass}
\alias{indexFormat}
\alias{convertIndex}
\alias{indexClass<-}
\alias{tclass<-}
\alias{indexFormat<-}
\alias{index.xts}
\alias{index<-.xts}
\alias{.indexhour}
\alias{.indexsec}
\alias{.indexmin}
\alias{.indexyear}
\alias{.indexymon}
\alias{.indexyday}
\alias{.indexday}
\alias{.indexDate}
\alias{.indexmday}
\alias{.indexwday}
\alias{.indexweek}
\alias{.indexmon}
\alias{.index}
\alias{.index<-}
\title{ Extracting/Replacing the Class of an xts Index }
\description{
Generic functions to extract, replace, and format the class of the index
of an xts object.
}
\usage{
\method{index}{xts}(x, ...)
\method{index}{xts}(x) <- value

.index(x, ...)
.index(x) <- value

indexClass(x)
indexClass(x) <- value

tclass(x)
tclass(x) <- value

indexFormat(x)
indexFormat(x) <- value

convertIndex(x,value)

# time component extraction/conversion

.indexDate(x)

.indexday(x)
.indexmday(x)
.indexwday(x)
.indexweek(x)
.indexmon(x)
.indexyday(x)
.indexyear(x)

.indexhour(x)
.indexmin(x)
.indexsec(x)
}
\arguments{
  \item{x}{ xts object }
  \item{value}{ desired new class or format. See details }
  \item{\dots}{ additional arguments (unused) }
}
\details{
The main accessor methods to an \code{xts} object's index
is via the \code{index} and \code{index<-} replacement method.
The structure of the index internally is now a numeric
value corresponding to seconds since the epoch (POSIXct converted to numeric).
This change allows for near native-speed matrix subsetting, as
well as nearly instantaneous speed subsets by time.

A call to \code{index} translates to the desired class on-the-fly.
The desired index class is stored as an attribute within the
xts object.  Upon a standard \code{index} call, this is used
to convert the numeric value to the desired class.

It is possible to view and set the class of the time-index
of a given \code{xts} object via the \code{indexClass} function.

To retrieve the raw numeric data a new accessor function (and replacement) has been
added \code{.index}. This is primarily for internal use, but may be useful for
end-users.

\code{.indexXXX} functions are useful to extract time
components of the underlying time index.  The \sQuote{indexClass}
is virtual, and as such suitable conversions are made depending
on the component requested.


The specified value for 
\code{indexClass<-} must be a character string containing
one of the following: \code{Date}, \code{POSIXct},
\code{chron}, \code{yearmon}, \code{yearqtr} or \code{timeDate}.

\code{indexFormat} only manages the manner in which the object
is displayed via \code{print} (also called automatically
when the object is returned) and in conversion to other
classes such as \code{matrix}.  The valid values
for indexFormat are the same for \code{format.POSIXct},
as this is the function that does the conversion internally.

\code{convertIndex} returns a modified \code{xts} object, and
does \emph{not} alter the original.

Changing the index type may alter the behavior of \pkg{xts}
functions expecting a different index, as well as the functionality
of additional methods.  Use with caution.
}
\author{ Jeffrey A. Ryan }
\examples{
x <- timeBasedSeq('2010-01-01/2010-01-02 12:00')
x <- xts(1:length(x), x)

# all obs. in the first 6 and last 3 minutes of the
# 8th and 15th hours on each day
x[.indexhour(x) \%in\% c(8,15) & .indexmin(x) \%in\% c(0:5,57:59)]

# change the index format
indexFormat(x) <- "\%Y-\%b-\%d \%H:\%M:\%OS3"
head(x)

}
\keyword{ utilities }
