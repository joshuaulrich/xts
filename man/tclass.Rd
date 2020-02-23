\name{tclass}
\alias{tclass}
\alias{tformat}
\alias{indexClass}
\alias{indexFormat}
\alias{convertIndex}
\alias{indexClass<-}
\alias{tclass<-}
\alias{tformat<-}
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

tclass(x, ...)
tclass(x) <- value

tformat(x)
tformat(x) <- value

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
of a given \code{xts} object via the \code{tclass} function.

To retrieve the raw numeric data a new accessor function (and replacement) has been
added \code{.index}. This is primarily for internal use, but may be useful for
end-users.

\code{.indexXXX} functions are useful to extract time
components of the underlying time index.  The \sQuote{tclass}
is virtual, and as such suitable conversions are made depending
on the component requested.


The specified value for 
\code{tclass<-} must be a character string containing
one of the following: \code{Date}, \code{POSIXct},
\code{chron}, \code{yearmon}, \code{yearqtr} or \code{timeDate}.

\code{tformat} only manages the manner in which the object
is displayed via \code{print} (also called automatically
when the object is returned) and in conversion to other
classes such as \code{matrix}.  The valid values
for \code{tformat} are the same for \code{format.POSIXct},
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
tformat(x) <- "\%Y-\%b-\%d \%H:\%M:\%OS3"
head(x)

i <- 0:60000
focal_date <- as.numeric(as.POSIXct("2018-02-01", tz = "UTC"))
x <- .xts(i, c(focal_date + i * 15), tz = "UTC", dimnames = list(NULL, "value"))

#select all observations for the first minute of each hour:
x[.indexmin(x) == 0]

# Select all observations for Monday:
mon <- x[.indexwday(x) == 1]
head(mon) ; tail(mon)
unique(weekdays(index(mon))) # check


# Disjoint time of day selections

# Select all observations between 08:30 and 08:59:59.9999  or between 12:00 and 12:14:59.99999:
x[.indexhour(x) == 8 & .indexmin(x) >= 30 | .indexhour(x) == 12 & .indexmin(x) \%in\% 0:14]

### Compound selections

# Select all observations for Wednesdays or Fridays between 9am and 4pm (exclusive of 4pm):
x[.indexwday(x) \%in\% c(3, 5) & (.indexhour(x) \%in\%  c(9:15))]

# Select all observations on Monday between 8:59:45 and 09:04:30:

x[.indexwday(x) == 1 & (.indexhour(x) == 8 & .indexmin(x) == 59 & .indexsec(x) >= 45 |
                          .indexhour(x) == 9 & 
                          (.indexmin(x) < 4 | .indexmin(x) == 4 & .indexsec(x) <= 30))]

i <- 0:30000
u <- .xts(i, c(focal_date + i * 1800), tz = "UTC", dimnames = list(NULL, "value"))

# Select all observations for January or February:
u[.indexmon(u) \%in\% c(0, 1) ]

# Select all data for the 28th to 31st of each month, excluding any Fridays:
u[.indexmday(u) \%in\% 28:31 & .indexwday(u) != 5]

# Subset by week since origin

unique(.indexweek(u))
origin <- xts(1, as.POSIXct("1970-01-01"))
unique(.indexweek(origin))

# e.g. select all observations in weeks 2515 to 2517.
u2 <- u[.indexweek(u) \%in\% 2515:2517]
head(u2); tail(u2)

# select all observations after 12pm for day 50 and 51 in each year
u[.indexyday(u) \%in\% 50:51 & .indexhour(u) >= 12]

}
\keyword{ utilities }
