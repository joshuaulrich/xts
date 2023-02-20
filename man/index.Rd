\name{index.xts}
\alias{index.xts}
\alias{index<-.xts}
\alias{.index}
\alias{.index<-}
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
\alias{.indexisdst}
\alias{convertIndex}
\title{ Get and Replace the Class of an xts Index }
\description{
Functions to get and replace an xts object's index values and it's components.
}
\usage{
\method{index}{xts}(x, ...)
\method{index}{xts}(x) <- value

.index(x, ...)
.index(x) <- value

convertIndex(x, value)

# date/time component extraction
.indexsec(x)
.indexmin(x)
.indexhour(x)

.indexDate(x)
.indexday(x)
.indexwday(x)
.indexmday(x)

.indexweek(x)
.indexmon(x)
.indexyear(x)
.indexyday(x)

.indexisdst(x)
}
\arguments{
  \item{x}{ an \code{xts} object }
  \item{value}{ new index value }
  \item{\dots}{ arguments passed to other methods }
}
\details{
Internally, an xts object's index is a \emph{numeric} value corresponding to
seconds since the epoch in the UTC timezone. The \code{.index} and
\code{.index<-} functions get and replace the internal \emph{numeric} value of
the index, respectively.  These functions are primarily for internal use, but
are exported because they may be useful for users.

The \code{index} and \code{index<-} methods get and replace the xts object's
index, respectively. The replacement method also updates the \code{\link{tclass}}
and \code{\link{tzone}} of the index to match the class and timezone of the new
index, respectively. The \code{index} method converts the index to the class
specified by the \code{\link{tclass}} attribute and with the timezone specified by
the \code{\link{tzone}} attribute before returning the index values to the user.

% #### this is not true, it returns the original object as of 2023-02-02 ####
%The \code{convertIndex} function returns a modified \code{xts} object, and
%\emph{does not} alter the original.

The \code{.indexXXX} functions extract time components (similar to
\code{\link{POSIXlt}} components) from the internal time index:

\describe{
    \item{\code{.indexsec}}{0 - 61: seconds of the minute (local time)}
    \item{\code{.indexmin}}{0 - 59: minutes of the hour (local time)}
    \item{\code{.indexhour}}{0 - 23: hours of the day (local time)}
    \item{\code{.indexDate}}{date as seconds since the epoch (UTC \emph{not local time}}
    \item{\code{.indexday}}{date as seconds since the epoch (UTC \emph{not local time}}
    \item{\code{.indexwday}}{0 - 6: day of the week (Sunday - Saturday, local time)}
    \item{\code{.indexmday}}{1 - 31: day of the month (local time)}
    \item{\code{.indexweek}}{weeks since the epoch (UTC \emph{not local time}}
    \item{\code{.indexmon}}{0 - 11: month of the year (local time)}
    \item{\code{.indexyear}}{years since 1900 (local time)}
    \item{\code{.indexyday}}{0 - 365: day of the year (local time, 365 only in leap years)}
    \item{\code{.indexisdst}}{1, 0, -1: Daylight Saving Time flag. Positive if Daylight
      Saving Time is in effect, zero if not, negative if unknown.}
}

Changes in timezone, index class, and index format internal structure,
by \pkg{xts} version:

\describe{
  \item{Version 0.12.0:}{The \code{.indexTZ}, \code{.indexCLASS} and
    \code{.indexFORMAT} attributes are no longer stored on xts objects, only
    on the index itself.

    The \code{indexTZ}, \code{indexClass}, and \code{indexFormat} functions
    (and their respective replacement methods) are deprecated in favor of their
    respective \code{tzone}, \code{tclass}, and \code{tformat} versions. The
    previous versions will throw a warning that they're deprecated, but they
    will continue to work.  There are no plans to remove them or have them
    throw an error. Ever.

    The latter versions are careful to look for the old attributes on the xts
    object, in case they're ever called on an xts object that was created prior
    to the attributes being added to the index itself.

    There are options to throw a warning if there is no \code{tzone} or
    \code{tclass} attribute on the index, even if there may be one on the xts
    object. This gives the user a way to know if an xts object should be
    updated to use the new structure.

    You can enable the warnings via:
    \code{options(xts.warn.index.missing.tzone = TRUE, xts.warn.index.missing.tclass = TRUE)}
    You can identify xts objects with the old structure by printing them. Then
    you can update them to the new structure using \code{x <- as.xts(x)}.
  }
  \item{Version 0.9.8:}{The index timezone is now set to "UTC" for time classes
    that do not have any intra-day component (e.g. days, months, quarters).
    Previously the timezone was blank, which meant "local time" as determined
    by R and the OS.
  }
  \item{Version 0.9.2:}{There are new get/set methods for the timezone, index
    class, and index format attributes: \code{tzone} and, \code{tzone<-},
    \code{tclass} and \code{tclass<-}, and \code{tformat} and \code{tformat<-}.

    These new functions are aliases to their \code{indexTZ}, \code{indexClass},
    and \code{indexFormat} counterparts.
  }
  \item{Version 0.7.5:}{The timezone, index class, and index format were added
    as attributes to the index itself, as \code{tzone}, \code{tclass}, and
    \code{tformat}, respectively. This is in order to remove those three
    attributes from the xts object, so they're only on the index itself.

    The \code{indexTZ}, \code{indexClass}, and \code{indexFormat} functions
    (and their respective replacement methods) will continue to work as in
    prior \pkg{xts} versions. The attributes on the index take priority over
    their respective counterparts that may be on the xts object.
  }
  \item{Versions 0.6.4 and prior:}{Objects track their timezone and index class
    in their \code{.indexTZ} and \code{.indexCLASS} attributes, respectively.}
}
}
\seealso{
\code{\link{tformat}} describes how the index values are formatted when printed,
\code{\link{tclass}} provides details how \pkg{xts} handles the class of the index,
and \code{\link{tzone}} has more information about the index timezone settings.
}
\author{ Jeffrey A. Ryan }
\examples{
x <- timeBasedSeq('2010-01-01/2010-01-01 12:00/H')
x <- xts(seq_along(x), x)

# the index values, converted to 'tclass' (POSIXct in this case)
index(x)
class(index(x))  # POSIXct
tclass(x)        # POSIXct

# the internal numeric index
.index(x)
# add 1 hour (3600 seconds) to the numeric index
.index(x) <- index(x) + 3600
index(x)

y <- timeBasedSeq('2010-01-01/2010-01-02 12:00')
y <- xts(seq_along(y), y)

# Select all observations in the first 6 and last 3 minutes of the
# 8th and 15th hours on each day
y[.indexhour(y) \%in\% c(8, 15) & .indexmin(y) \%in\% c(0:5, 57:59)]

i <- 0:60000
focal_date <- as.numeric(as.POSIXct("2018-02-01", tz = "UTC"))
y <- .xts(i, c(focal_date + i * 15), tz = "UTC", dimnames = list(NULL, "value"))

# Select all observations for the first minute of each hour
y[.indexmin(y) == 0]

# Select all observations on Monday
mon <- y[.indexwday(y) == 1]
head(mon)
tail(mon)
unique(weekdays(index(mon))) # check

# Disjoint time of day selections

# Select all observations between 08:30 and 08:59:59.9999  or between 12:00 and 12:14:59.99999:
y[.indexhour(y) == 8 & .indexmin(y) >= 30 | .indexhour(y) == 12 & .indexmin(x) \%in\% 0:14]

### Compound selections

# Select all observations for Wednesdays or Fridays between 9am and 4pm (exclusive of 4pm):
y[.indexwday(y) \%in\% c(3, 5) & (.indexhour(y) \%in\%  c(9:15))]

# Select all observations on Monday between 8:59:45 and 09:04:30:

y[.indexwday(y) == 1 & (.indexhour(y) == 8 & .indexmin(y) == 59 & .indexsec(y) >= 45 |
                        .indexhour(y) == 9 &
                        (.indexmin(y) <  4 | .indexmin(y) ==  4 & .indexsec(y) <= 30))]

i <- 0:30000
u <- .xts(i, c(focal_date + i * 1800), tz = "UTC", dimnames = list(NULL, "value"))

# Select all observations for January or February:
u[.indexmon(u) \%in\% c(0, 1)]

# Select all data for the 28th to 31st of each month, excluding any Fridays:
u[.indexmday(u) \%in\% 28:31 & .indexwday(u) != 5]

# Subset by week since origin
unique(.indexweek(u))
origin <- xts(1, as.POSIXct("1970-01-01"))
unique(.indexweek(origin))

# Select all observations in weeks 2515 to 2517.
u2 <- u[.indexweek(u) \%in\% 2515:2517]
head(u2); tail(u2)

# Select all observations after 12pm for day 50 and 51 in each year
u[.indexyday(u) \%in\% 50:51 & .indexhour(u) >= 12]
}
\keyword{ts}
\keyword{utilities}
