\name{to.period}
\alias{to.period}
\alias{to_period}
\alias{to.minutes}
\alias{to.minutes3}
\alias{to.minutes5}
\alias{to.minutes10}
\alias{to.minutes15}
\alias{to.minutes30}
\alias{to.hourly}
\alias{to.daily}
\alias{to.weekly}
\alias{to.monthly}
\alias{to.quarterly}
\alias{to.yearly}
\alias{OHLC}
\title{ Convert time series data to an OHLC series }
\description{
Convert an OHLC or univariate object to a specified periodicity lower
than the given data object. For example, convert a daily
series to a monthly series, or a monthly series to a yearly one,
or a one minute series to an hourly series.

The result will contain the open and close for the
given period, as well as the maximum and minimum over the
new period, reflected in the new high and low, respectively.

If volume for a period was available, the new volume will also be
calculated.
}
\usage{
to.minutes(x,k,name,...)
to.minutes3(x,name,...)
to.minutes5(x,name,...)
to.minutes10(x,name,...)
to.minutes15(x,name,...)
to.minutes30(x,name,...)
to.hourly(x,name,...)
to.daily(x,drop.time=TRUE,name,...)

to.weekly(x,drop.time=TRUE,name,...)
to.monthly(x,indexAt='yearmon',drop.time=FALSE,name,...)
to.quarterly(x,indexAt='yearqtr',drop.time=FALSE,name,...)
to.yearly(x,drop.time=TRUE,name,...)

to.period(x,
          period = 'months', 
          k = 1, 
          indexAt, 
          name=NULL,
          OHLC = TRUE,
          ...)
}
\arguments{
  \item{x}{ a univariate or OHLC type time-series object } 
  \item{period}{ period to convert to. See details. }
  \item{indexAt}{ convert final index to new class or date. See details }
  \item{drop.time}{ remove time component of POSIX datestamp (if any) }
  \item{k}{ number of sub periods to aggregate on (only for minutes and seconds) }
  \item{name}{ override column names }
  \item{OHLC}{ should an OHLC object be returned? (only \code{OHLC=TRUE} currently supported) }
  \item{\dots}{ additional arguments }
}
\details{
Essentially an easy and reliable way to convert one periodicity of data
into any new periodicity. It is important to note
that all dates will be aligned to the \emph{end} of each period
by default - with the exception of \code{to.monthly} and \code{to.quarterly},
which index by \sQuote{yearmon} and \sQuote{yearqtr} from the \pkg{zoo}
package, respectively.

Valid period character strings include: \code{"seconds"}, 
\code{"minutes"}, \code{"hours"}, \code{"days"}, \code{"weeks"},
\code{"months"}, \code{"quarters"}, and \code{"years"}. These are
calculated internally via \code{endpoints}. See that function's help
page for further details.

To adjust the final indexing style, it is possible to set
\code{indexAt} to one of the following: \sQuote{yearmon},
\sQuote{yearqtr}, \sQuote{firstof}, \sQuote{lastof},
\sQuote{startof}, or \sQuote{endof}.  The final index will
then be \code{yearmon}, \code{yearqtr}, the first time of the period,
the last time of the period, the starting time in the data for that
period, or the ending time in the data for that period, respectively.

It is also possible to pass a single time series, such as
a univariate exchange rate, and return an OHLC object of
lower frequency - e.g. the weekly OHLC of the daily series.

Setting \code{drop.time} to \code{TRUE} (the default)
will convert a series that includes a time
component into one with just a date index, as the time index
is often of little value in lower frequency series.

It is not possible to convert a series from a lower periodicity
to a higher periodicity - e.g. weekly to daily or
daily to 5 minute bars, as that would
require magic.
}
\value{
An object of the original type, with new periodicity.
}
\note{
In order for this function to  work properly on OHLC data, it is necessary
that the Open, High, Low and Close columns be names as such; including
the first letter capitalized and the full spelling found.  Internally
a call is made to reorder the data into the correct column order,
and then a verification step to make sure that this ordering and naming
has succeeded. All other data formats must be aggregated with functions
such as \code{aggregate} and \code{period.apply}.

This method should work on almost all time-series-like
objects. Including \sQuote{timeSeries}, \sQuote{zoo},
\sQuote{ts}, \sQuote{its}, and \sQuote{irts}. It is even likely
to work well for other data structures - including \sQuote{data.frames}
and \sQuote{matrix} objects.

Internally a call to \code{as.xts} converts the original \code{x}
into the universal \code{xts} format, and then re-converts back
to the original type.

A special note with respect to \sQuote{ts} objects. As these are
strictly regular they may include \code{NA} values.  These are
stripped for aggregation purposes, though replaced before returning.
This inevitably leads to many, many additional \sQuote{NA} values
in the data. It is more beneficial to consider using an \sQuote{xts}
object originally, or converting to one in the function call by means
of \code{as.xts}.
}
\examples{
data(sample_matrix)

samplexts <- as.xts(sample_matrix)

to.monthly(samplexts)
to.monthly(sample_matrix)

str(to.monthly(samplexts))
str(to.monthly(sample_matrix))
}
\author{ Jeffrey A. Ryan }
          
\keyword{ utilities }
