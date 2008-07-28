\name{xts}
\alias{xts}
\alias{is.xts}
\title{ Create Or Test For An xts Time-Series Object }
\description{
Constructor function for creating an extensible time-series object.

\code{xts} is used to create an \code{xts} object from raw data inputs.
}
\usage{
xts(x = NULL, order.by = index(x),frequency = NULL, row.names = TRUE, ...)

is.xts(x)
}

\arguments{
  \item{x}{ an object containing the time series data }
  \item{order.by}{ a corresponding vector of unique times/dates - must be
of a known time-based class. See details. }
  \item{frequency}{ numeric indicating frequency of \code{order.by}. See details. }
  \item{row.names}{ should row.names be stripped to minimize memory requirements }
  \item{\dots}{ additional attributes to be added. See details. }
}
\details{
An \code{xts} object extends the S3 class \code{zoo} from the package of the
same name.

The first difference in this extension provides for a requirement
that the index values not only be unique and ordered, but also must be of
a time-based class. Currently acceptable classes include: \sQuote{Date},
\sQuote{POSIXct}, \sQuote{timeDate}, as well as \sQuote{yearmon} and
\sQuote{yearqtr} where the index values remain unique.

The second difference is that the object may now carry additional
attributes that may be desired in individual time-series handling.
This includes the ability to augment the objects data with
meta-data otherwise not cleanly attachable to a standard zoo object.

Examples of usage from finance may include the addition of data
for keeping track of sources, last-update times, financial
instrument descriptions or details, etc.

The idea behind \code{xts} is to offer the user the ability to utilize
a standard zoo object, while providing an mechanism to customize
the object's meta-data, as well as create custom methods to handle
the object in a manner required by the user.

Most, if not all, standard zoo methods will work without change.  Where
changes where required (e.g. to return an xts object) new methods have been
written. At present these modified methods include \code{"["}, \code{coredata},

[add more here]

Subsetting via "[" methods offers the ability to specify dates by
range, if they are enclosed in quotes.  The style borrows from
python by creating ranges with a double colon "::" operator.  Each side of
the operator may be left blank, which would then default to the beginning
and end of the data, respectively.  To specify a subset of times,
it is only required that the time specified be in standard ISO format, with
some form of separation between the elements. The time must be \sQuote{left-filled},
that is to specify a full year one needs only to provide the year, a month would require
the full year and the integer of the month requested - e.g. '1999-01'.
This format would extend all the way down to seconds - e.g. '1999-01-01 08:35:23'.
Leading zeros are not necessary.  See the examples for more detail.

Users may also extend the \code{xts} class to new classes to
allow for method overloading.

Additional benefits derive from the use of \code{as.xts} and
\code{reclass}, which allow for lossless two-way conversion
between common R time-series classes and the \code{xts} object
structure. See those functions for more detail. 


}
\value{
An S3 object of class \code{xts}. As it inherits and extends
the zoo class, all zoo methods remain valid.  Additional
attributes may be assigned and extracted via
\code{xtsAttributes}.
}
\references{ \pkg{zoo}: }
\author{ Jeffrey A. Ryan and Josh M. Ulrich }
\note{ 
Most users will benefit the most by using the
\code{as.xts} and \code{reclass} functions to
automagically handle \emph{all} data abjects
as one would handle a \code{zoo} object.
}
\seealso{ \code{\link{as.xts}}, \code{\link{reclass}},
          \code{\link{xtsAttributes}} }
\examples{
\dontrun{
library(tseries)
yhoo <- get.hist.quote("YHOO")

sample.xts <- xts(yhoo,order.by=index(yhoo),descr="my ts object")

class(sample.xts)
str(sample.xts)

head(sample.xts)  # attribute 'descr' hidden from view
attr(sample.xts,'descr')

sample.xts['2007']  # all of 2007
sample.xts['2007-03::']  # March 2007 to the end of the data set
sample.xts['2007-03::2007']  # March 2007 to the end of 2007
sample.xts['::'] # the whole data set
sample.xts['::2007'] # the beginning of the data through 2007
sample.xts['2007-01-03'] # just the 3rd of January 2007
}
}
\keyword{ utilities }
