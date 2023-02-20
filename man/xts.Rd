\name{xts}
\alias{xts}
\alias{.xts}
\alias{is.xts}
\title{ Create Or Test For An xts Time-Series Object }
\description{
Constructor function for creating an extensible time-series object.

\code{xts} is used to create an \code{xts} object from raw data inputs.
}
\usage{
xts(x = NULL,
    order.by = index(x),
    frequency = NULL,
    unique = TRUE,
    tzone = Sys.getenv("TZ"),
    ...)

.xts(x = NULL,
     index,
     tclass = c("POSIXct", "POSIXt"),
     tzone = Sys.getenv("TZ"),
     check = TRUE,
     unique = FALSE,
     ...)

is.xts(x)
}

\arguments{
  \item{x}{ an object containing the time series data }
  \item{order.by}{ a corresponding vector of dates/times of a known time-based
      class. See Details. }
  \item{index}{ a corresponding \emph{numeric} vector specified as seconds
      since the UNIX epoch (1970-01-01 00:00:00.000) }
  \item{frequency}{ numeric indicating frequency of \code{order.by}. See Details. }
  \item{unique}{ check the index for unique timestamps? }
  \item{check}{ check that the index is ordered? }
  \item{tclass}{ time class to use for the index. See code{\link{tclass}}. }
  \item{tzone}{ time zone of the index (ignored indices without a time
      component, e.g. Date, yearmon, yearqtr). See \code{\link{tzone}}. }
  \item{\dots}{ additional attributes to be added. See Details. }
}
\details{
An \code{xts} object extends the S3 class \code{zoo} from the package of the
same name.

The \code{xts()} constructor is the preferred way to create xts objects. It
performs several checks to ensure it returns a well-formed xts object. The
\code{.xts()} constructor is mainly for internal use. It is more efficient than
the regular \code{xts()} constructor because it doesn't perform as many
validity checks. Use it with caution.

% TODO: add notes here about the differences between 'empty', 'zero-width', and
%    'zero-length' xts objects.

Similar to zoo objects, xts objects must have an ordered index.  While zoo
indexes cannot contain duplicate values, xts objects have optionally supported
duplicate index elements since version 0.5-0.  The \code{xts} class has one
additional requirement, the index must be a time-based class.  Currently
supported classes include: \sQuote{Date}, \sQuote{POSIXct}, \sQuote{timeDate},
as well as \sQuote{yearmon} and \sQuote{yearqtr} where the index values
remain unique.

The uniqueness requirement was relaxed in version 0.5-0, but is still enforced
by default.  Setting \code{unique = FALSE} skips the uniqueness check and only
ensures that the index is ordered via the \code{isOrdered} function.

As of version 0.10-0, xts no longer allows missing values in the index.  This
is because many xts functions expect all index values to be finite.  The most
important of these is \code{merge.xts}, which is used ubiquitously.  Missing
values in the index are usually the result of a date-time conversion error (e.g.
incorrect format, non-existent time due to daylight saving time, etc).  Because
of how non-finite numbers are represented, a missing timestamp will always be
at the end of the index (except if it is \code{-Inf}, which will be first).

Another difference from \pkg{zoo} is that xts object may carry additional
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

Many xts-specific methods have been written to better handle the
unique aspects of xts.  These include, \sQuote{"["}, merge, cbind,
rbind, c, Ops, lag, diff, coredata, head and tail.  Additionally
there are xts specific methods for converting to/from R's different
time-series classes. 

Subsetting via "[" methods offers the ability to specify dates by
range, if they are enclosed in quotes.  The style borrows from
python by creating ranges with a double colon \dQuote{"::"} or \dQuote{"/"} operator.  Each side of
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

Additional benefits derive from the use of \code{\link{as.xts}} and
\code{\link{reclass}}, which allow for lossless two-way conversion
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
\author{ Jeffrey A. Ryan and Joshua M. Ulrich }
\note{ 
Most users will benefit the most by using the
\code{as.xts} and \code{reclass} functions to
automagically handle \emph{all} data objects
as one would handle a \code{zoo} object.

}
\seealso{ \code{\link{as.xts}}, \code{\link{index}}, \code{\link{tclass}},
  \code{\link{tformat}}, \code{\link{tzone}}, \code{\link{xtsAttributes}} }
\examples{
data(sample_matrix)
sample.xts <- as.xts(sample_matrix, descr='my new xts object')

class(sample.xts)
str(sample.xts)

head(sample.xts)  # attribute 'descr' hidden from view
attr(sample.xts,'descr')

sample.xts['2007']  # all of 2007
sample.xts['2007-03/']  # March 2007 to the end of the data set
sample.xts['2007-03/2007']  # March 2007 to the end of 2007
sample.xts['/'] # the whole data set
sample.xts['/2007'] # the beginning of the data through 2007
sample.xts['2007-01-03'] # just the 3rd of January 2007
}
\keyword{ utilities }
