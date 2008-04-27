\name{firstof}
\alias{firstof}
\alias{lastof}
\title{ Create a POSIXct Object }
\description{
Enable fast creation of time stamps corresponding
to the first or last observation in a specified
time period.
}
\usage{
firstof(year = 1970, month = 1, day = 1, hour = 0, min = 0, sec = 0, tz = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{year,month,day}{ numerical values to specify a day }
  \item{hour,min,sec}{ numerical vaues to specify time within a day }
  \item{tz}{ timezone used for conversion }
}
\details{
A wrapper to the \R function ISOdatetime with
defaults corresponding to the first or last
possible time in a given period.
}
\value{
An object of class POSIXct.
}
\author{ Jeffrey A. Ryan }
\seealso{ \code{\link{ISOdatetime}} }
\examples{
firstof(2000)
firstof(2005,01,01)

lastof(2007)
lastof(2007,10)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ utilities }
