\name{timeBased}
\alias{timeBased}
\title{ Check if Class is Time-Based }
\description{
Used to verify that the object is one of the
known time-based classes in R.
}
\usage{
timeBased(x)
}
\arguments{
  \item{x}{ object to test }
}
\details{
Current time-based objects supported are
\code{Date}, \code{POSIXct}, \code{chron},
\code{yearmon}, \code{yearqtr}, and \code{timeDate}.
}
\value{
Logical
}
\author{ Jeffrey A. Ryan }
\examples{
timeBased(Sys.time())
timeBased(Sys.Date())

timeBased(200701)
}
\keyword{ utilities }
