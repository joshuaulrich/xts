\name{addEventLines}
\alias{addEventLines}
\title{Add vertical lines to an existing xts plot}
\usage{
addEventLines(events, main = "", on = 0, lty = 1, lwd = 1, col = 1, ...)
}
\arguments{
\item{events}{xts object of events and their associated labels. It is assumed
that the first column of \code{events} is the event description/label.}

\item{main}{main title for a new panel if drawn.}

\item{on}{panel number to draw on. A new panel will be drawn if \code{on=NA}.
The default, \code{on=0}, will add to the active panel. The active panel is 
defined as the panel on which the most recent action was performed. Note that
only the first element of \code{on} is checked for the default behavior to
add to the last active panel.}

\item{lty}{set the line type, same as in \code{\link{par}}.}

\item{lwd}{set the line width, same as in \code{\link{par}}.}

\item{col}{color palette to use, set by default to rational choices.}

\item{\dots}{any other passthrough parameters to \code{\link{text}} to
control how the event labels are drawn}
}
\description{
Add vertical lines and labels to an existing xts plot
}
\author{
Ross Bennett
}
\examples{
\dontrun{
library(xts)
data(sample_matrix)
sample.xts <- as.xts(sample_matrix)
events <- xts(letters[1:3], 
              as.Date(c("2007-01-12", "2007-04-22", "2007-06-13")))
plot(sample.xts[,4])
addEventLines(events, srt=90, pos=2)
}
}
