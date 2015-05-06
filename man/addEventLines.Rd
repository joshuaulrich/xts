\name{addEventLines}
\alias{addEventLines}
\title{Add vertical lines to an existing xts plot}
\usage{
addEventLines(event.dates, event.labels = NULL, date.format = "\%Y-\%m-\%d",
  main = "", on = 0, lty = 1, lwd = 1, col = 1, ...)
}
\arguments{
\item{event.dates}{character vector of dates. Vertical lines will be drawn
to indicate that an event happened during that time period.  \code{event.lines} should
be a vector of dates (e.g., \code{c("09/03","05/06"))} formatted the same as
\code{date.format}. This function matches the re-formatted row names (dates) with
the events.list, so to get a match the formatting needs to be correct.}

\item{event.labels}{character vector of event labels corresponding to
\code{event.dates}. This will apply text labels (e.g.,
\code{c("This Event", "That Event")} to the vertical lines drawn.}

\item{date.format}{format for the dates in \code{event.lines}.}

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
Add vertical lines to an existing xts plot
}
\author{
Ross Bennett
}

