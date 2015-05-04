\name{addSeries}
\alias{addSeries}
\title{Add a time series to an existing xts plot}
\usage{
addSeries(x, main = "", on = NA, type = "l", col = NULL, lty = 1,
  lwd = 1, pch = 0, ...)
}
\arguments{
\item{x}{an xts object to plot.}

\item{main}{main title for a new panel if drawn.}

\item{on}{panel number to draw on. A new panel will be drawn if \code{on=NA}.}

\item{type}{the type of plot to be drawn, same as in \code{\link{plot}}.}

\item{col}{color palette to use, set by default to rational choices.}

\item{lty}{set the line type, same as in \code{\link{par}}.}

\item{lwd}{set the line width, same as in \code{\link{par}}.}

\item{pch}{the type of plot to be drawn, same as in \code{\link{par}}.}

\item{\dots}{any other passthrough graphical parameters.}
}
\description{
Add a time series to an existing xts plot
}
\author{
Ross Bennett
}

