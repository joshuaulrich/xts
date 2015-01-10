\name{lines.xts}
\alias{lines.xts}
\title{Add time series lines to an existing xts plot}
\usage{
\method{lines}{xts}(x, ..., main = "", on = NA, col = NULL, lty = 1,
  lwd = 1, pch = 0)
}
\arguments{
\item{x}{an xts object to plot.}

\item{\dots}{any other passthrough parameters. Not currently used.}

\item{main}{main title for a new panel if drawn.}

\item{on}{panel number to draw on. A new panel will be drawn if \code{on=NA}.}

\item{col}{color palette to use, set by default to rational choices.}

\item{lty}{set the line type, same as in \code{\link{plot}}.}

\item{lwd}{set the line width, same as in \code{\link{plot}}.}

\item{pch}{the type of plot to be drawn, same as in \code{\link{plot}}.}
}
\description{
Add time series lines to an existing xts plot
}
\author{
Ross Bennett
}