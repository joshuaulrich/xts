\name{points.xts}
\alias{points.xts}
\title{Add time series of points to an existing xts plot}
\usage{
\method{points}{xts}(x, ..., main = "", on = NA, col = NULL, pch = 0)
}
\arguments{
\item{x}{an xts object to plot.}

\item{\dots}{any other passthrough parameters. Not currently used.}

\item{main}{main title for a new panel if drawn.}

\item{on}{panel number to draw on. A new panel will be drawn if \code{on=NA}.}

\item{col}{color palette to use, set by default to rational choices.}

\item{pch}{the type of plot to be drawn, same as in \code{\link{plot}}.}
}
\description{
Add time series of points to an existing xts plot
}
\author{
Ross Bennett
}

