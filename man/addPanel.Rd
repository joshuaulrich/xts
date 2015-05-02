\name{addPanel}
\alias{addPanel}
\title{Add a panel to an existing xts plot}
\usage{
addPanel(FUN, main = "", on = NA, type = "l", col = NULL, lty = 1,
  lwd = 1, pch = 0, ...)
}
\arguments{
\item{FUN}{an xts object to plot.}

\item{main}{main title for a new panel if drawn.}

\item{on}{panel number to draw on. A new panel will be drawn if \code{on=NA}.}

\item{type}{the type of plot to be drawn, same as in \code{\link{plot}}.}

\item{col}{color palette to use, set by default to rational choices.}

\item{lty}{set the line type, same as in \code{\link{plot}}.}

\item{lwd}{set the line width, same as in \code{\link{plot}}.}

\item{pch}{the type of plot to be drawn, same as in \code{\link{plot}}.}

\item{\dots}{additional named arguments passed through to \code{FUN} and 
any other graphical passthrough parameters.}
}
\description{
Apply a function to the data of an existing xts plot object and plot
the result. \code{FUN} should have arguments \code{x} or \code{R} for the  
data of the existing xts plot object to be passed to. All other additional
arguments for \code{FUN} are passed through \dots.
}
\author{
Ross Bennett
}
