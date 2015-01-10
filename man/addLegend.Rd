\name{addLegend}
\alias{addLegend}
\title{Add Legend}
\usage{
addLegend(legend.loc = "center", legend.names = NULL, col = NULL,
  ncol = 1, on = 1, ...)
}
\arguments{
\item{legend.loc}{legend.loc places a legend into one of nine locations on
the chart: bottomright, bottom, bottomleft, left, topleft, top, topright,
right, or center.}

\item{legend.names}{character vector of names for the legend. If \code{NULL},
the column names of the current plot object are used.}

\item{col}{fill colors for the legend. If \code{NULL},
the colorset of the current plot object data is used.}

\item{ncol}{number of columns for the legend}

\item{on}{panel number to draw on. A new panel will be drawn if \code{on=NA}.}

\item{\dots}{any other passthrough parameters to \code{\link{legend}}.}
}
\description{
Add Legend
}
\author{
Ross Bennett
}

