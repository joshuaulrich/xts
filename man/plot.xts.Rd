\name{plot.xts}
\alias{plot.xts}
\alias{lines.xts}
\alias{points.xts}
\title{Plotting xts Objects}
\usage{
\method{plot}{xts}(x, y = NULL, ..., subset = "", FUN = NULL,
  panels = NULL, multi.panel = FALSE, col = 1:12, up.col = "green",
  dn.col = "red", type = "l", lty = 1, lwd = 2, lend = 1,
  main = deparse(substitute(x)), clev = 0, cex = 0.6, cex.axis = 0.9,
  mar = c(3, 2, 0, 2), srt = 0, xaxis.las = 0, ylim = NULL,
  yaxis.same = TRUE, yaxis.left = TRUE, yaxis.right = TRUE,
  grid.ticks.on = "months", grid.ticks.lwd = 1, grid.ticks.lty = 1,
  grid.col = "darkgray", labels.col = "#333333", format.labels = TRUE,
  shading = 1, bg.col = "#FFFFFF", grid2 = "#F5F5F5", legend.loc = NULL)
\method{lines}{xts}(x, ..., main = "", on = NA, col = NULL, type = "l",
  lty = 1, lwd = 1, pch = 0)
\method{points}{xts}(x, ..., main = "", on = NA, col = NULL, pch = 0)
}
\arguments{
\item{x}{xts object}

\item{y}{NULL, not used}

\item{\dots}{any passthrough parameters to FUN for \code{plot} and additional graphical 
arguments for \code{lines} and \code{points}}

\item{subset}{character vector of length one of the subset range using subsetting as in \code{\link{xts}}}

\item{FUN}{function to apply to \code{x} and plot}

\item{panels}{character vector of expressions to plot as panels}

\item{multi.panel}{TRUE/FALSE or an integer less than or equal to the number
of columns in the data set. If TRUE, each column of the data is plotted in a
separate panel. For example, if \code{multi.panel = 2}, then the data
will be plotted in groups of 2 columns and each group is plotted in a
separate panel.}

\item{col}{color palette to use, set by default to rational choices}

\item{up.col}{color for positive bars if \code{type="h"}}

\item{dn.col}{color for positive bars if \code{type="h"}}

\item{type}{the type of plot to be drawn, same as in \code{\link{plot}}}

\item{lty}{set the line type, same as in plot}

\item{lwd}{set the line width, same as in plot}

\item{lend}{set the line end style, same as in plot}

\item{main}{main title}

\item{clev}{level for shading, not currently used}

\item{cex}{not currently used}

\item{cex.axis}{cex of the axis}

\item{mar}{set the margins, same as in par}

\item{srt}{rotation for the y axis labels}

\item{xaxis.las}{rotation for the x axis labels}

\item{ylim}{the range of the y axis}

\item{yaxis.same}{TRUE/FALSE. If TRUE, the y axis is drawn with the same ylim for multiple panels}

\item{yaxis.left}{if TRUE, draws the y axis on the left}

\item{yaxis.right}{if TRUE, draws the y axis on the right}

\item{grid.ticks.on}{period to draw the grid ticks on}

\item{grid.ticks.lwd}{line width of the grid}

\item{grid.ticks.lty}{line type of the grid}

\item{grid.col}{color of the grid}

\item{labels.col}{color of the axis labels}

\item{format.labels}{not currently used}

\item{shading}{not currently used}

\item{bg.col}{not currently used}

\item{grid2}{color for secondary x axis grid}

\item{legend.loc}{places a legend into one of nine locations on the chart:
bottomright, bottom, bottomleft, left, topleft, top, topright, right, or
center. Default NULL does not draw a legend.}

\item{pch}{the plotting character to use, same as in \code{\link{par}}.}

\item{on}{panel number to draw on. A new panel will be drawn if \code{on=NA}.}
}
\description{
Plotting for xts objects.
TODO: description, details, and examples
}
\author{
Ross Bennett
}
\references{
based on \code{chart_Series} in the \code{quantmod} package by Jeffrey A. Ryan
}
\examples{
\dontrun{
data(sample_matrix)
sample.xts <- as.xts(sample_matrix)

# plot the Close
plot(sample.xts[,"Close"])

# plot a subset of the data
plot(sample.xts[,"Close"], subset="2007-04-01/2007-06-31")

# function to compute simple returns
simple.ret <- function(x, col.name){
  x[,col.name] / lag(x[,col.name]) - 1
}

# pass in the simple.ret function and plot the output
plot(sample.xts, FUN=simple.ret, col.name="Close")

# plot the close and add a panel with the simple returns
plot(sample.xts[,"Close"])
R <- simple.ret(sample.xts, "Close")
lines(R, type="h")

# add the 50 period simple moving average to panel 1 of the plot
library(TTR)
lines(SMA(sample.xts[,"Close"], n = 50), on=1, col="blue")

# add month end points to the chart
points(sample.xts[endpoints(sample.xts[,"Close"], on = "months"), "Close"], 
       col="red", pch=17, on=1)

# add legend to panel 1
addLegend("topright", on=1, 
          legend.names = c("Close", "SMA(50)"), 
          lty=c(1, 1), lwd=c(2, 1),
          col=c("black", "blue", "red"))
}
}
