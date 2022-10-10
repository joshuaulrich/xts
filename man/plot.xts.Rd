\name{plot.xts}
\alias{plot.xts}
\alias{lines.xts}
\alias{points.xts}
\title{Plotting xts Objects}
\usage{
\method{plot}{xts}(x, y = NULL, ..., subset = "",
  panels = NULL, multi.panel = FALSE, col = 1:8, up.col = NULL,
  dn.col = NULL, bg = "#FFFFFF", type = "l", lty = 1, lwd = 2, lend = 1,
  main = deparse(substitute(x)), main.timespan = TRUE, observation.based = FALSE,
  ylim = NULL, yaxis.same = TRUE, yaxis.left = TRUE, yaxis.right = TRUE,
  yaxis.ticks = 5, major.ticks = "auto", minor.ticks = NULL,
  grid.ticks.on = "auto", grid.ticks.lwd = 1, grid.ticks.lty = 1,
  grid.col = "darkgray", labels.col = "#333333", format.labels = TRUE,
  grid2 = "#F5F5F5", legend.loc = NULL, extend.xaxis = FALSE)
\method{lines}{xts}(x, ..., main = "", on = 0, col = NULL, type = "l",
  lty = 1, lwd = 1, pch = 1)
\method{points}{xts}(x, ..., main = "", on = 0, col = NULL, pch = 1)
}
\arguments{
\item{x}{xts object}

\item{y}{NULL, not used}

\item{\dots}{any passthrough graphical arguments for \code{lines} and \code{points}}

\item{subset}{character vector of length one of the subset range using subsetting as in \code{\link{xts}}}

\item{panels}{character vector of expressions to plot as panels}

\item{multi.panel}{TRUE/FALSE or an integer less than or equal to the number
of columns in the data set. If TRUE, each column of the data is plotted in a
separate panel. For example, if \code{multi.panel = 2}, then the data
will be plotted in groups of 2 columns and each group is plotted in a
separate panel.}

\item{col}{color palette to use, set by default to rational choices}

\item{up.col}{color for positive bars if \code{type="h"}}

\item{dn.col}{color for negative bars if \code{type="h"}}

\item{bg}{background color of plotting area, same as in \code{\link{par}}}

\item{type}{the type of plot to be drawn, same as in \code{\link{plot}}}

\item{lty}{set the line type, same as in \code{\link{par}}}

\item{lwd}{set the line width, same as in \code{\link{par}}}

\item{lend}{set the line end style, same as in \code{\link{par}}}

\item{main}{main title}

\item{main.timespan}{include the timespan of the series on the plot? (default \code{TRUE})}

\item{observation.based}{TRUE/FALSE (default FALSE). If \code{TRUE}, the x-axis is drawn based on observations in the data. If \code{FALSE}, the x-axis is drawn based on the time index of the data.}

\item{ylim}{the range of the y axis}

\item{yaxis.same}{TRUE/FALSE. If TRUE, the y axis is drawn with the same ylim for multiple panels}

\item{yaxis.left}{if TRUE, draws the y axis on the left}

\item{yaxis.right}{if TRUE, draws the y axis on the right}

\item{yaxis.ticks}{desired number of y axis grid lines. The actual number
  of grid lines is determined by the \code{n} argument to \code{\link{pretty}}.}

\item{major.ticks}{period that specifies where tick marks and labels will be
  drawn on the x-axis. See Details for possible values.}

\item{minor.ticks}{period that specifies where minor ticks on will be drawn on
  the x-axis. If \code{NULL}, minor ticks are not drawn. See Details for
  possible values.}

\item{grid.ticks.on}{period that specifies where vertical grid lines will be
  drawn. See Details for possible values.}

\item{grid.ticks.lwd}{line width of the grid}

\item{grid.ticks.lty}{line type of the grid}

\item{grid.col}{color of the grid}

\item{labels.col}{color of the axis labels}

\item{format.labels}{label format to draw lower frequency x-axis ticks and labels passed to \code{\link{axTicksByTime}}}

\item{grid2}{color for secondary x axis grid}

\item{legend.loc}{places a legend into one of nine locations on the chart:
bottomright, bottom, bottomleft, left, topleft, top, topright, right, or
center. Default NULL does not draw a legend.}

\item{pch}{the plotting character to use, same as in \code{\link{par}}.}

\item{on}{panel number to draw on. A new panel will be drawn if \code{on=NA}.
The default, \code{on=0}, will add to the active panel. The active panel is 
defined as the panel on which the most recent action was performed. Note that
only the first element of \code{on} is checked for the default behavior to
add to the last active panel.}

\item{extend.xaxis}{TRUE/FALSE (default FALSE). If TRUE, extend the x-axis
before and/or after the plot's existing time index range, so all of of the
time index values of the new series are included in the plot.}

}
\details{
  Possible values for arguments \code{major.ticks}, \code{minor.ticks},
  and \code{grid.ticks.on} include \sQuote{auto}, \sQuote{minute},
  \sQuote{hours}, \sQuote{days}, \sQuote{weeks}, \sQuote{months},
  \sQuote{quarters}, and \sQuote{years}. The default is \sQuote{auto}, which
  attempts to determine sensible locations from the periodicity and locations
  of observations. The other values are based on the possible values for the
  \code{ticks.on} argument of \code{\link{axTicksByTime}}.
}
\description{
Plotting for xts objects.
}
\author{
Ross Bennett
}
\references{
based on \code{chart_Series} in the \code{quantmod} package by Jeffrey A. Ryan
}
\seealso{
\code{\link{addSeries}}, \code{\link{addPanel}}
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

# plot the close and add a panel with the simple returns
plot(sample.xts[,"Close"])
R <- simple.ret(sample.xts, "Close")
lines(R, type="h", on=NA)

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
