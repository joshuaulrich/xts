\name{plot.xts}
\alias{plot.xts}
\title{ Plotting xts Objects }
\description{
Plotting methods for xts objects.
}
\usage{
\method{plot}{xts}(x, y = NULL,
         type = "l", 
         auto.grid = TRUE,
         major.ticks = "auto",
         minor.ticks = TRUE,
         bar.col = "grey",
         candle.col = "white",
         ann = TRUE, axes = TRUE, ...)
}
\arguments{
  \item{x}{ an \code{xts} object }
  \item{y}{ an \code{xts} object or NULL }
  \item{type}{ type of plot to produce }
  \item{auto.grid}{ should grid lines be drawn }
  \item{major.ticks}{ should major tickmarks be drawn and labeled }
  \item{minor.ticks}{ should minor tickmarks be drawn }
  \item{bar.col}{ the color of the bars when type is \sQuote{bars} or \sQuote{candles} }
  \item{candle.col}{ the color of the candles when type is \sQuote{candles} }
  \item{ann}{ passed \sQuote{par} graphical parameter }
  \item{axes}{passed \sQuote{par} graphical parameter }
  \item{\dots}{ additional graphical arguments }
}
\details{
Mainly used to draw time-series plots with sensible x-axis labels, it
can also plot basic OHLC series using \code{type='candles'} or \code{type='bars'}.

Better financial plots can be found in the \pkg{quantmod} package, though
these are generally incompatible with standard R graphics tools.
}
\value{
Plots an xts object to the current device.
}
\author{ Jeffrey A. Ryan }
\examples{
data(sample_matrix)
plot(sample_matrix)
plot(as.xts(sample_matrix))
plot(as.xts(sample_matrix), type='candles')
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
