\name{addPanel}
\alias{addPanel}
\title{Add a panel to an existing xts plot}
\usage{
addPanel(FUN, main = "", on = NA, type = "l", col = NULL, lty = 1,
  lwd = 1, pch = 1, ...)
}
\arguments{
\item{FUN}{an xts object to plot.}

\item{main}{main title for a new panel if drawn.}

\item{on}{panel number to draw on. A new panel will be drawn if \code{on=NA}.}

\item{type}{the type of plot to be drawn, same as in \code{\link{plot}}.}

\item{col}{color palette to use, set by default to rational choices.}

\item{lty}{set the line type, same as in \code{\link{par}}.}

\item{lwd}{set the line width, same as in \code{\link{par}}.}

\item{pch}{the type of plot to be drawn, same as in \code{\link{par}}.}

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
\examples{
library(xts)
data(sample_matrix)
sample.xts <- as.xts(sample_matrix)

calcReturns <- function(price, method = c("discrete", "log")){
  px <- try.xts(price)
  method <- match.arg(method)[1L]
  returns <- switch(method,
    simple = ,
    discrete = px / lag(px) - 1,
    compound = ,
    log = diff(log(px)))
  reclass(returns, px)
}

# plot the Close
plot(sample.xts[,"Close"])
# calculate returns 
addPanel(calcReturns, method="discrete", type="h")
# Add simple moving average to panel 1
addPanel(rollmean, k=20, on=1)
addPanel(rollmean, k=40, col="blue", on=1)
}
