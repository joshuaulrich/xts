\name{addPolygon}
\alias{addPolygon}
\title{Add a polygon to an existing xts plot}
\description{
Draw a polygon on an existing xts plot by specifying a time series of y 
coordinates. The xts index is used for the x coordinates and the first two 
columns are the upper and lower y coordinates, respectively.
}
\usage{
addPolygon(x, y = NULL, main = "", on = NA, col = NULL, ...)
}
\arguments{
  \item{x}{
  an xts object to plot. Must contain 2 columns for the upper and lower y
  coordinates for the polygon. The first column is interpreted as the upper y
  coordinates and the second column as the lower y coordinates.
}
  \item{y}{NULL, not used}
  \item{main}{main title for a new panel if drawn.}
  \item{on}{panel number to draw on. A new panel will be drawn if \code{on=NA}.}
  \item{col}{color palette to use, set by default to rational choices.}
  \item{\dots}{passthru parameters to \code{\link{par}}}
}
\examples{
\dontrun{
library(xts)
data(sample_matrix)
x <- as.xts(sample_matrix)[,1]
ix <- index(x["2007-02"])
shade <- xts(matrix(rep(range(x), each = length(ix)), ncol = 2), ix)

plot(x)

# set on = -1 to draw the shaded region *behind* the main series
addPolygon(shade, on = -1, col = "lightgrey")
}
}
\references{
Based on code by Dirk Eddelbuettel from \url{http://dirk.eddelbuettel.com/blog/2011/01/16/}
}
\author{Ross Bennett}
