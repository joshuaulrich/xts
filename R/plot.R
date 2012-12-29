#
#   xts: eXtensible time-series 
#
#   Copyright (C) 2008  Jeffrey A. Ryan jeff.a.ryan @ gmail.com
#
#   Contributions from Joshua M. Ulrich
#
#   This program is free software: you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation, either version 3 of the License, or
#   (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program.  If not, see <http://www.gnu.org/licenses/>.


`plot.xts` <- function(x, y=NULL,
                       type='l', auto.grid=TRUE,
                       major.ticks='auto', minor.ticks=TRUE, 
                       major.format=TRUE,
                       bar.col='grey', candle.col='white',
                       ann=TRUE, axes=TRUE,
                       ...) {
  series.title <- deparse(substitute(x))

  #time.scale <- periodicity(x)$scale
  ep <- axTicksByTime(x,major.ticks, format.labels=major.format)

  otype <- type

  if(is.OHLC(x) && type %in% c('candles','bars')) {
    x <- x[,has.OHLC(x, TRUE)]
    xycoords <- list(x=.index(x), y=seq(min(x),max(x),length.out=NROW(x)))
    type <- "n"
  } else {
    if(NCOL(x) > 1) warning('only the univariate series will be plotted')
    if(is.null(y))
      xycoords <- xy.coords(.index(x), x[,1])
  }

  plot(xycoords$x, xycoords$y, type=type, axes=FALSE, ann=FALSE, ...)

  if(auto.grid) {
    abline(v=xycoords$x[ep], col='grey', lty=4)
    grid(NA,NULL)
  }

  if(is.OHLC(x) && otype == 'candles')
    plot.ohlc.candles(x, bar.col=bar.col, candle.col=candle.col, ...)

  dots <- list(...)

#  if('axes' %in% names(dots)) {
#    if(!dots$axes) axes <- FALSE
#  } else axes <- TRUE

  if(axes) {
    if(minor.ticks)
      axis(1, at=xycoords$x, labels=FALSE, col='#BBBBBB', ...)
    axis(1, at=xycoords$x[ep], labels=names(ep), las=1, lwd=1, mgp=c(3,2,0),...) 
    axis(2, ...)
  }
  
  box()

  if(!'main' %in% names(dots)) title(main=series.title)
  do.call('title',list(...))
  assign(".plot.xts",recordPlot(),.xtsEnv)
}

.lines.xts <- function(x, ...)
{
  getxvalues <- function(x) x[[1]][[3]][[2]][[1]]$x
  last.plot <- if(exists(".plot.xts",.xtsEnv)) {
                 get(".plot.xts", .xtsEnv) } else NULL
  if( identical(getxvalues(last.plot), getxvalues(recordPlot())) ) {
    lines.default(getxvalues(recordPlot()),
                  x, ...)
  } else zoo:::lines.zoo(x, ...)
  assign(".plot.xts",recordPlot(),.xtsEnv)
}


`plot.ohlc.candles` <-
function(x, width=0.2, order=1:4, bar.col='grey', candle.col='white',...) {
  # order = position of c(O,H,L,C)
  # draw HL bars
  segments(.index(x), x[,order[2]], .index(x), x[,order[3]], col=bar.col,...)
  # draw candles
  rect(.index(x) - width, x[,order[1]], .index(x) + width, x[,order[4]], col=candle.col, ...)
}

`plot.ohlc.bars` <- function() {

}

lines.xts <- function(x, y=NULL, type="l", ...) {
  lines(.index(x), coredata(x), type=type, ...)
}

points.xts <- function(x, y=NULL, type="p", ...) {
  points(.index(x), coredata(x), type=type, ...)
}
