`plot.xts` <- function(x, y=NULL,
                       type='l', auto.grid=TRUE,
                       major.ticks='auto', minor.ticks=TRUE, 
                       major.format=TRUE,
                       bar.col='grey', candle.col='white',
                       ann=TRUE, axes=TRUE, 
                       ...) {
  series.title <- deparse(substitute(x))

  if(is.null(y))
    xycoords <- xy.coords(x)
  time.scale <- periodicity(x)$scale
  ep <- axTicksByTime(x,major.ticks, format=major.format)

  otype <- type

  if(is.OHLC(x) && type %in% c('candles','bars')) {
    x <- x[,has.OHLC(x, TRUE)]
    plot(1:NROW(x),seq(min(x),max(x), length.out=NROW(x)), type="n", axes=FALSE, ann=FALSE, ...)
  } else {
    if(NCOL(x) > 1) warning('only the univariate series will be plotted')
    plot(1:NROW(x),xycoords$y, type=type, axes=FALSE, ann=FALSE, ...)
  }

  if(auto.grid) {
    abline(v=ep, col='grey', lty=4)
    grid(NA,NULL)
  }

  if(is.OHLC(x) && otype == 'candles')
    plot.ohlc.candles(x, bar.col=bar.col, candle.col=candle.col, ...)

  dots <- list(...)

  if('axes' %in% names(dots)) {
    if(!dots$axes) axes <- FALSE
  } else axes <- TRUE

  if(axes) {
    if(minor.ticks)
      axis(1, at=1:NROW(x), labels=FALSE, col='#BBBBBB')
    axis(1, at=ep, labels=names(ep), las=1, lwd=1, mgp=c(3,2,0)) 
    axis(2)
  }
  
  box()

  if(!'main' %in% names(dots)) title(main=series.title)
  do.call('title',list(...))

}

`plot.ohlc.candles` <-
function(x, width=0.2, order=1:4, bar.col='grey', candle.col='white',...) {
  # order = position of c(O,H,L,C)
  # draw HL bars
  segments(1:NROW(x), x[,order[2]], 1:NROW(x), x[,order[3]], col=bar.col,...)
  # draw candles
  rect(1:NROW(x)-width, x[,order[1]], 1:NROW(x)+ width, x[,order[4]], col=candle.col, ...)
}

`plot.ohlc.bars` <- function() {

}
