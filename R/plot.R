`plot.xts` <- function(x, y=NULL,
                       type='l', auto.grid=TRUE,
                       major.ticks='auto', minor.ticks=TRUE, ...) {
  if(NCOL(x) > 1) warning('only the univariate series will be plotted')
  if(is.null(y))
    xycoords <- xy.coords(x)
  time.scale <- periodicity(x)$scale
  ep <- axTicksByTime(x,major.ticks)
  #ep <- c(rev(rev(ep)[-1]),rev(ep)[1]-1)
#  x.labels <- format(index(x)[ep],'%n%b%n%Y')
#  if(time.scale== "weekly" | time.scale == "daily") 
#      x.labels <- format(index(x)[ep], "%b %d%n%Y")
#  if(time.scale == "minute" | time.scale == "hourly") 
#      x.labels <- format(index(x)[ep], "%b %d%n%H:%M")
#
  plot(1:NROW(x),xycoords$y, type=type, axes=FALSE, ann=FALSE, ...)
  if(minor.ticks)
    axis(1, at=1:NROW(x), labels=FALSE, col='#BBBBBB')
  axis(1, at=ep, labels=names(ep), las=1, lwd=1, mgp=c(3,2,0)) 
  axis(2)
  box()

  dots <- list(...)
  if(!'main' %in% names(dots)) title(main=deparse(substitute(x)))
  do.call('title',list(...))

  if(auto.grid) {
    abline(v=ep, col='grey', lty=4)
    grid(NA,NULL)
  }

}
