`plot.xts` <- function(x, y=NULL,
                       type='l', auto.grid=TRUE,
                       major.ticks='auto', minor.ticks=TRUE, ...) {
  if(NCOL(x) > 1) warning('only the univariate series will be plotted')
  if(is.null(y))
    xycoords <- xy.coords(x)
  time.scale <- periodicity(x)$scale
  ticks <- function(ival, major.ticks, gt=2, lt=30) {
    tick.opts <- c('years','months','weeks','days',
                   'hours','minutes','seconds')
    if(major.ticks %in% tick.opts) {
      cl <- major.ticks[1]
    } else {
      is <- sapply(tick.opts,
                   function(y) {
                     length(endpoints(ival,y,1))-1
                   })
      cl <- names(is)[which(is > gt & is < lt)][1]
    }
    ep <- endpoints(ival,cl)
    ep 
  }
  ep <- ticks(x,major.ticks)
  x.labels <- format(index(x)[ep+1],'%n%b%n%Y')
  if(time.scale== "weekly" | time.scale == "daily") 
      x.labels <- format(index(x)[ep + 1], "%b %d%n%Y")
  if(time.scale == "minute") 
      x.labels <- format(index(x)[ep + 1], "%b %d%n%H:%M")

  plot(1:NROW(x),xycoords$y, type=type, axes=FALSE, ann=FALSE, ...)
  if(minor.ticks)
    axis(1, at=1:NROW(x), labels=FALSE, col='#BBBBBB')
  axis(1, at=ep+1, labels=x.labels, las=1, lwd=1, mgp=c(3,2,0)) 
  axis(2)
  box()

  dots <- list(...)
  if(!'main' %in% names(dots)) title(main=deparse(substitute(x)))
  do.call('title',list(...))

  if(auto.grid) {
    abline(v=ep+1, col='grey', lty=4)
    grid(NA,NULL)
  }

}
