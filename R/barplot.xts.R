barplot.xts <-
function(x, ..., ticks.on="auto", format.labels=TRUE, colors=c("green","red","lightgrey")) {
  plot.new()
  plot.window(c(1,NROW(x)), c(min(x,na.rm=TRUE),max(x,na.rm=TRUE)))
  ticks <- axTicksByTime(x, ticks.on=ticks.on, format.labels=format.labels)
  abline(v=ticks,col=col[3])
  axis(1, ticks, names(ticks))
  abline(h=axTicks(2), col=col[3])
  axis(2)
  rect(which(x>0)-0.5, 0, which(x>0)+0.5,x[x>0],col=col[1],border=col[3])
  rect(which(x<0)-0.5, 0, which(x<0)+0.5,x[x<0],col=col[2],border=col[3])
}

