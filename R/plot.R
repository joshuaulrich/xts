#
#   xts: eXtensible time-series
#
#   Copyright (C) 2009-2015  Jeffrey A. Ryan jeff.a.ryan @ gmail.com
#
#   Contributions from Ross Bennett and Joshua M. Ulrich
#
#   This program is free software: you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation, either version 2 of the License, or
#   (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program.  If not, see <http://www.gnu.org/licenses/>.

axTicksByTime2 <- function (x, ticks.on = "auto", k = 1, labels = TRUE, 
                            format.labels = TRUE,  ends = TRUE, 
                            gt = 2, lt = 25){
  if (timeBased(x)) 
    x <- xts(rep(1, length(x)), x)

  tick.opts <- c("years", "months", "weeks", "days", "hours", "minutes", 
                 "seconds", "milliseconds", "microseconds")
  tick.k.opts <- rep(1, length(tick.opts))
  if (ticks.on %in% tick.opts) {
    cl <- ticks.on[1]
    ck <- k
  }
  else {
    tick.opts <- paste(tick.opts, tick.k.opts)
    is <- structure(rep(0, length(tick.opts)), .Names = tick.opts)
    for (i in 1:length(tick.opts)) {
      y <- strsplit(tick.opts[i], " ")[[1]]
      ep <- endpoints(x, y[1], as.numeric(y[2]))
      if(i>1 && is[i-1] == length(ep)-1)
        break
      is[i] <- length(ep) - 1
      if (is[i] > lt)
        break
    }
    nms <- rev(names(is)[which(is > gt & is < lt)])[1]
    cl <- strsplit(nms, " ")[[1]][1]
    ck <- as.numeric(strsplit(nms, " ")[[1]][2])
  }
  if (is.na(cl) || is.na(ck) || is.null(cl)) {
    return(c(1,NROW(x)))
  }
  else ep <- endpoints(x, cl, ck)
  if (ends) 
    ep <- ep + c(rep(1, length(ep) - 1), 0)
  if (labels) {
    if (is.logical(format.labels) || is.character(format.labels)) {
      unix <- ifelse(.Platform$OS.type == "unix", TRUE, FALSE)
      fmt <- switch(cl,
                    "years"="%Y",
                    "months"="%b",
                    "days"="%d",
                    "weeks"="W%W",
                    "hours"="%H:%M",
                    "minutes"="%H:%M:%S",
                    "seconds"="%H:%M:%S")
      if(ndays(x) > 1 && cl %in% c("hours","minutes","seconds")) {
        fmt <- paste("%b-%d",fmt)
      }
      names(ep) <- format(index(x)[ep], fmt)
    }
    else names(ep) <- as.character(index(x)[ep])
  }
  ep
}

current.xts_chob <- function() invisible(get(".xts_chob",.plotxtsEnv))

# Currently not necessary, but potentially very useful:
# http://www.fromthebottomoftheheap.net/2011/07/23/passing-non-graphical-parameters-to-graphical-functions-using/
chart.lines <- function(x, 
                        type="l", 
                        lty=1,
                        lwd=2,
                        lend=1,
                        col=NULL,
                        up.col=NULL, 
                        dn.col=NULL,
                        legend.loc=NULL,
                        ...){
  xx <- current.xts_chob()
  switch(type,
         h={
           # use up.col and dn.col if specified
           if (!is.null(up.col) && !is.null(dn.col)){
             colors <- ifelse(x[,1] < 0, dn.col, up.col)
           } else {
             colors <- if (is.null(col)) 1 else col
           }
           if (length(colors) < nrow(x[,1]))
               colors <- colors[1]
           lines(xx$Env$xycoords$x,x[,1],lwd=2,col=colors,lend=lend,lty=1,type="h",...)
         },
         p=, l=, b=, c=, o=, s=, S=, n={
           if(is.null(col))
             col <- xx$Env$theme$col

           if(length(lty) < NCOL(x)) lty <- rep(lty, length.out = NCOL(x))
           if(length(lwd) < NCOL(x)) lwd <- rep(lwd, length.out = NCOL(x))
           if(length(col) < NCOL(x)) col <- rep(col, length.out = NCOL(x))

           # ensure series only has index values in xdata subset
           xdataSubset <- xx$Env$xdata[xx$Env$xsubset]
           y <- merge(x, .xts(,.index(xdataSubset)), join = "right")
           xcoords <- xx$Env$xycoords$x

           for(i in NCOL(y):1) {
             lines(xcoords, y[,i], type=type, lend=lend, col=col[i],
                   lty=lty[i], lwd=lwd[i], ...)
           }
         },
         {
           # default case
           warning(paste(type, "not recognized. Type must be one of
                         'p', 'l', 'b, 'c', 'o', 'h', 's', 'S', 'n'.
                         plot.xts supports the same types as plot.default,
                         see ?plot for valid arguments for type"))
         }
        )

  if(!is.null(legend.loc)){
    lc <- legend.coords(legend.loc, xx$Env$xlim, range(x, na.rm=TRUE))
    legend(x=lc$x, y=lc$y, legend=colnames(x), xjust=lc$xjust, yjust=lc$yjust,
           fill=col[1:NCOL(x)], bty="n")
  }
}

add.par.from.dots <- function(call., ...) {
  stopifnot(is.call(call.))

  # from graphics:::.Pars
  parnames <- c("xlog","ylog","adj","ann","ask","bg","bty","cex","cex.axis",
                "cex.lab","cex.main","cex.sub","cin","col","col.axis","col.lab",
                "col.main","col.sub","cra","crt","csi","cxy","din","err",
                "family", "fg","fig","fin","font","font.axis","font.lab",
                "font.main","font.sub","lab","las","lend","lheight","ljoin",
                "lmitre","lty","lwd","mai","mar","mex","mfcol","mfg","mfrow",
                "mgp","mkh","new","oma","omd","omi","page","pch","pin","plt",
                "ps","pty","smo","srt","tck","tcl","usr","xaxp","xaxs","xaxt",
                "xpd","yaxp","yaxs","yaxt","ylbias")

  dots <- list(...)
  argnames <- names(dots)
  pm <- match(argnames, parnames, nomatch = 0L)

  call.list <- as.list(call.)
  # only pass the args from dots ('...') that are in parnames
  as.call(c(call.list, dots[pm > 0L]))
}


chart.lines.expression <- function(...) {
    mc <- match.call()
    mc[[1]] <- quote(chart.lines)
    as.expression(mc)
}

# Main plot.xts method.
# author: Ross Bennett (adapted from Jeffrey Ryan's chart_Series)
plot.xts <- function(x, 
                     y=NULL,
                     ...,
                     subset="",
                     panels=NULL,
                     multi.panel=FALSE,
                     col=1:8,
                     up.col=NULL,
                     dn.col=NULL,
                     bg="#FFFFFF",
                     type="l",
                     lty=1,
                     lwd=2,
                     lend=1,
                     main=deparse(substitute(x)),
                     observation.based=FALSE,
                     ylim=NULL,
                     yaxis.same=TRUE,
                     yaxis.left=TRUE,
                     yaxis.right=TRUE,
                     major.ticks="months",
                     minor.ticks=NULL,
                     grid.ticks.on="months",
                     grid.ticks.lwd=1,
                     grid.ticks.lty=1,
                     grid.col="darkgray",
                     labels.col="#333333",
                     format.labels=TRUE,
                     grid2="#F5F5F5",
                     legend.loc=NULL){
  
  # Small multiples with multiple pages behavior occurs when multi.panel is
  # an integer. (i.e. multi.panel=2 means to iterate over the data in a step
  # size of 2 and plot 2 panels on each page
  # Make recursive calls and return
  if(is.numeric(multi.panel)){
    multi.panel <- min(NCOL(x), multi.panel)
    idx <- seq.int(1L, NCOL(x), 1L)
    chunks <- split(idx, ceiling(seq_along(idx)/multi.panel))
    
    # allow color and line attributes for each panel in a multi.panel plot
    if(length(lty) < ncol(x)) lty <- rep(lty, length.out = ncol(x))
    if(length(lwd) < ncol(x)) lwd <- rep(lwd, length.out = ncol(x))
    if(length(col) < ncol(x)) col <- rep(col, length.out = ncol(x))
    
    
    if(!is.null(panels) && nchar(panels) > 0){
      # we will plot the panels, but not plot the data by column
      multi.panel <- FALSE
    } else {
      # we will plot the data by column, but not the panels
      multi.panel <- TRUE
      panels <- NULL
      
      # set the ylim based on the data passed into the x argument
      if(yaxis.same)
        ylim <- range(x[subset], na.rm=TRUE)
    }
    
    for(i in 1:length(chunks)){
      tmp <- chunks[[i]]
      p <- plot.xts(x=x[,tmp], 
                    y=y,
                    ...=...,
                    subset=subset,
                    panels=panels,
                    multi.panel=multi.panel,
                    col=col[tmp],
                    up.col=up.col,
                    dn.col=dn.col,
                    bg=bg,
                    type=type,
                    lty=lty[tmp],
                    lwd=lwd[tmp],
                    lend=lend,
                    main=main,
                    observation.based=observation.based,
                    ylim=ylim,
                    yaxis.same=yaxis.same,
                    yaxis.left=yaxis.left,
                    yaxis.right=yaxis.right,
                    major.ticks=major.ticks,
                    minor.ticks=minor.ticks,
                    grid.ticks.on=grid.ticks.on,
                    grid.ticks.lwd=grid.ticks.lwd,
                    grid.ticks.lty=grid.ticks.lty,
                    grid.col=grid.col,
                    labels.col=labels.col,
                    format.labels=format.labels,
                    grid2=grid2,
                    legend.loc=legend.loc)
      if(i < length(chunks))
        print(p)
    }
    # NOTE: return here so we don't draw another chart
    return(p)
  }
  
  cs <- new.replot_xts()
  # major.ticks shouldn't be null so we'll set major.ticks here if it is null
  if(is.null(major.ticks)) {
    xs <- x[subset]
    mt <- c(years=nyears(xs),
                    months=nmonths(xs),
                    days=ndays(xs))
    major.ticks <- names(mt)[rev(which(mt < 30))[1]]
  }

  # add theme and charting parameters to Env
  plot.call <- match.call(expand.dots=TRUE)
  if(isTRUE(multi.panel)){
    if(NCOL(x) == 1)
      cs$set_asp(3)
    else
      cs$set_asp(NCOL(x))
  } else {
    cs$set_asp(3)
  }
  cs$Env$cex <- if (hasArg("cex")) eval.parent(plot.call$cex) else 0.6
  cs$Env$mar <- if (hasArg("mar")) eval.parent(plot.call$mar) else c(3,2,0,2)
  cs$Env$theme$up.col <- up.col
  cs$Env$theme$dn.col <- dn.col
  
  # check for colorset or col argument
  # if col has a length of 1, replicate to NCOL(x) so we can keep it simple
  # and color each line by its index in col
  if(hasArg("colorset")) col <- eval.parent(plot.call$colorset)
  if(length(col) < ncol(x)) col <- rep(col, length.out = ncol(x))
  cs$Env$theme$col <- col
  
  cs$Env$theme$rylab <- yaxis.right
  cs$Env$theme$lylab <- yaxis.left
  cs$Env$theme$bg <- bg
  cs$Env$theme$grid <- grid.col
  cs$Env$theme$grid2 <- grid2
  cs$Env$theme$labels <- labels.col
  cs$Env$theme$srt <- if (hasArg("srt")) eval.parent(plot.call$srt) else 0
  cs$Env$theme$las <- if (hasArg("las")) eval.parent(plot.call$las) else 0
  cs$Env$theme$cex.axis <- if (hasArg("cex.axis")) eval.parent(plot.call$cex.axis) else 0.9
  cs$Env$format.labels <- format.labels
  cs$Env$major.ticks <- major.ticks
  cs$Env$minor.ticks <- minor.ticks
  cs$Env$grid.ticks.on <- grid.ticks.on
  cs$Env$grid.ticks.lwd <- grid.ticks.lwd
  cs$Env$grid.ticks.lty <- grid.ticks.lty
  cs$Env$type <- type
  
  # if lty or lwd has a length of 1, replicate to NCOL(x) so we can keep it 
  # simple and draw each line with attributes by index
  if(length(lty) < ncol(x)) lty <- rep(lty, length.out = ncol(x))
  if(length(lwd) < ncol(x)) lwd <- rep(lwd, length.out = ncol(x))
  cs$Env$lty <- lty
  cs$Env$lwd <- lwd
  
  cs$Env$lend <- lend
  cs$Env$legend.loc <- legend.loc
  cs$Env$call_list <- list()
  cs$Env$call_list[[1]] <- plot.call
  cs$Env$observation.based <- observation.based
  
  # Do some checks on x
  if(is.character(x))
    stop("'x' must be a time-series object")
  
  # Raw returns data passed into function
  cs$Env$xdata <- x
  cs$Env$xsubset <- subset
  cs$Env$column_names <- colnames(x)
  cs$Env$nobs <- NROW(cs$Env$xdata)
  cs$Env$main <- main
  
  # Set xlim using the raw returns data passed into function
  # xlim can be based on observations or time
  if(cs$Env$observation.based){
    # observation based x-axis
    cs$Env$xycoords <- xy.coords(1:NROW(cs$Env$xdata[subset]))
    cs$set_xlim(c(1,NROW(cs$Env$xdata[subset])))
    cs$Env$xstep <- 1
  } else {
    # time based x-axis
    xycoords <- xy.coords(.index(cs$Env$xdata[cs$Env$xsubset]), 
                          cs$Env$xdata[cs$Env$xsubset][,1])
    cs$Env$xycoords <- xycoords
    cs$Env$xlim <- range(xycoords$x, na.rm=TRUE)
    cs$Env$xstep <- diff(xycoords$x[1:2])
    # I don't think I need this because I already set cs$Env$xlim
    cs$set_xlim(cs$Env$xlim)
  }
  
  # chart_Series uses fixed=FALSE and add_* uses fixed=TRUE, not sure why or
  # which is best.
  if(is.null(ylim)){
    if(isTRUE(multi.panel)){
      if(yaxis.same){
        # set the ylim for the first panel based on all the data
        yrange <- range(cs$Env$xdata[subset], na.rm=TRUE)
      } else {
        # set the ylim for the first panel based on the first column
        yrange <- range(cs$Env$xdata[,1][subset], na.rm=TRUE)
      }
    } else {
      # set the ylim based on all the data if this is not a multi.panel plot
      yrange <- range(cs$Env$xdata[subset], na.rm=TRUE)
    }
    if(yrange[1L] == yrange[2L]) {
      if(yrange[1L] == 0) {
        yrange <- yrange + c(-1, 1)
      } else {
        yrange <- c(0.8, 1.2) * yrange[1L]
      }
    }
    cs$set_ylim(list(structure(yrange, fixed=TRUE)))
    cs$Env$constant_ylim <- range(cs$Env$xdata[subset], na.rm=TRUE)
  } else {
    # use the ylim arg passed in
    cs$set_ylim(list(structure(ylim, fixed=TRUE)))
    cs$Env$constant_ylim <- ylim
  }
  
  cs$set_frame(1,FALSE)
  
  # compute the x-axis ticks for the grid
  cs$add(expression(atbt <- axTicksByTime2(xdata[xsubset], ticks.on=grid.ticks.on),
                    segments(xycoords$x[atbt],
                             get_ylim()[[2]][1],
                             xycoords$x[atbt],
                             get_ylim()[[2]][2], 
                             col=theme$grid, lwd=grid.ticks.lwd, lty=grid.ticks.lty)),
         clip=FALSE,expr=TRUE)
  
  # Add frame for the chart "header" to display the name and start/end dates
  cs$add_frame(0,ylim=c(0,1),asp=0.5)
  cs$set_frame(1)
  
  # add observation level ticks on x-axis if < 400 obs.
  cs$add(expression(if(NROW(xdata[xsubset])<400) 
  {axis(1,at=xycoords$x,labels=FALSE,col=theme$grid2,col.axis=theme$grid2,tcl=0.3)}),expr=TRUE)
  
  # major x-axis ticks and labels
  cs$add(expression(axt <- axTicksByTime(xdata[xsubset], ticks.on=major.ticks, format.labels=format.labels),
                    axis(1,
                         at=xycoords$x[axt],
                         labels=names(axt),
                         las=theme$las, lwd.ticks=1.5, mgp=c(3,1.5,0), 
                         tcl=-0.4, cex.axis=theme$cex.axis, 
                         col=theme$labels, col.axis=theme$labels)),
         expr=TRUE)
  
  # minor x-axis ticks
  if(!is.null(minor.ticks)){
    cs$add(expression(axt <- axTicksByTime(xdata[xsubset], ticks.on=minor.ticks, format.labels=format.labels),
                      axis(1,
                           at=xycoords$x[axt],
                           labels=FALSE,
                           las=theme$las, lwd.ticks=0.75, mgp=c(3,1.5,0),
                           tcl=-0.4, cex.axis=theme$cex.axis,
                           col=theme$labels, col.axis=theme$labels)),
           expr=TRUE)
  }
  
  # add main title and date range of data
  text.exp <- c(expression(text(xlim[1],0.5,main,font=2,col=theme$labels,offset=0,cex=1.1,pos=4)),
                expression(text(xlim[2],0.5,
                                paste(start(xdata[xsubset]),end(xdata[xsubset]),sep=" / "),
                                col=theme$labels,adj=c(0,0),pos=2)))
  cs$add(text.exp, env=cs$Env, expr=TRUE)
  
  cs$set_frame(2)
  
  # define function to plot the y-axis grid lines
  cs$Env$y_grid_lines <- function(ylim) { 
    p <- pretty(ylim,5)
    p[p > ylim[1] & p < ylim[2]]
  }
  
  # add y-axis grid lines and labels
  exp <- expression(segments(xlim[1], 
                             y_grid_lines(get_ylim()[[2]]), 
                             xlim[2], 
                             y_grid_lines(get_ylim()[[2]]), 
                             col=theme$grid, lwd=grid.ticks.lwd, lty=grid.ticks.lty))
  if(yaxis.left){
    exp <- c(exp, 
             # left y-axis labels
             expression(text(xlim[1], y_grid_lines(get_ylim()[[2]]),
                             noquote(format(y_grid_lines(get_ylim()[[2]]), justify="right")),
                             col=theme$labels, srt=theme$srt, offset=1, pos=2,
                             cex=theme$cex.axis, xpd=TRUE)))
  }
  if(yaxis.right){
    exp <- c(exp, 
             # right y-axis labels
             expression(text(xlim[2], y_grid_lines(get_ylim()[[2]]),
                             noquote(format(y_grid_lines(get_ylim()[[2]]), justify="right")),
                             col=theme$labels, srt=theme$srt, offset=1, pos=4,
                             cex=theme$cex.axis, xpd=TRUE)))
  }
  cs$add(exp, env=cs$Env, expr=TRUE)
  
  # add main series
  cs$set_frame(2)
  if(isTRUE(multi.panel)){
    # We need to plot the first "panel" here because the plot area is
    # set up based on the code above
    lenv <- cs$new_environment()
    lenv$xdata <- cs$Env$xdata[subset,1]
    lenv$label <- colnames(cs$Env$xdata[,1])
    lenv$type <- cs$Env$type
    if(yaxis.same){
      lenv$ylim <- cs$Env$constant_ylim
    } else {
      lenv$ylim <- range(cs$Env$xdata[subset,1], na.rm=TRUE)
    }
    
    exp <- quote(chart.lines(xdata,
                             type=type, 
                             lty=lty,
                             lwd=lwd,
                             lend=lend,
                             col=theme$col, 
                             up.col=theme$up.col, 
                             dn.col=theme$dn.col,
                             legend.loc=legend.loc))
    exp <- as.expression(add.par.from.dots(exp, ...))

    # Add expression for the main plot
    cs$add(exp, env=lenv, expr=TRUE)
    text.exp <- expression(text(x=xycoords$x[2],
                                y=ylim[2]*0.9,
                                labels=label,
                                col=theme$labels,
                                adj=c(0,0),cex=1,offset=0,pos=4))
    cs$add(text.exp,env=lenv,expr=TRUE)
    
    if(NCOL(cs$Env$xdata) > 1){
      for(i in 2:NCOL(cs$Env$xdata)){
        # create a local environment
        lenv <- cs$new_environment()
        lenv$xdata <- cs$Env$xdata[subset,i]
        lenv$label <- cs$Env$column_names[i]
        if(yaxis.same){
          lenv$ylim <- cs$Env$constant_ylim
        } else {
          yrange <- range(cs$Env$xdata[subset,i], na.rm=TRUE)
          if(all(yrange == 0)) yrange <- yrange + c(-1,1)
          lenv$ylim <- yrange
        }
        lenv$type <- cs$Env$type
        
        # allow color and line attributes for each panel in a multi.panel plot
        lenv$lty <- cs$Env$lty[i]
        lenv$lwd <- cs$Env$lwd[i]
        lenv$col <- cs$Env$theme$col[i]
        
        # Add a small frame
        cs$add_frame(ylim=c(0,1),asp=0.25)
        cs$next_frame()
        text.exp <- expression(text(x=xlim[1],
                                    y=0.5,
                                    labels="",
                                    adj=c(0,0),cex=0.9,offset=0,pos=4))
        cs$add(text.exp, env=lenv, expr=TRUE)
        
        # Add the frame for the sub-plots
        cs$add_frame(ylim=lenv$ylim, asp=NCOL(cs$Env$xdata), fixed=TRUE)
        cs$next_frame()
        
        exp <- quote(chart.lines(xdata[xsubset],
                                 type=type, 
                                 lty=lty,
                                 lwd=lwd,
                                 lend=lend,
                                 col=col, 
                                 up.col=theme$up.col, 
                                 dn.col=theme$dn.col,
                                 legend.loc=legend.loc))
        exp <- as.expression(add.par.from.dots(exp, ...))
        
        # define function to plot the y-axis grid lines
        lenv$y_grid_lines <- function(ylim) { 
          p <- pretty(ylim,5)
          p[p > ylim[1] & p < ylim[2]]
        }
        
        # NOTE 'exp' was defined earlier as chart.lines
        exp <- c(exp, 
                 # y-axis grid lines
                 expression(segments(xlim[1],
                                     y_grid_lines(ylim),
                                     xlim[2], 
                                     y_grid_lines(ylim), 
                                     col=theme$grid, lwd=grid.ticks.lwd, lty=grid.ticks.lty)),
                 # x-axis grid lines
                 expression(atbt <- axTicksByTime2(xdata[xsubset], ticks.on=grid.ticks.on),
                            segments(xycoords$x[atbt],
                                     ylim[1],
                                     xycoords$x[atbt],
                                     ylim[2], 
                                     col=theme$grid, lwd=grid.ticks.lwd, lty=grid.ticks.lty)))
        if(yaxis.left){
          exp <- c(exp, 
                   # y-axis labels/boxes
                   expression(text(xlim[1], y_grid_lines(ylim),
                                   noquote(format(y_grid_lines(ylim),justify="right")),
                                   col=theme$labels, srt=theme$srt, offset=1,
                                   pos=2, cex=theme$cex.axis, xpd=TRUE)))
        }
        if(yaxis.right){
          exp <- c(exp, 
                   expression(text(xlim[2], y_grid_lines(ylim),
                                   noquote(format(y_grid_lines(ylim),justify="right")),
                                   col=theme$labels, srt=theme$srt, offset=1,
                                   pos=4, cex=theme$cex.axis, xpd=TRUE)))
        }
        cs$add(exp,env=lenv,expr=TRUE,no.update=TRUE)
        text.exp <- expression(text(x=xycoords$x[2],
                                    y=ylim[2]*0.9,
                                    labels=label,
                                    col=theme$labels,
                                    adj=c(0,0),cex=1,offset=0,pos=4))
        cs$add(text.exp,env=lenv,expr=TRUE)
      }
    }
  } else {
    if(type == "h" & NCOL(x) > 1) 
      warning("only the univariate series will be plotted")

    exp <- quote(chart.lines(xdata[xsubset],
                             type=type, 
                             lty=lty,
                             lwd=lwd,
                             lend=lend,
                             col=theme$col,
                             up.col=theme$up.col, 
                             dn.col=theme$dn.col,
                             legend.loc=legend.loc))
    exp <- as.expression(add.par.from.dots(exp, ...))
    cs$add(exp, expr=TRUE)

    assign(".xts_chob", cs, .plotxtsEnv)
  }
  
  # Plot the panels or default to a simple line chart
  if(!is.null(panels) && nchar(panels) > 0) {
    panels <- parse(text=panels, srcfile=NULL)
    for( p in 1:length(panels)) {
      if(length(panels[p][[1]][-1]) > 0) {
        cs <- eval(panels[p])
      } else {
        cs <- eval(panels[p])
      }
    }
  }
  assign(".xts_chob", cs, .plotxtsEnv)
  cs
}

# apply a function to the xdata in the xts chob and add a panel with the result
addPanel <- function(FUN, main="", on=NA, type="l", col=NULL, lty=1, lwd=1, pch=1, ...){
  # get the chob and the raw data (i.e. xdata)
  chob <- current.xts_chob()
  # xdata will be passed as first argument to FUN
  xdata <- chob$Env$xdata
  
  fun <- match.fun(FUN)
  .formals <- formals(fun)
  if("..." %in% names(.formals)) {
    # Just call do.call if FUN has '...'
    x <- try(do.call(fun, c(list(xdata), list(...)), quote=TRUE), silent=TRUE)
  } else {
    # Otherwise, ensure we only pass relevant args to FUN
    .formals <- modify.args(formals=.formals, arglist=list(...))
    .formals[[1]] <- quote(xdata)
    x <- try(do.call(fun, .formals), silent=TRUE)
  }

  if(inherits(x, "try-error")) {
    message(paste("FUN function failed with message", x))
    return(NULL)
  }
  
  addSeriesCall <- quote(addSeries(x = x, main = main, on = on,
    type = type, col = col, lty = lty, lwd = lwd, pch = pch))

  addSeriesCall <- add.par.from.dots(addSeriesCall, ...)
  eval(addSeriesCall)
}

# Add a time series to an existing xts plot
# author: Ross Bennett
addSeries <- function(x, main="", on=NA, type="l", col=NULL, lty=1, lwd=1, pch=1, ...){
  plot_object <- current.xts_chob()
  lenv <- plot_object$new_environment()
  lenv$main <- main
  lenv$plot_lines <- function(x, ta, on, type, col, lty, lwd, pch, ...){
    xdata <- x$Env$xdata
    xsubset <- x$Env$xsubset
    xDataSubset <- xdata[xsubset]
    if(all(is.na(on))){
      # Add x-axis grid lines
      atbt <- axTicksByTime2(xDataSubset, ticks.on=x$Env$grid.ticks.on)
      segments(x$Env$xycoords$x[atbt],
               par("usr")[3],
               x$Env$xycoords$x[atbt],
               par("usr")[4],
               col=x$Env$theme$grid)
    }
    # we can add points that are not necessarily at the points
    # on the main series
    if(xsubset == "") {
      subset.range <- xsubset
    } else {
      subset.range <- paste(start(xDataSubset), end(xDataSubset),sep="/")
    }
    ta.y <- merge(ta, .xts(,.index(xDataSubset), tzone=tzone(xdata)))[subset.range]
    chart.lines(ta.y, type=type, col=col, lty=lty, lwd=lwd, pch=pch, ...)
  }

  # get tag/value from dots
  expargs <- substitute(alist(ta=x,
                              on=on,
                              type=type,
                              col=col,
                              lty=lty,
                              lwd=lwd,
                              pch=pch,
                              ...))
  # capture values from caller, so we don't need to copy objects to lenv,
  # since this gives us evaluated versions of all the object values
  expargs <- lapply(expargs[-1L], eval, parent.frame())
  exp <- as.call(c(quote(plot_lines),
                   x = quote(current.xts_chob()),
                   expargs))

  plot_object$add_call(match.call())
  
  xdata <- plot_object$Env$xdata
  xsubset <- plot_object$Env$xsubset
  no.update <- FALSE
  lenv$xdata <- merge(x,xdata,retside=c(TRUE,FALSE))
  if(hasArg("ylim")) {
    ylim <- eval.parent(substitute(alist(...))$ylim)
  } else {
    ylim <- range(lenv$xdata[xsubset], na.rm=TRUE)
    if(all(ylim == 0)) ylim <- c(-1, 1)
  }
  lenv$ylim <- ylim
  
  if(is.na(on[1])){
    # add the frame for drawdowns info
    plot_object$add_frame(ylim=c(0,1),asp=0.25)
    plot_object$next_frame()
    text.exp <- expression(text(x=xlim[1], y=0.3, labels=main,
                                col=1,adj=c(0,0),cex=0.9,offset=0,pos=4))
    plot_object$add(text.exp, env=lenv, expr=TRUE)
    
    # add frame for the data
    plot_object$add_frame(ylim=ylim,asp=1,fixed=TRUE)
    plot_object$next_frame()
    
    # define function to plot the y-axis grid lines
    lenv$y_grid_lines <- function(ylim) { 
      p <- pretty(ylim,5)
      p[p > ylim[1] & p < ylim[2]]
    }
    
    # NOTE 'exp' was defined earlier as chart.lines
    exp <- c(exp, 
             # y-axis grid lines
             expression(segments(xlim[1],
                                 y_grid_lines(ylim),
                                 xlim[2], 
                                 y_grid_lines(ylim), 
                                 col=theme$grid, lwd=grid.ticks.lwd, lty=grid.ticks.lty)))
    if(plot_object$Env$theme$lylab){
      exp <- c(exp, 
               # y-axis labels/boxes
               expression(text(xlim[1]-xstep*2/3-max(strwidth(y_grid_lines(ylim))), 
                               y_grid_lines(ylim),
                               noquote(format(y_grid_lines(ylim),justify="right")),
                               col=theme$labels, srt=theme$srt, offset=0, 
                               pos=4, cex=theme$cex.axis, xpd=TRUE)))
    }
    if(plot_object$Env$theme$rylab){
      exp <- c(exp, 
               expression(text(xlim[2]+xstep*2/3, 
                               y_grid_lines(ylim),
                               noquote(format(y_grid_lines(ylim),justify="right")),
                               col=theme$labels, srt=theme$srt, offset=0,
                               pos=4, cex=theme$cex.axis, xpd=TRUE)))
    }
    plot_object$add(exp,env=lenv,expr=TRUE,no.update=TRUE)
  } else {
    for(i in 1:length(on)) {
      plot_object$set_frame(2*on[i]) # this is defaulting to using headers, should it be optionable?
      plot_object$add(exp,env=lenv,expr=TRUE,no.update=no.update)
    }
  }
  plot_object
}

# Add time series of lines to an existing xts plot
# author: Ross Bennett
lines.xts <- function(x, ..., main="", on=0, col=NULL, type="l", lty=1, lwd=1, pch=1){
  if(!is.na(on[1]))
    if(on[1] == 0) on[1] <- current_panel()
  
  addSeries(x, ...=..., main=main, on=on, type=type, col=col, lty=lty, lwd=lwd, pch=pch)
}

# Add time series of points to an existing xts plot
# author: Ross Bennett
points.xts <- function(x, ..., main="", on=0, col=NULL, pch=1){
  if(!is.na(on[1]))
    if(on[1] == 0) on[1] <- current_panel()
  
  addSeries(x, ...=..., main=main, on=on, type="p", col=col, pch=pch)
}

# Add vertical lines to an existing xts plot
# author: Ross Bennett
addEventLines <- function(events, main="", on=0, lty=1, lwd=1, col=1, ...){
  events <- try.xts(events)
  if(!is.na(on[1]))
    if(on[1] == 0) on[1] <- current_panel()
  
  if(nrow(events) > 1){
    if(length(lty) == 1) lty <- rep(lty, nrow(events))
    if(length(lwd) == 1) lwd <- rep(lwd, nrow(events))
    if(length(col) == 1) col <- rep(col, nrow(events))
  }
  
  plot_object <- current.xts_chob()
  lenv <- plot_object$new_environment()
  lenv$main <- main
  lenv$plot_event_lines <- function(x, events, on, lty, lwd, col, ...){
    xdata <- x$Env$xdata
    xsubset <- x$Env$xsubset

    if(all(is.na(on))){
      # Add x-axis grid lines
      atbt <- axTicksByTime2(xdata[xsubset], ticks.on=x$Env$grid.ticks.on)
      segments(x$Env$xycoords$x[atbt],
               par("usr")[3],
               x$Env$xycoords$x[atbt],
               par("usr")[4],
               col=x$Env$theme$grid)
    }
    ypos <- x$Env$ylim[[2*on]][2]*0.995
    # we can add points that are not necessarily at the points on the main series
    subset.range <- paste(start(xdata[xsubset]),
                          end(xdata[xsubset]),sep="/")
    ta.adj <- merge(n=.xts(1:NROW(xdata[xsubset]),
                           .index(xdata[xsubset]), 
                           tzone=tzone(xdata)),
                    .xts(rep(1, NROW(events)),# use numeric for the merge
                         .index(events)))[subset.range]
    # should we not merge and only add events that are in index(xdata)?
    ta.x <- as.numeric(na.approx(ta.adj[,1], rule=2) )
    ta.y <- ta.adj[,-1]
    # the merge should result in NAs for any object that is not in events
    event.ind <- which(!is.na(ta.y))
    abline(v=x$Env$xycoords$x[event.ind], col=col, lty=lty, lwd=lwd)
    text(x=x$Env$xycoords$x[event.ind], y=ypos, 
         labels=as.character(events[,1]), 
         col=x$Env$theme$labels, ...)
  }

  # get tag/value from dots
  expargs <- substitute(alist(events=events,
                              on=on,
                              lty=lty,
                              lwd=lwd,
                              col=col,
                              ...))
  # capture values from caller, so we don't need to copy objects to lenv,
  # since this gives us evaluated versions of all the object values
  expargs <- lapply(expargs[-1L], eval, parent.frame())
  exp <- as.call(c(quote(plot_event_lines),
                   x = quote(current.xts_chob()),
                   expargs))

  plot_object$add_call(match.call())
  
  if(is.na(on[1])){
    xdata <- plot_object$Env$xdata
    xsubset <- plot_object$Env$xsubset
    no.update <- FALSE
    lenv$xdata <- xdata
    ylim <- range(xdata[xsubset], na.rm=TRUE)
    lenv$ylim <- ylim
    
    # add the frame for drawdowns info
    plot_object$add_frame(ylim=c(0,1),asp=0.25)
    plot_object$next_frame()
    text.exp <- expression(text(x=xlim[1], y=0.3, labels=main,
                                col=1,adj=c(0,0),cex=0.9,offset=0,pos=4))
    plot_object$add(text.exp, env=lenv, expr=TRUE)
    
    # add frame for the data
    plot_object$add_frame(ylim=ylim,asp=1,fixed=TRUE)
    plot_object$next_frame()
    
    # define function to plot the y-axis grid lines
    lenv$y_grid_lines <- function(ylim) { 
      p <- pretty(ylim,5)
      p[p > ylim[1] & p < ylim[2]]
    }
    
    # NOTE 'exp' was defined earlier as chart.lines
    exp <- c(exp, 
             # y-axis grid lines
             expression(segments(xlim[1],
                                 y_grid_lines(ylim),
                                 xlim[2], 
                                 y_grid_lines(ylim), 
                                 col=theme$grid, lwd=grid.ticks.lwd, lty=grid.ticks.lty)))
    if(plot_object$Env$theme$lylab){
      exp <- c(exp, 
               # y-axis labels/boxes
               expression(text(xlim[1]-xstep*2/3-max(strwidth(y_grid_lines(ylim))), 
                               y_grid_lines(ylim),
                               noquote(format(y_grid_lines(ylim),justify="right")),
                               col=theme$labels, srt=theme$srt, offset=0, 
                               pos=4, cex=theme$cex.axis, xpd=TRUE)))
    }
    if(plot_object$Env$theme$rylab){
      exp <- c(exp, 
               expression(text(xlim[2]+xstep*2/3, 
                               y_grid_lines(ylim),
                               noquote(format(y_grid_lines(ylim),justify="right")),
                               col=theme$labels, srt=theme$srt, offset=0,
                               pos=4, cex=theme$cex.axis, xpd=TRUE)))
    }
    plot_object$add(exp,env=lenv,expr=TRUE,no.update=TRUE)
  } else {
    for(i in 1:length(on)) {
      ind <- on[i]
      no.update <- FALSE

      plot_object$set_frame(2*on[i]) # this is defaulting to using headers, should it be optionable?
      plot_object$add(exp,env=lenv,expr=TRUE,no.update=no.update)
    }
  }
  plot_object
}

# Add legend to an existing xts plot
# author: Ross Bennett
addLegend <- function(legend.loc="topright", legend.names=NULL, col=NULL, ncol=1, on=0, ...){
  if(!is.na(on[1]))
    if(on[1] == 0) on[1] <- current_panel()
  
  plot_object <- current.xts_chob()
  lenv <- plot_object$new_environment()
  lenv$plot_legend <- function(x, legend.loc, legend.names, col, ncol, on, bty, text.col, ...){
    if(is.na(on[1])){
      yrange <- c(0, 1)
    } else {
      yrange <- x$Env$ylim[[2*on]]
    }
    # this just gets the data of the main plot
    # TODO: get the data of frame[on]
    if(is.null(ncol)){
      ncol <- NCOL(x$Env$xdata)
    }
    if(is.null(col)){
      col <- x$Env$theme$col[1:NCOL(x$Env$xdata)]
    }
    if(is.null(legend.names)){
      legend.names <- x$Env$column_names
    }
    if(missing(bty)){
      bty <- "n"
    }
    if(missing(text.col)){
      text.col <- x$Env$theme$labels
    }
    lc <- legend.coords(legend.loc, x$Env$xlim, yrange)
    legend(x=lc$x, y=lc$y, legend=legend.names, xjust=lc$xjust, yjust=lc$yjust,
           ncol=ncol, col=col, bty=bty, text.col=text.col, ...)
  }
  
  # store the call
  plot_object$add_call(match.call())
  
  # get tag/value from dots
  expargs <- substitute(alist(legend.loc=legend.loc,
                              legend.names=legend.names,
                              col=col,
                              ncol=ncol,
                              on=on,
                              ...))
  # capture values from caller, so we don't need to copy objects to lenv,
  # since this gives us evaluated versions of all the object values
  expargs <- lapply(expargs[-1L], eval, parent.frame())
  exp <- as.call(c(quote(plot_legend),
                   x = quote(current.xts_chob()),
                   expargs))


  # if on[1] is NA, then add a new frame for the legend
  if(is.na(on[1])){
    # add frame for spacing
    plot_object$add_frame(ylim=c(0,1),asp=0.25)
    plot_object$next_frame()
    text.exp <- expression(text(x=xlim[1], y=0.3, labels=main,
                                col=theme$labels,adj=c(0,0),cex=0.9,offset=0,pos=4))
    plot_object$add(text.exp, env=lenv, expr=TRUE)
    
    # add frame for the legend panel
    plot_object$add_frame(ylim=c(0,1),asp=0.8,fixed=TRUE)
    plot_object$next_frame()
    
    # add plot_legend expression
    plot_object$add(exp,env=lenv,expr=TRUE,no.update=TRUE)
  } else {
    for(i in 1:length(on)) {
      ind <- on[i]
      no.update <- FALSE

      plot_object$set_frame(2*on[i]) # this is defaulting to using headers, should it be optionable?
      plot_object$add(exp,env=lenv,expr=TRUE,no.update=no.update)
    }
  }
  plot_object
}

# Determine legend coordinates based on legend location,
# range of x values and range of y values
legend.coords <- function(legend.loc, xrange, yrange) {
  switch(legend.loc,
        topleft = list(xjust = 0,   yjust = 1,   x = xrange[1], y = yrange[2]),
           left = list(xjust = 0,   yjust = 0.5, x = xrange[1], y = sum(yrange) / 2),
     bottomleft = list(xjust = 0,   yjust = 0,   x = xrange[1], y = yrange[1]),
            top = list(xjust = 0.5, yjust = 1,   x = (xrange[1] + xrange[2]) / 2, y = yrange[2]),
         center = list(xjust = 0.5, yjust = 0.5, x = (xrange[1] + xrange[2]) / 2, y = sum(yrange) / 2),
         bottom = list(xjust = 0.5, yjust = 0,   x = (xrange[1] + xrange[2]) / 2, y = yrange[1]),
       topright = list(xjust = 1,   yjust = 1,   x = xrange[2], y = yrange[2]),
          right = list(xjust = 1,   yjust = 0.5, x = xrange[2], y = sum(yrange) / 2),
    bottomright = list(xjust = 1,   yjust = 0,   x = xrange[2], y = yrange[1])
  )
}

# Add a polygon to an existing xts plot
# author: Ross Bennett
addPolygon <- function(x, y=NULL, main="", on=NA, col=NULL, ...){
  # add polygon to xts plot based on http://dirk.eddelbuettel.com/blog/2011/01/16/
  
  # some simple checks
  x <- try.xts(x)
  if(!is.null(y)) stop("y is not null")
  if(ncol(x) > 2) warning("more than 2 columns detected in x, only the first 2 will be used")
  
  plot_object <- current.xts_chob()
  lenv <- plot_object$new_environment()
  lenv$main <- main
  lenv$plot_lines <- function(x, ta, on, col, ...){
    xdata <- x$Env$xdata
    xsubset <- x$Env$xsubset
    if(is.null(col)) col <- x$Env$theme$col
    if(all(is.na(on))){
      # Add x-axis grid lines
      atbt <- axTicksByTime2(xdata[xsubset], ticks.on=x$Env$grid.ticks.on)
      segments(x$Env$xycoords$x[atbt],
               par("usr")[3],
               x$Env$xycoords$x[atbt],
               par("usr")[4],
               col=x$Env$theme$grid)
    }
    # we can add points that are not necessarily at the points
    # on the main series
    subset.range <- paste(start(xdata[xsubset]),
                          end(xdata[xsubset]),sep="/")
    ta.adj <- merge(n=.xts(1:NROW(xdata[xsubset]),
                           .index(xdata[xsubset]), 
                           tzone=tzone(xdata)),ta)[subset.range]
    ta.x <- as.numeric(na.approx(ta.adj[,1], rule=2) )
    # NAs in the coordinates break the polygon which is not the behavior we want
    ta.y <- na.omit(ta.adj[,-1])
    
    n <- NROW(ta.y)
    # x coordinates
    xx <- .index(ta.y)[c(1,1:n,n:1)]
    # y coordinates upper and lower
    # assume first column is upper and second column is lower y coords for
    # initial prototype
    yu <- as.vector(coredata(ta.y[,1]))
    yl <- as.vector(coredata(ta.y[,2]))
    polygon(x=xx, y=c(yl[1], yu, rev(yl)), border=NA, col=col, ...)
  }

  # get tag/value from dots
  expargs <- substitute(alist(ta=x,
                              col=col,
                              on=on,
                              ...))
  # capture values from caller, so we don't need to copy objects to lenv,
  # since this gives us evaluated versions of all the object values
  expargs <- lapply(expargs[-1L], eval, parent.frame())
  exp <- as.call(c(quote(plot_lines),
                   x = quote(current.xts_chob()),
                   expargs))

  plot_object$add_call(match.call())
  
  xdata <- plot_object$Env$xdata
  xsubset <- plot_object$Env$xsubset
  no.update <- FALSE
  lenv$xdata <- merge(x,xdata,retside=c(TRUE,FALSE))
  if(hasArg("ylim")) {
    ylim <- eval.parent(substitute(alist(...))$ylim)
  } else {
    ylim <- range(lenv$xdata[xsubset], na.rm=TRUE)
    if(all(ylim == 0)) ylim <- c(-1, 1)
  }
  lenv$ylim <- ylim
  
  if(is.na(on[1])){
    plot_object$add_frame(ylim=c(0,1),asp=0.25)
    plot_object$next_frame()
    text.exp <- expression(text(x=xlim[1], y=0.3, labels=main,
                                col=1,adj=c(0,0),cex=0.9,offset=0,pos=4))
    plot_object$add(text.exp, env=lenv, expr=TRUE)
    
    # add frame for the data
    plot_object$add_frame(ylim=ylim,asp=1,fixed=TRUE)
    plot_object$next_frame()
    
    # define function to plot the y-axis grid lines
    lenv$y_grid_lines <- function(ylim) { 
      p <- pretty(ylim,5)
      p[p > ylim[1] & p < ylim[2]]
    }
    
    # NOTE 'exp' was defined earlier as plot_lines
    exp <- c(exp, 
             # y-axis grid lines
             expression(segments(xlim[1],
                                 y_grid_lines(ylim),
                                 xlim[2], 
                                 y_grid_lines(ylim), 
                                 col=theme$grid, lwd=grid.ticks.lwd, lty=grid.ticks.lty)))
    if(plot_object$Env$theme$lylab){
      exp <- c(exp, 
               # y-axis labels/boxes
               expression(text(xlim[1], y_grid_lines(ylim),
                               noquote(format(y_grid_lines(ylim),justify="right")),
                               col=theme$labels, srt=theme$srt, offset=1,
                               pos=2, cex=theme$cex.axis, xpd=TRUE)))
    }
    if(plot_object$Env$theme$rylab){
      exp <- c(exp, 
               expression(text(xlim[2], y_grid_lines(ylim),
                               noquote(format(y_grid_lines(ylim),justify="right")),
                               col=theme$labels, srt=theme$srt, offset=1,
                               pos=4, cex=theme$cex.axis, xpd=TRUE)))
    }
    plot_object$add(exp,env=lenv,expr=TRUE,no.update=TRUE)
  } else {
    for(i in 1:length(on)) {
      plot_object$set_frame(2*on[i]) # this is defaulting to using headers, should it be optionable?
      plot_object$add(exp,env=lenv,expr=TRUE,no.update=no.update)
    }
  }
  plot_object
}# polygon


# R/replot.R in quantmod with only minor edits to change class name to
# replot_xts and use the .plotxtsEnv instead of the .plotEnv in quantmod

new.replot_xts <- function(frame=1,asp=1,xlim=c(1,10),ylim=list(structure(c(1,10),fixed=FALSE))) {
  # global variables
  Env <- new.env()
  Env$frame <- frame
  Env$asp   <- asp
  Env$xlim  <- xlim
  Env$ylim  <- ylim
  Env$pad1 <- -0 # bottom padding per frame
  Env$pad3 <-  0 # top padding per frame 
  if(length(asp) != length(ylim))
    stop("'ylim' and 'asp' must be the same length")
  
  
  # setters
  set_frame <- function(frame,clip=TRUE) { 
    Env$frame <<- frame; 
    set_window(clip); # change actual window
  }
  set_asp   <- function(asp) { Env$asp <<- asp }
  set_xlim  <- function(xlim) { Env$xlim <<- xlim }
  set_ylim  <- function(ylim) { Env$ylim <<- ylim }
  set_pad   <- function(pad) { Env$pad1 <<- pad[1]; Env$pad3 <<- pad[2] }
  reset_ylim <- function() {
    ylim <- get_ylim()
    ylim <- rep(list(c(Inf,-Inf)),length(ylim))

    lapply(Env$actions,
           function(x) {
             frame <- attr(x, "frame")
             if(frame > 0) {
               lenv <- attr(x,"env")
               if(is.list(lenv)) lenv <- lenv[[1]]
               ylim[[frame]][1] <<- min(ylim[[frame]][1],range(na.omit(lenv$xdata[Env$xsubset]))[1],na.rm=TRUE)
               ylim[[frame]][2] <<- max(ylim[[frame]][2],range(na.omit(lenv$xdata[Env$xsubset]))[2],na.rm=TRUE)
             }
           })
    # reset all ylim values, by looking for range(env[[1]]$xdata)
    # xdata should be either coming from Env or if lenv lenv
    set_ylim(ylim)
  }
  
  # getters
  get_frame <- function(frame) { Env$frame }
  get_asp   <- function(asp) { Env$asp }
  get_xlim  <- function(xlim) { Env$xlim }
  get_ylim  <- function(ylim) { Env$ylim }
  get_pad   <- function() c(Env$pad1,Env$pad3)
  
  # scale ylim based on current frame, and asp values
  scale_ranges <- function(frame, asp, ranges)
  {
    asp/asp[frame] * abs(diff(ranges[[frame]]))
  }
  # set_window prepares window for drawing
  set_window <- function(clip=TRUE,set=TRUE)
  {
    frame <- Env$frame
    frame <- abs(frame)
    asp   <- Env$asp
    xlim  <- Env$xlim
    ylim  <- lapply(Env$ylim, function(x) structure(x + (diff(x) * c(Env$pad1, Env$pad3)),fixed=attr(x,"fixed")))
    sr <- scale_ranges(frame, asp, ylim)
    if(frame == 1) {
      win <- list(xlim, c((ylim[[frame]][1] - sum(sr[-1])), ylim[[frame]][2]))
    } else
      if(frame == length(ylim)) {
        win <- list(xlim, c(ylim[[frame]][1], ylim[[frame]][2] + sum(sr[-length(sr)])))
      } else {
        win <- list(xlim, c(ylim[[frame]][1] - sum(sr[-(1:frame)]),
                            ylim[[frame]][2] + sum(sr[-(frame:length(sr))])))
      }
    if(!set) return(win)
    do.call("plot.window",win)
    if(clip) clip(par("usr")[1],par("usr")[2],ylim[[frame]][1],ylim[[frame]][2])
  }
  
  get_actions <- function(frame) {
    actions <- NULL
    for(i in 1:length(Env$actions)) {
      if(abs(attr(Env$actions[[i]],"frame"))==frame)
        actions <- c(actions, Env$actions[i])
    }
    actions
  }
  
  # add_frame:
  #   append a plot frame to the plot window
  add_frame <- function(after, ylim=c(0,0), asp=0, fixed=FALSE) {
    if(missing(after))
      after <- max(abs(sapply(Env$actions, function(x) attr(x,"frame"))))
    for(i in 1:length(Env$actions)) {
      cframe <- attr(Env$actions[[i]],"frame")
      if(cframe > 0 && cframe > after)
        attr(Env$actions[[i]], "frame") <- cframe+1L
      if(cframe < 0 && cframe < -after)
        attr(Env$actions[[i]], "frame") <- cframe-1L
    }
    Env$ylim <- append(Env$ylim,list(structure(ylim,fixed=fixed)),after)
    Env$asp  <- append(Env$asp,asp,after)
  }
  update_frames <- function(headers=TRUE) {
    # use subset code here, without the subset part.
    from_by <- ifelse(headers,2,1)  
    ylim <- get_ylim()
    for(y in seq(from_by,length(ylim),by=from_by)) {
      if(!attr(ylim[[y]],'fixed'))
        ylim[[y]] <- structure(c(Inf,-Inf),fixed=FALSE)
    }
    lapply(Env$actions,
           function(x) {
             if(!is.null(attr(x,"no.update")) && attr(x, "no.update"))
               return(NULL)
             frame <- abs(attr(x, "frame"))
             fixed <- attr(ylim[[frame]],'fixed')
             if(frame %% from_by == 0 && !fixed) {
               lenv <- attr(x,"env")
               if(is.list(lenv)) lenv <- lenv[[1]]
               dat.range <- range(na.omit(lenv$xdata[Env$xsubset]))
               min.tmp <- min(ylim[[frame]][1],dat.range,na.rm=TRUE)
               max.tmp <- max(ylim[[frame]][2],dat.range,na.rm=TRUE)
               ylim[[frame]] <<- structure(c(min.tmp,max.tmp),fixed=fixed)
             }
           })
    # reset all ylim values, by looking for range(env[[1]]$xdata)
    # xdata should be either coming from Env or if lenv, lenv
    set_ylim(ylim)
  }
  remove_frame <- function(frame) {
    rm.frames <- NULL
    max.frame <- max(abs(sapply(Env$actions, function(x) attr(x,"frame"))))
    for(i in 1:length(Env$actions)) {
      cframe <- attr(Env$actions[[i]],"frame")
      if(abs(attr(Env$actions[[i]],"frame"))==frame)
        rm.frames <- c(rm.frames, i)
      if(cframe > 0 && cframe > frame) {
        attr(Env$actions[[i]], "frame") <- cframe-1L
      }
      if(cframe < 0 && cframe < -frame) {
        attr(Env$actions[[i]], "frame") <- cframe+1L
      }
    }
    if(frame > max.frame) {
      Env$frame <- max.frame
    } else Env$frame <- max.frame-1
    Env$ylim <- Env$ylim[-frame]
    Env$asp  <- Env$asp[-frame]
    if(!is.null(rm.frames))
      Env$actions <- Env$actions[-rm.frames]
  }
  next_frame <- function() {
    set_frame(max(abs(sapply(Env$actions,function(x) attr(x,"frame"))))+1L)
  }
  move_frame   <- function() {}
  
  # actions
  Env$actions <- list()
  
  # aplot
  add <- replot <- function(x,env=Env,expr=FALSE,clip=TRUE,...) {
    if(!expr) {
      x <- match.call()$x
    } 
    a <- structure(x,frame=Env$frame,clip=clip,env=env,...)
    Env$actions[[length(Env$actions)+1]] <<- a
  }
  
  # subset function
  subset <- function(x="") {
    Env$xsubset <<- x
    set_xlim(range(Env$xycoords$x, na.rm=TRUE))
    ylim <- get_ylim()
    for(y in seq(2,length(ylim),by=2)) {
      if(!attr(ylim[[y]],'fixed'))
        ylim[[y]] <- structure(c(Inf,-Inf),fixed=FALSE)
    }
    lapply(Env$actions,
           function(x) {
             frame <- abs(attr(x, "frame"))
             fixed <- attr(ylim[[frame]],'fixed')
             if(frame %% 2 == 0 && !fixed) {
               lenv <- attr(x,"env")
               if(is.list(lenv)) lenv <- lenv[[1]]
               yrange <- range(lenv$xdata[Env$xsubset], na.rm=TRUE)
               if(all(yrange == 0)) yrange <- yrange + c(-1,1)
               min.tmp <- min(ylim[[frame]][1],yrange[1],na.rm=TRUE)
               max.tmp <- max(ylim[[frame]][2],yrange[2],na.rm=TRUE)
               ylim[[frame]] <<- structure(c(min.tmp,max.tmp),fixed=fixed)
             }
           })
    # reset all ylim values, by looking for range(env[[1]]$xdata)
    # xdata should be either coming from Env or if lenv, lenv
    set_ylim(ylim)
  }

  # calls
  add_call <- function(call.) {
    stopifnot(is.call(call.))
    ncalls <- length(Env$call_list)
    Env$call_list[[ncalls+1]] <- call.
  }

  # return
  replot_env <- new.env()
  class(replot_env) <- c("replot_xts","environment")
  replot_env$Env <- Env
  replot_env$set_window <- set_window
  replot_env$add <- add
  replot_env$replot <- replot
  replot_env$get_actions <- get_actions
  replot_env$subset <- subset
  replot_env$update_frames <- update_frames
  replot_env$set_frame <- set_frame
  replot_env$get_frame <- get_frame
  replot_env$next_frame <- next_frame
  replot_env$add_frame <- add_frame
  replot_env$remove_frame <- remove_frame
  replot_env$set_asp <- set_asp
  replot_env$get_asp <- get_asp
  replot_env$set_xlim <- set_xlim
  replot_env$get_xlim <- get_xlim
  replot_env$reset_ylim <- reset_ylim
  replot_env$set_ylim <- set_ylim
  replot_env$get_ylim <- get_ylim
  replot_env$set_pad <- set_pad
  replot_env$add_call <- add_call

  replot_env$new_environment <- function() { new.env(TRUE, Env) }
  return(replot_env)
}

str.replot_xts <- function(x, ...) {
  print(str(unclass(x)))
}

print.replot_xts <- function(x, ...) plot(x,...)
plot.replot_xts <- function(x, ...) {
  # must set the background color before calling plot.new
  obg <- par(bg = x$Env$theme$bg)
  plot.new()
  assign(".xts_chob",x,.plotxtsEnv)

  # only reasonable way to fix X11/quartz issue
  ocex <- par(cex = if(.Device == "X11") x$Env$cex else x$Env$cex * 1.5)
  omar <- par(mar = x$Env$mar)
  oxpd <- par(xpd = FALSE)
  usr <- par("usr")

  # plot negative (underlay) actions
  last.frame <- x$get_frame()
  x$update_frames()
  lapply(x$Env$actions,
         function(aob) {
           if(attr(aob,"frame") < 0) {
             x$set_frame(attr(aob,"frame"),attr(aob,"clip"))
             env <- attr(aob,"env")
             if(is.list(env)) {
               # if env is c(env, Env), convert to list
               env <- unlist(lapply(env, function(x) eapply(x, eval)),recursive=FALSE)
             }
             eval(aob, env)
           }
         }
  )
  # plot positive (overlay) actions
  lapply(x$Env$actions,
         function(aob) {
           if(attr(aob,"frame") > 0) {
             x$set_frame(attr(aob,"frame"),attr(aob,"clip"))
             env <- attr(aob,"env")
             if(is.list(env)) {
               env <- unlist(lapply(env, function(x) eapply(x, eval)),recursive=FALSE)
             }
             eval(aob, env)
           }
         }
  )

  x$set_frame(abs(last.frame),clip=FALSE)
  do.call("clip", as.list(usr))  # reset clipping region
  # reset par
  par(xpd = oxpd$xpd, cex = ocex$cex, mar = omar$mar, bg = obg$bg)
  invisible(x$Env$actions)
}

actions <- function(obj) obj$Env$actions
chart_actions <- function() actions(current.xts_chob())

current_panel <- function() {
  act <- chart_actions()
  # we need to divide by 2 because there are 2 frames per panel
  attr(act[[length(act)]], "frame") / 2
}
