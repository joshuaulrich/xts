
# Environment for our xts chart objects (xts_chob)
# .plotxtsEnv <- new.env()

axTicksByTime2 <- function (x, ticks.on = "auto", k = 1, labels = TRUE, 
                            format.labels = TRUE,  ends = TRUE, 
                            gt = 2, lt = 25){
  if (timeBased(x)) 
    x <- xts(rep(1, length(x)), x)
  #tick.opts <- c("years", "months", "days", "hours", 
  #    "minutes", "seconds")
  tick.opts <- c("years", "months", "weeks", "days")
  tick.k.opts <- c(1,1,1,1)
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
    #ep <- NULL
  }
  else ep <- endpoints(x, cl, ck)
  if (ends) 
    ep <- ep + c(rep(1, length(ep) - 1), 0)
  if (labels) {
    if (is.logical(format.labels) || is.character(format.labels)) {
      unix <- ifelse(.Platform$OS.type == "unix", TRUE, 
                     FALSE)
      #time.scale <- periodicity(x)$scale
      #fmt <- ifelse(unix, "%n%b%n%Y", "%b %Y")
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

chart.lines <- function(x, 
                        type="l", 
                        lty=1,
                        lwd=2,
                        lend=1,
                        col=1:10, 
                        up.col=NULL, 
                        dn.col=NULL,
                        legend.loc=NULL,
                        pch=1){
  if(is.null(up.col)) up.col <- "green"
  if(is.null(dn.col)) dn.col <- "red"
  xx <- current.xts_chob()
  if(type == "h"){
    colors <- ifelse(x[,1] < 0, dn.col, up.col)
    # lines(1:NROW(x),x[,1],lwd=2,col=colors,lend=lend,lty=1,type="h")
    # non-equally spaced x-axis
    lines(xx$Env$xycoords$x,x[,1],lwd=2,col=colors,lend=lend,lty=1,type="h")
  } else if(type == "l" || type == "p") {
    if(length(lty) == 1) lty <- rep(lty, NCOL(x))
    if(length(lwd) == 1) lwd <- rep(lwd, NCOL(x))
    for(i in NCOL(x):1){
      # lines(1:NROW(x), x[,i], type=type, lend=lend, col=col[i], lty=lty[i], lwd=lwd[i], pch=pch)
      # non-equally spaced x-axis
      lines(xx$Env$xycoords$x, x[,i], type=type, lend=lend, col=col[i], lty=lty[i], lwd=lwd[i], pch=pch)
    }
  } else if(type == "bar"){
    # This does not work correctly
    # The geometry of the x-axis and y-axis is way off with stacked bar plot and
    # the x-axis is off for unstacked bar plot
    # We may need a separate function to do this correctly because of the
    # different geometry/dimensions with stacked and unstacked barplots
    positives = negatives = x
    for(column in 1:NCOL(x)){
      for(row in 1:NROW(x)){ 
        positives[row,column] = max(0, x[row,column])
        negatives[row,column] = min(0, x[row,column])
      }
    }
    barplot.default(t(positives), add=TRUE, col=col, axisnames=FALSE, axes=FALSE)
    barplot.default(t(negatives), add=TRUE, col=col, axisnames=FALSE, axes=FALSE)
  }
  if(!is.null(legend.loc)){
    yrange <- range(x, na.rm=TRUE)
    # nobs <- NROW(x)
    chob.xlim <- xx$Env$xlim
    switch(legend.loc,
           topleft = {
             xjust <- 0
             yjust <- 1
             lx <- chob.xlim[1]
             ly <- yrange[2]
           },
           left = {
             xjust <- 0
             yjust <- 0.5
             lx <- chob.xlim[1]
             ly <- sum(yrange) / 2
           },
           bottomleft = {
             xjust <- 0
             yjust <- 0
             lx <- chob.xlim[1]
             ly <- yrange[1]
           },
           top = {
             xjust <- 0.5
             yjust <- 1
             lx <- (chob.xlim[1] + chob.xlim[2]) / 2
             ly <- yrange[2]
           },
           center = {
             xjust <- 0.5
             yjust <- 0.5
             lx <- (chob.xlim[1] + chob.xlim[2]) / 2
             ly <- sum(yrange) / 2
           },
           bottom = {
             xjust <- 0.5
             yjust <- 0
             lx <- (chob.xlim[1] + chob.xlim[2]) / 2
             ly <- yrange[1]
           },
           topright = {
             xjust <- 1
             yjust <- 1
             lx <- chob.xlim[2]
             ly <- yrange[2]
           },
           right = {
             xjust <- 1
             yjust <- 0.5
             lx <- chob.xlim[2]
             ly <- sum(yrange) / 2
           },
           bottomright = {
             xjust <- 1
             yjust <- 0
             lx <- chob.xlim[2]
             ly <- yrange[1]
           }
    )
    legend(x=lx, y=ly, legend=colnames(x), xjust=xjust, yjust=yjust, 
           fill=col[1:NCOL(x)], bty="n")
  }
}


# xtsExtraTheme <- function(){
#   theme <-list(col=list(bg="#FFFFFF",
#                         label.bg="#F0F0F0",
#                         grid="darkgray", #grid="#F0F0F0",
#                         grid2="#F5F5F5",
#                         ticks="#999999",
#                         labels="#333333",
#                         line.col="darkorange",
#                         dn.col="red",
#                         up.col="green", 
#                         dn.border="#333333", 
#                         up.border="#333333",
#                         colorset=1:10),
#                shading=1,
#                format.labels=TRUE,
#                coarse.time=TRUE,
#                rylab=TRUE,
#                lylab=TRUE,
#                grid.ticks.lwd=1,
#                grid.ticks.on="months")
#   theme
# }

# based on quantmod::chart_Series
#' Time series Plotting
#' 
#' Plotting for xts objects.
#' TODO: description, details, and examples
#' 
#' @param x xts object
#' @param y NULL, not used
#' @param \dots any passthrough parameters to FUN
#' @param subset character vector of length one of the subset range using subsetting as in \code{\link{xts}}
#' @param FUN function to apply to \code{x} and plot
#' @param panels character vector of expressions to plot as panels
#' @param multi.panel TRUE/FALSE or an integer less than or equal to the number 
#' of columns in the data set. If TRUE, each column of the data is plotted in a 
#' separate panel. For example, if \code{multi.panel = 2}, then the data
#' will be plotted in groups of 2 columns and each group is plotted in a 
#' separate panel. 
#' @param col color palette to use, set by default to rational choices
#' @param up.col color for positive bars if \code{type="h"}
#' @param dn.col color for positive bars if \code{type="h"}
#' @param type the type of plot to be drawn, same as in \code{\link{plot}}
#' @param lty set the line type, same as in plot
#' @param lwd set the line width, same as in plot
#' @param lend set the line end style, same as in plot
#' @param main main title
#' @param clev level for shading, not currently used
#' @param cex not currently used
#' @param cex.axis
#' @param mar set the margins, same as in par
#' @param srt rotation for the y axis labels
#' @param xaxis.las rotation for the x axis labels
#' @param ylim the range of the y axis
#' @param yaxis.same TRUE/FALSE. If TRUE, the y axis is drawn with the same ylim for multiple panels 
#' @param yaxis.left if TRUE, draws the y axis on the left
#' @param yaxis.right if TRUE, draws the y axis on the right
#' @param grid.ticks.on period to draw the grid ticks on
#' @param grid.ticks.lwd line width of the grid
#' @param grid.ticks.lty line type of the grid
#' @param grid.col color of the grid
#' @param labels.col color of the axis labels
#' @param format.labels not currently used
#' @param shading not currently used
#' @param bg.col not currently used
#' @param grid2 color for secondary x axis grid
#' @param legend.loc places a legend into one of nine locations on the chart: 
#' bottomright, bottom, bottomleft, left, topleft, top, topright, right, or 
#' center. Default NULL does not draw a legend. 
#' @author Ross Bennett
plot.xts <- function(x, 
                     y=NULL,
                     ...,
                     subset="",
                     FUN=NULL,
                     panels=NULL,
                     multi.panel=FALSE,
                     col=1:12,
                     up.col="green",
                     dn.col="red",
                     type="l",
                     lty=1,
                     lwd=2,
                     lend=1,
                     main=deparse(substitute(x)),  
                     clev=0,
                     cex=0.6, 
                     cex.axis=0.9,
                     mar=c(3,2,0,2), 
                     srt=0,
                     xaxis.las=0,
                     ylim=NULL,
                     yaxis.same=TRUE,
                     yaxis.left=TRUE,
                     yaxis.right=TRUE,
                     grid.ticks.on="months",
                     grid.ticks.lwd=1,
                     grid.ticks.lty=1,
                     grid.col="darkgray",
                     labels.col="#333333",
                     format.labels=TRUE,
                     shading=1,
                     bg.col="#FFFFFF",
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
    
    if(!is.null(panels) && nchar(panels) > 0){
      # we will plot the panels, but not plot the returns by column
      multi.panel <- FALSE
    } else {
      # we will plot the returns by column, but not the panels
      multi.panel <- TRUE
      panels <- NULL
      
      if(yaxis.same){
        # If we want the same y-axis and a FUN is specified, we need to
        # apply the transformation first to compute the range for the y-axis
        if(!is.null(FUN) && nchar(FUN) > 0){
          fun <- match.fun(FUN)
          .formals <- formals(fun)
          .formals <- modify.args(formals=.formals, arglist=list(...), dots=TRUE)
          if("R" %in% names(.formals)) .formals <- modify.args(formals=.formals, arglist=NULL, R=x, dots=TRUE)
          .formals$... <- NULL
          R <- try(do.call(fun, .formals), silent=TRUE)
          if(inherits(R, "try-error")) { 
            message(paste("FUN function failed with message", R))
            ylim <- range(x[subset], na.rm=TRUE)
          } else {
            ylim <- range(R[subset], na.rm=TRUE)
          }
        } else {
          # set the ylim based on the data passed into the x argument
          ylim <- range(x[subset], na.rm=TRUE)
        }
      }
    }
    
    for(i in 1:length(chunks)){
      tmp <- chunks[[i]]
      p <- plot.xts(x=x[,tmp], 
                    y=y,
                    ...=...,
                    subset=subset,
                    FUN=FUN,
                    panels=panels,
                    multi.panel=multi.panel,
                    col=col,
                    up.col=up.col,
                    dn.col=dn.col,
                    type=type,
                    lty=lty,
                    lwd=lwd,
                    lend=lend,
                    main=main,  
                    clev=clev,
                    cex=cex, 
                    cex.axis=cex.axis,
                    mar=mar, 
                    srt=srt,
                    xaxis.las=xaxis.las,
                    ylim=ylim,
                    yaxis.same=yaxis.same,
                    yaxis.left=yaxis.left,
                    yaxis.right=yaxis.right,
                    grid.ticks.on=grid.ticks.on,
                    grid.ticks.lwd=grid.ticks.lwd,
                    grid.ticks.lty=grid.ticks.lty,
                    grid.col=grid.col,
                    labels.col=labels.col,
                    format.labels=format.labels,
                    shading=shading,
                    bg.col=bg.col,
                    grid2=grid2,
                    legend.loc=legend.loc)
      if(i < length(chunks))
        print(p)
    }
    # NOTE: return here so we don't draw another chart
    return(p)
  }
  
  cs <- new.replot_xts()
  if(is.null(grid.ticks.on)) {
    xs <- x[subset]
    major.grid <- c(years=nyears(xs),
                    months=nmonths(xs),
                    days=ndays(xs))
    grid.ticks.on <- names(major.grid)[rev(which(major.grid < 30))[1]]
  } #else grid.ticks.on <- theme$grid.ticks.on
  #label.bg <- theme$col$label.bg
  
  # define a subset function
  cs$subset <- function(x) {
    if(FALSE) {set_ylim <- get_ylim <- set_xlim <- Env <-function(){} }  # appease R parser?
    if(missing(x)) {
      x <- "" #1:NROW(Env$xdata)
    }
    Env$xsubset <<- x
    # set_xlim(c(1,NROW(Env$xdata[Env$xsubset])))
    # non equally spaced x-axis
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
             #fixed <- attr(x, "fixed")
             if(frame %% 2 == 0 && !fixed) {
               lenv <- attr(x,"env")
               if(is.list(lenv)) lenv <- lenv[[1]]
               min.tmp <- min(ylim[[frame]][1],range(lenv$xdata[Env$xsubset], na.rm=TRUE)[1],na.rm=TRUE)
               max.tmp <- max(ylim[[frame]][2],range(lenv$xdata[Env$xsubset], na.rm=TRUE)[2],na.rm=TRUE)
               ylim[[frame]] <<- structure(c(min.tmp,max.tmp),fixed=fixed)
             }
           })
    # reset all ylim values, by looking for range(env[[1]]$xdata)
    # xdata should be either coming from Env or if lenv, lenv
    set_ylim(ylim)
  }
  environment(cs$subset) <- environment(cs$get_asp)
  
  # add theme and charting parameters to Env
  if(multi.panel){
    cs$set_asp(NCOL(x))
  } else {
    cs$set_asp(3)
  }
  cs$Env$cex <- cex
  cs$Env$mar <- mar
  cs$Env$clev = min(clev+0.01,1) # (0,1]
  cs$Env$theme$shading <- shading
  cs$Env$theme$up.col <- up.col
  cs$Env$theme$dn.col <- dn.col
  if (hasArg(colorset)){
    cs$Env$theme$col <- match.call(expand.dots=TRUE)$colorset
  } else {
    cs$Env$theme$col <- col
  }
  cs$Env$theme$rylab <- yaxis.right
  cs$Env$theme$lylab <- yaxis.left
  cs$Env$theme$bg <- bg.col
  cs$Env$theme$grid <- grid.col
  cs$Env$theme$grid2 <- grid2
  cs$Env$theme$labels <- labels.col
  cs$Env$theme$srt <- srt
  cs$Env$theme$xaxis.las <- xaxis.las
  cs$Env$theme$cex.axis <- cex.axis
  cs$Env$format.labels <- format.labels
  cs$Env$grid.ticks.on <- grid.ticks.on
  cs$Env$grid.ticks.lwd <- grid.ticks.lwd
  cs$Env$grid.ticks.lty <- grid.ticks.lty
  cs$Env$type <- type
  cs$Env$lty <- lty
  cs$Env$lwd <- lwd
  cs$Env$lend <- lend
  cs$Env$legend.loc <- legend.loc
  cs$Env$call_list <- list()
  cs$Env$call_list[[1]] <- match.call()
  
  # Do some checks on x
  if(is.character(x))
    stop("'x' must be a time-series object")
  
  # If we detect an OHLC object, we should call quantmod::chart_Series
  
  # Raw returns data passed into function
  cs$Env$xdata <- x
  cs$Env$xsubset <- subset
  cs$Env$column_names <- colnames(x)
  cs$Env$nobs <- NROW(cs$Env$xdata)
  cs$Env$main <- main
  
  # non equally spaced x-axis
  xycoords <- xy.coords(.index(cs$Env$xdata[cs$Env$xsubset]), 
                        cs$Env$xdata[cs$Env$xsubset][,1])
  cs$Env$xycoords <- xycoords
  cs$Env$xlim <- range(xycoords$x, na.rm=TRUE)
  cs$Env$xstep <- diff(xycoords$x[1:2])
  
  # Compute transformation if specified by panel argument
  # rough prototype for calling a function for the main "panel"
  if(!is.null(FUN)){
    fun <- match.fun(FUN)
    .formals <- formals(fun)
    .formals <- modify.args(formals=.formals, arglist=list(...), dots=TRUE)
    if("R" %in% names(.formals)) .formals <- modify.args(formals=.formals, arglist=NULL, R=x, dots=TRUE)
    if("x" %in% names(.formals)) .formals <- modify.args(formals=.formals, arglist=NULL, x=x, dots=TRUE)
    .formals$... <- NULL
    R <- try(do.call(fun, .formals), silent=TRUE)
    if(inherits(R, "try-error")) { 
      message(paste("FUN function failed with message", R))
      cs$Env$R <- x
    } else {
      cs$Env$R <- R
    }
  } else {
    cs$Env$R <- x
  }
  
  # Set xlim based on the raw returns data passed into function
  # cs$set_xlim(c(1,NROW(cs$Env$xdata[subset])))
  # non equally spaced x-axis
  cs$set_xlim(cs$Env$xlim)
  
  
  # Set ylim based on the transformed data
  # chart_Series uses fixed=FALSE and add_* uses fixed=TRUE, not sure why or
  # which is best.
  if(is.null(ylim)){
    if(isTRUE(multi.panel)){
      if(yaxis.same){
        # set the ylim for the first panel based on all the data
        cs$set_ylim(list(structure(range(cs$Env$R[subset], na.rm=TRUE),fixed=TRUE)))
      } else {
        # set the ylim for the first panel based on the first column
        cs$set_ylim(list(structure(range(cs$Env$R[,1][subset], na.rm=TRUE),fixed=TRUE))) 
      }
    } else {
      # set the ylim based on all the data if this is not a multi.panel plot
      cs$set_ylim(list(structure(range(cs$Env$R[subset], na.rm=TRUE),fixed=TRUE)))
    }
    cs$Env$constant_ylim <- range(cs$Env$R[subset], na.rm=TRUE)
  } else {
    # use the ylim arg passed in
    cs$set_ylim(list(structure(ylim, fixed=TRUE)))
    cs$Env$constant_ylim <- ylim
  }
  
  cs$set_frame(1,FALSE)
  # axis_ticks function to label lower frequency ranges/grid lines
  #cs$Env$axis_ticks <- function(xdata,xsubset) {
  #  ticks <- diff(axTicksByTime2(xdata[xsubset],labels=FALSE))/2 + 
  #    last(axTicksByTime2(xdata[xsubset],labels=TRUE),-1)
  #  if(min(diff(ticks)) < max(strwidth(names(ticks)))) {
  #    ticks <- unname(ticks)
  #  }
  #  ticks
  #}
  
  # compute the x-axis ticks
  cs$add(expression(atbt <- axTicksByTime2(xdata[xsubset]),
                    segments(xycoords$x[atbt], #axTicksByTime2(xdata[xsubset]),
                             get_ylim()[[2]][1],
                             xycoords$x[atbt], #axTicksByTime2(xdata[xsubset]),
                             get_ylim()[[2]][2], 
                             col=theme$grid, lwd=grid.ticks.lwd, lty=grid.ticks.lty)),
         clip=FALSE,expr=TRUE)
  
  # Add frame for the chart "header" to display the name and start/end dates
  cs$add_frame(0,ylim=c(0,1),asp=0.5)
  cs$set_frame(1)
  
  # add observation level ticks on x-axis if < 400 obs.
  cs$add(expression(if(NROW(xdata[xsubset])<400) 
  {axis(1,at=xycoords$x,labels=FALSE,col=theme$grid2,tcl=0.3)}),expr=TRUE)
  
  # add "month" or "month.abb"
  cs$add(expression(axt <- axTicksByTime(xdata[xsubset],format.labels=format.labels),
                    axis(1,
                         at=xycoords$x[axt], #axTicksByTime(xdata[xsubset]),
                         labels=names(axt), #axTicksByTime(xdata[xsubset],format.labels=format.labels)),
                         las=theme$xaxis.las, lwd.ticks=1, mgp=c(3,1.5,0), 
                         tcl=-0.4, cex.axis=theme$cex.axis)),
         expr=TRUE)
  
  # add main and start/end dates
  #if((isTRUE(multi.panel)) | (multi.panel == 1) | (NCOL(x) == 1))
  #  cs$Env$main <- cs$Env$column_names[1] else cs$Env$main <- main
  
  text.exp <- c(expression(text(xlim[1],0.5,main,font=2,col='#444444',offset=0,cex=1.1,pos=4)),
                expression(text(xlim[2],0.5,
                                paste(start(xdata[xsubset]),end(xdata[xsubset]),sep=" / "),
                                col=1,adj=c(0,0),pos=2)))
  cs$add(text.exp, env=cs$Env, expr=TRUE)
  
  cs$set_frame(2)
  # define function for y-axis labels
  #cs$Env$grid_lines <- function(xdata, xsubset) {
  #  ylim <- range(xdata[xsubset])
  #  p <- pretty(ylim, 5)
  #  p[p > ylim[1] & p < ylim[2]]
  #}
  
  cs$Env$y_grid_lines <- function(ylim) { 
    #pretty(range(xdata[xsubset]))
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
             expression(text(xlim[1]-xstep*2/3-max(strwidth(y_grid_lines(get_ylim()[[2]]))), 
                             y_grid_lines(get_ylim()[[2]]),
                             noquote(format(y_grid_lines(get_ylim()[[2]]), justify="right")),
                             col=theme$labels, srt=theme$srt, offset=0, pos=4, 
                             cex=theme$cex.axis, xpd=TRUE)))
  }
  if(yaxis.right){
    exp <- c(exp, 
             # right y-axis labels
             expression(text(xlim[2]+xstep*2/3,
                             y_grid_lines(get_ylim()[[2]]),
                             noquote(format(y_grid_lines(get_ylim()[[2]]), justify="right")),
                             col=theme$labels, srt=theme$srt, offset=0, pos=4, 
                             cex=theme$cex.axis, xpd=TRUE)))
  }
  cs$add(exp, env=cs$Env, expr=TRUE)
  
  # add main series
  cs$set_frame(2)
  if(isTRUE(multi.panel)){
    # We need to plot the first "panel" here because the plot area is
    # set up based on the code above
    lenv <- new.env()
    lenv$xdata <- cs$Env$R[,1][subset]
    lenv$label <- colnames(cs$Env$R[,1])
    lenv$type <- cs$Env$type
    if(yaxis.same){
      lenv$ylim <- cs$Env$constant_ylim
    } else {
      lenv$ylim <- range(cs$Env$R[,1][subset], na.rm=TRUE)
    }
    exp <- expression(chart.lines(xdata, 
                                  type=type, 
                                  lty=lty,
                                  lwd=lwd,
                                  lend=lend,
                                  col=theme$col, 
                                  up.col=theme$up.col, 
                                  dn.col=theme$dn.col,
                                  legend.loc=legend.loc))
    # Add expression for the main plot
    cs$add(exp, env=c(lenv,cs$Env), expr=TRUE)
    text.exp <- expression(text(x=xycoords$x[2],
                                y=ylim[2]*0.9,
                                labels=label,
                                adj=c(0,0),cex=1,offset=0,pos=4))
    cs$add(text.exp,env=c(lenv, cs$Env),expr=TRUE)
    
    if(NCOL(cs$Env$xdata) > 1){
      for(i in 2:NCOL(cs$Env$xdata)){
        # create a local environment
        lenv <- new.env()
        lenv$xdata <- cs$Env$R[,i][subset]
        lenv$label <- cs$Env$column_names[i]
        if(yaxis.same){
          lenv$ylim <- cs$Env$constant_ylim
        } else {
          lenv$ylim <- range(cs$Env$R[,i][subset], na.rm=TRUE)
        }
        lenv$type <- cs$Env$type
        
        # Add a small frame
        cs$add_frame(ylim=c(0,1),asp=0.25)
        cs$next_frame()
        text.exp <- expression(text(x=xlim[1],
                                    y=0.5,
                                    labels="",
                                    adj=c(0,0),cex=0.9,offset=0,pos=4))
        cs$add(text.exp, env=c(lenv,cs$Env), expr=TRUE)
        
        # Add the frame for the sub-plots
        # Set the ylim based on the (potentially) transformed data in cs$Env$R
        cs$add_frame(ylim=lenv$ylim, asp=NCOL(cs$Env$xdata), fixed=TRUE)
        cs$next_frame()
        
        exp <- expression(chart.lines(xdata[xsubset], 
                                      type=type, 
                                      lty=lty,
                                      lwd=lwd,
                                      lend=lend,
                                      col=theme$col, 
                                      up.col=theme$up.col, 
                                      dn.col=theme$dn.col,
                                      legend.loc=legend.loc))
        
        # define function to plot the y-axis grid lines
        lenv$y_grid_lines <- function(ylim) { 
          #pretty(range(xdata[xsubset]))
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
                 expression(atbt <- axTicksByTime2(xdata[xsubset]),
                            segments(xycoords$x[atbt], #axTicksByTime2(xdata[xsubset]),
                                     ylim[1],
                                     xycoords$x[atbt], #axTicksByTime2(xdata[xsubset]),
                                     ylim[2], 
                                     col=theme$grid, lwd=grid.ticks.lwd, lty=grid.ticks.lty)))
        if(yaxis.left){
          exp <- c(exp, 
                   # y-axis labels/boxes
                   expression(text(xlim[1]-xstep*2/3-max(strwidth(y_grid_lines(ylim))), 
                                   y_grid_lines(ylim),
                                   noquote(format(y_grid_lines(ylim),justify="right")),
                                   col=theme$labels, srt=theme$srt, offset=0, 
                                   pos=4, cex=theme$cex.axis, xpd=TRUE)))
        }
        if(yaxis.right){
          exp <- c(exp, 
                   expression(text(xlim[2]+xstep*2/3, y_grid_lines(ylim),
                                   noquote(format(y_grid_lines(ylim),justify="right")),
                                   col=theme$labels, srt=theme$srt, offset=0,
                                   pos=4, cex=theme$cex.axis, xpd=TRUE)))
        }
        cs$add(exp,env=c(lenv, cs$Env),expr=TRUE,no.update=TRUE)
        text.exp <- expression(text(x=xycoords$x[2],
                                    y=ylim[2]*0.9,
                                    labels=label,
                                    adj=c(0,0),cex=1,offset=0,pos=4))
        cs$add(text.exp,env=c(lenv, cs$Env),expr=TRUE)
      }
    }
  } else {
    if(type == "h" & NCOL(x) > 1) 
      warning("only the univariate series will be plotted")
    cs$add(expression(chart.lines(R[xsubset], 
                                  type=type, 
                                  lty=lty,
                                  lwd=lwd,
                                  lend=lend,
                                  col=theme$col,
                                  up.col=theme$up.col, 
                                  dn.col=theme$dn.col,
                                  legend.loc=legend.loc)),expr=TRUE)
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

#' Add a time series to an existing xts plot
#' 
#' @param x an xts object to plot.
#' @param main main title for a new panel if drawn.
#' @param on panel number to draw on. A new panel will be drawn if \code{on=NA}.
#' @param type the type of plot to be drawn, same as in \code{\link{plot}}.
#' @param col color palette to use, set by default to rational choices.
#' @param lty set the line type, same as in \code{\link{plot}}.
#' @param lwd set the line width, same as in \code{\link{plot}}.
#' @param pch the type of plot to be drawn, same as in \code{\link{plot}}.
#' @param \dots any other passthrough parameters. Not currently used.
#' @author Ross Bennett
addSeries <- function(x, main="", on=NA, type="l", col=NULL, lty=1, lwd=1, pch=0, ...){
  lenv <- new.env()
  lenv$main <- main
  lenv$plot_lines <- function(x, ta, on, type, col, lty, lwd, pch, ...){
    xdata <- x$Env$xdata
    xsubset <- x$Env$xsubset
    if(is.null(col)) col <- x$Env$theme$col
    if(all(is.na(on))){
      # Add x-axis grid lines
      atbt <- axTicksByTime2(xdata[xsubset])
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
                           tzone=indexTZ(xdata)),ta)[subset.range]
    ta.x <- as.numeric(na.approx(ta.adj[,1], rule=2) )
    ta.y <- ta.adj[,-1]
    chart.lines(ta.y, type=type, col=col, lty=lty, lwd=lwd, pch=pch)
  }
  # map all passed args (if any) to 'lenv' environment
  mapply(function(name,value) { assign(name,value,envir=lenv) }, 
         names(list(x=x,on=on,type=type,col=col,lty=lty,lwd=lwd,pch=pch,...)),
         list(x=x,on=on,type=type,col=col,lty=lty,lwd=lwd,pch=pch,...))
  exp <- parse(text=gsub("list","plot_lines",
                         as.expression(substitute(list(x=current.xts_chob(),
                                                       ta=get("x"),
                                                       on=on,
                                                       type=type,
                                                       col=col,
                                                       lty=lty,
                                                       lwd=lwd,
                                                       pch=pch,
                                                       ...)))),
               srcfile=NULL)
  
  plot_object <- current.xts_chob()
  ncalls <- length(plot_object$Env$call_list)
  plot_object$Env$call_list[[ncalls+1]] <- match.call()
  
  xdata <- plot_object$Env$xdata
  xsubset <- plot_object$Env$xsubset
  no.update <- FALSE
  lenv$xdata <- merge(x,xdata,retside=c(TRUE,FALSE))
  ylim <- range(lenv$xdata[xsubset], na.rm=TRUE)
  lenv$ylim <- ylim
  
  if(is.na(on)){
    # add the frame for drawdowns info
    plot_object$add_frame(ylim=c(0,1),asp=0.25)
    plot_object$next_frame()
    text.exp <- expression(text(x=xlim[1], y=0.3, labels=main,
                                col=1,adj=c(0,0),cex=0.9,offset=0,pos=4))
    plot_object$add(text.exp, env=c(lenv,plot_object$Env), expr=TRUE)
    
    # add frame for the data
    plot_object$add_frame(ylim=ylim,asp=1,fixed=TRUE)
    plot_object$next_frame()
    
    # define function to plot the y-axis grid lines
    lenv$y_grid_lines <- function(ylim) { 
      #pretty(range(xdata[xsubset]))
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
    plot_object$add(exp,env=c(lenv, plot_object$Env),expr=TRUE,no.update=TRUE)
  } else {
    for(i in 1:length(on)) {
      plot_object$set_frame(2*on[i]) # this is defaulting to using headers, should it be optionable?
      plot_object$add(exp,env=c(lenv, plot_object$Env),expr=TRUE,no.update=no.update)
    }
  }
  plot_object
}

lines.xts <- function(x, ..., main="", on=NA, col=NULL, type="l", lty=1, lwd=1, pch=0){
  addSeries(x, ...=..., main=main, on=on, type=type, col=col, lty=lty, lwd=lwd, pch=pch)
}


# addPoints <- function(x, main="", on=NA, col=NULL, pch=0, ...){
#   addSeries(x, main=main, on=on, type="p", col=col, pch=pch, ...)
# }

#' Add time series of points to an existing xts plot
#' 
#' @param x an xts object to plot.
#' @param main main title for a new panel if drawn.
#' @param on panel number to draw on. A new panel will be drawn if \code{on=NA}.
#' @param col color palette to use, set by default to rational choices.
#' @param pch the type of plot to be drawn, same as in \code{\link{plot}}.
#' @param \dots any other passthrough parameters. Not currently used.
#' @author Ross Bennett
points.xts <- function(x, ..., main="", on=NA, col=NULL, pch=0){
  addSeries(x, ...=..., main=main, on=on, type="p", col=col, pch=pch)
}

#' Add vertical lines to an existing xts plot
#' 
#' @param event.lines character vector of dates. Vertical lines will be drawn 
#' to indicate that an event happened during that time period.  \code{event.lines} should
#' be a vector of dates (e.g., \code{c("09/03","05/06"))} formatted the same as
#' \code{date.format}. This function matches the re-formatted row names (dates) with
#' the events.list, so to get a match the formatting needs to be correct.
#' @param event.labels character vector of event labels corresponding to 
#' \code{event.lines}. This will apply text labels (e.g., 
#' \code{c("This Event", "That Event")} to the vertical lines drawn.
#' @param date.format format for the dates in \code{event.lines}.
#' @param main main title for a new panel if drawn.
#' @param on panel number to draw on. A new panel will be drawn if \code{on=NA}.
#' @param lty set the line type, same as in \code{\link{plot}}.
#' @param lwd set the line width, same as in \code{\link{plot}}.
#' @param col color palette to use, set by default to rational choices.
#' @param \dots any other passthrough parameters. Not currently used.
#' @author Ross Bennett
addEventLines <- function(event.dates, event.labels=NULL, date.format="%Y-%m-%d", main="", on=NA, lty=1, lwd=1, col=1, ...){
  # add checks for event.dates and event.labels
  if(!is.null(event.labels))
    if(length(event.dates) != length(event.labels)) stop("length of event.dates must match length of event.labels")
  
  lenv <- new.env()
  lenv$main <- main
  lenv$plot_event_lines <- function(x, event.dates, event.labels, date.format, on, lty, lwd, col, ...){
    xdata <- x$Env$xdata
    xsubset <- x$Env$xsubset
    col <- x$Env$theme$col
    if(all(is.na(on))){
      # Add x-axis grid lines
      atbt <- axTicksByTime2(xdata[xsubset])
      segments(x$Env$xycoords$x[atbt],
               par("usr")[3],
               x$Env$xycoords$x[atbt],
               par("usr")[4],
               col=x$Env$theme$grid)
    }
    ypos <- x$Env$ylim[[2*on]][2]
    # create a new xts object out of event.dates
    event.dates.xts <- xts(rep(999, length(event.dates)), 
                           order.by=as.POSIXct(event.dates, tz=indexTZ(xdata), format=date.format))
    # we can add points that are not necessarily at the points on the main series
    subset.range <- paste(start(xdata[xsubset]),
                          end(xdata[xsubset]),sep="/")
    ta.adj <- merge(n=.xts(1:NROW(xdata[xsubset]),
                           .index(xdata[xsubset]), 
                           tzone=indexTZ(xdata)),event.dates.xts)[subset.range]
    ta.x <- as.numeric(na.approx(ta.adj[,1], rule=2) )
    ta.y <- ta.adj[,-1]
    event.ind <- which(ta.y == 999)
    abline(v=x$Env$xycoords$x[event.ind], col=col, lty=lty, lwd=lwd)
    text(x=x$Env$xycoords$x[event.ind], y=ypos, labels=event.labels, offset=.2, pos=2, , srt=90, col=1)
  }
  
  plot_object <- current.xts_chob()
  ncalls <- length(plot_object$Env$call_list)
  plot_object$Env$call_list[[ncalls+1]] <- match.call()
  
  if(is.na(on[1])){
    # map all passed args (if any) to 'lenv' environment
    mapply(function(name,value) { assign(name,value,envir=lenv) }, 
           names(list(event.dates=event.dates,event.labels=event.labels,date.format=date.format,on=on,lty=lty,lwd=lwd,col=col,...)),
           list(event.dates=event.dates,event.labels=event.labels,date.format=date.format,on=on,lty=lty,lwd=lwd,col=col,...))
    exp <- parse(text=gsub("list","plot_event_lines",
                           as.expression(substitute(list(x=current.xts_chob(),
                                                         event.dates=event.dates,
                                                         event.labels=event.labels,
                                                         date.format=date.format,
                                                         on=on,
                                                         lty=lty,
                                                         lwd=lwd,
                                                         col=col,
                                                         ...)))),
                 srcfile=NULL)
    
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
    plot_object$add(text.exp, env=c(lenv,plot_object$Env), expr=TRUE)
    
    # add frame for the data
    plot_object$add_frame(ylim=ylim,asp=1,fixed=TRUE)
    plot_object$next_frame()
    
    # define function to plot the y-axis grid lines
    lenv$y_grid_lines <- function(ylim) { 
      #pretty(range(xdata[xsubset]))
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
    plot_object$add(exp,env=c(lenv, plot_object$Env),expr=TRUE,no.update=TRUE)
  } else {
    for(i in 1:length(on)) {
      ind <- on[i]
      no.update <- FALSE
      # map all passed args (if any) to 'lenv' environment
      mapply(function(name,value) { assign(name,value,envir=lenv) }, 
             names(list(event.dates=event.dates,event.labels=event.labels,date.format=date.format,on=ind,lty=lty,lwd=lwd,col=col,...)),
             list(event.dates=event.dates,event.labels=event.labels,date.format=date.format,on=ind,lty=lty,lwd=lwd,col=col,...))
      exp <- parse(text=gsub("list","plot_event_lines",
                             as.expression(substitute(list(x=current.xts_chob(),
                                                           event.dates=event.dates,
                                                           event.labels=event.labels,
                                                           date.format=date.format,
                                                           on=ind,
                                                           lty=lty,
                                                           lwd=lwd,
                                                           col=col,
                                                           ...)))),
                   srcfile=NULL)
      
      plot_object$set_frame(2*on[i]) # this is defaulting to using headers, should it be optionable?
      plot_object$add(exp,env=c(lenv, plot_object$Env),expr=TRUE,no.update=no.update)
    }
  }
  plot_object
}

#' Add Legend
#' 
#' @param legend.loc legend.loc places a legend into one of nine locations on 
#' the chart: bottomright, bottom, bottomleft, left, topleft, top, topright, 
#' right, or center.
#' @param legend.names character vector of names for the legend. If \code{NULL},
#' the column names of the current plot object are used.
#' @param col fill colors for the legend. If \code{NULL},
#' the colorset of the current plot object data is used.
#' @param ncol number of columns for the legend
#' @param \dots any other passthrough parameters to \code{\link{legend}}.
#' @author Ross Bennett
addLegend <- function(legend.loc="center", legend.names=NULL, col=NULL, ncol=1, on=1, ...){
  lenv <- new.env()
  lenv$plot_legend <- function(x, legend.loc, legend.names, col, ncol, on, ...){
    if(is.na(on)){
      yrange <- c(0, 1)
    } else {
      yrange <- x$Env$ylim[[2*on]]
    }
    chob.xlim <- x$Env$xlim
    switch(legend.loc,
           topleft = {
             xjust <- 0
             yjust <- 1
             lx <- chob.xlim[1]
             ly <- yrange[2]
           },
           left = {
             xjust <- 0
             yjust <- 0.5
             lx <- chob.xlim[1]
             ly <- sum(yrange) / 2
           },
           bottomleft = {
             xjust <- 0
             yjust <- 0
             lx <- chob.xlim[1]
             ly <- yrange[1]
           },
           top = {
             xjust <- 0.5
             yjust <- 1
             lx <- (chob.xlim[1] + chob.xlim[2]) / 2
             ly <- yrange[2]
           },
           center = {
             xjust <- 0.5
             yjust <- 0.5
             lx <- (chob.xlim[1] + chob.xlim[2]) / 2
             ly <- sum(yrange) / 2
           },
           bottom = {
             xjust <- 0.5
             yjust <- 0
             lx <- (chob.xlim[1] + chob.xlim[2]) / 2
             ly <- yrange[1]
           },
           topright = {
             xjust <- 1
             yjust <- 1
             lx <- chob.xlim[2]
             ly <- yrange[2]
           },
           right = {
             xjust <- 1
             yjust <- 0.5
             lx <- chob.xlim[2]
             ly <- sum(yrange) / 2
           },
           bottomright = {
             xjust <- 1
             yjust <- 0
             lx <- chob.xlim[2]
             ly <- yrange[1]
           }
    )
    # this just gets the data of the main plot
    # TODO: get the data of frame[on]
    if(is.null(ncol)){
      ncol <- NCOL(x$Env$xdata)
    }
    if(is.null(col)){
      col <- x$Env$theme$col[1:nc]
    }
    if(is.null(legend.names)){
      legend.names <- x$Env$column_names
    }
    legend(x=lx, y=ly, legend=legend.names, xjust=xjust, yjust=yjust, 
           ncol=ncol, col=col, bty="n", ...)
  }
  
  # store the call
  plot_object <- current.xts_chob()
  ncalls <- length(plot_object$Env$call_list)
  plot_object$Env$call_list[[ncalls+1]] <- match.call()
  
  # if on[1] is NA, then add a new frame for the legend
  if(is.na(on[1])){
    # map all passed args (if any) to 'lenv' environment
    mapply(function(name,value) { assign(name,value,envir=lenv) }, 
           names(list(legend.loc=legend.loc, legend.names=legend.names, col=col, ncol=ncol, on=on,...)),
           list(legend.loc=legend.loc, legend.names=legend.names, col=col, ncol=ncol, on=on,...))
    exp <- parse(text=gsub("list","plot_legend",
                           as.expression(substitute(list(x=current.xts_chob(),
                                                         legend.loc=legend.loc,
                                                         legend.names=legend.names,
                                                         col=col,
                                                         ncol=ncol,
                                                         on=on,
                                                         ...)))),
                 srcfile=NULL)
    
    # add frame for spacing
    plot_object$add_frame(ylim=c(0,1),asp=0.25)
    plot_object$next_frame()
    text.exp <- expression(text(x=xlim[1], y=0.3, labels=main,
                                col=1,adj=c(0,0),cex=0.9,offset=0,pos=4))
    plot_object$add(text.exp, env=c(lenv,plot_object$Env), expr=TRUE)
    
    # add frame for the legend panel
    plot_object$add_frame(ylim=c(0,1),asp=0.8,fixed=TRUE)
    plot_object$next_frame()
    
    # add plot_legend expression
    plot_object$add(exp,env=c(lenv, plot_object$Env),expr=TRUE,no.update=TRUE)
  } else {
    for(i in 1:length(on)) {
      ind <- on[i]
      no.update <- FALSE
      # map all passed args (if any) to 'lenv' environment
      mapply(function(name,value) { assign(name,value,envir=lenv) }, 
             names(list(legend.loc=legend.loc, legend.names=legend.names, col=col, ncol=ncol, on=on,...)),
             list(legend.loc=legend.loc, legend.names=legend.names, col=col, ncol=ncol, on=on,...))
      exp <- parse(text=gsub("list","plot_legend",
                             as.expression(substitute(list(x=current.xts_chob(),
                                                           legend.loc=legend.loc,
                                                           legend.names=legend.names,
                                                           col=col,
                                                           ncol=ncol,
                                                           on=on,
                                                           ...)))),
                   srcfile=NULL)
      plot_object$set_frame(2*on[i]) # this is defaulting to using headers, should it be optionable?
      plot_object$add(exp,env=c(lenv, plot_object$Env),expr=TRUE,no.update=no.update)
    }
  }
  plot_object
}


# R/replot.R in quantmod with only minor edits to change class name to
# replot_xts and use the .plotxtsEnv instead of the .plotEnv in quantmod

new.replot_xts <- function(frame=1,asp=1,xlim=c(1,10),ylim=list(structure(c(1,10),fixed=FALSE))) {
  # global variables
  Env <- new.env()
  Env$frame <- frame
  Env$asp   <- asp
  #Env$usr   <- par("usr")
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
    #ylim[[1]] <- range(OHLC(Env$xdata)[x]) # main data
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
             #fixed <- attr(x, "fixed")
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
  
  # prepare window to draw
  #set_window()
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
  return(replot_env)
}

str.replot_xts <- function(x, ...) {
  print(str(unclass(x)))
}

print.replot_xts <- function(x, ...) plot(x,...)
plot.replot_xts <- function(x, ...) {
  plot.new()
  assign(".xts_chob",x,.plotxtsEnv)
  cex <- par(cex=x$Env$cex)
  mar <- par(mar=x$Env$mar)
  if(.Device=="X11") # only reasonable way to fix X11/quartz issue
    par(cex=x$Env$cex * 1.5)
  oxpd <- par(xpd=FALSE)
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
  #for(frames in 1:length(x$get_ylim())) {
  #x$set_frame(frames)
  #abline(h=x$get_ylim()[[frames]][1], col=x$Env$theme$grid, lwd=1)
  #}
  x$set_frame(abs(last.frame),clip=FALSE)
  do.call("clip",as.list(usr))
  par(xpd=oxpd,cex=cex$cex,mar=mar$mar)#,usr=usr)
  invisible(x$Env$actions)
}

scale.ranges <- function(frame, asp, ranges)
{
  asp/asp[frame] * abs(diff(ranges[[frame]]))
}

`+.replot` <- function(e1, e2) {
  e2 <- match.call()$e2
  e2$plot_object <- (substitute(e1))
  eval(e2)
}

`+.replot` <- function(e1, e2) {
  assign(".xts_chob",e1,.plotxtsEnv)
  e2 <- eval(e2)
  e2
}


##### accessor functions

re_Chart <- function() current.xts_chob()
chart_asp <- function() current.xts_chob()$get_asp()
chart_ylim <- function() current.xts_chob()$get_ylim()
chart_xlim <- function() current.xts_chob()$get_xlim()

actions <- function(obj) obj$Env$actions
chart_actions <- function() actions(current.xts_chob())
