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


current.xts_chob <- function() invisible(get(".xts_chob",.plotxtsEnv))


# Current design
#
# The code doesn't currently have a distinction between "panels" and "frames".
# They're always called "frames" and there are implicitly 2 frames/panel.
# Env$ylim and Env$asp have data for each panel, and they're stored as:
# list(header1 = c(0, 1), data1 = c(-100, 100),
#      header2 = c(0, 1), data2 = c(-100, 100), ...)
#
# add_frame(n) adds a new "frame" after frame 'n'. It doesn't distinguish
# between header frames or series frames, so the added frame could be either.
#
# Each element in actions has a frame attribute. So functions like add_frame(),
# update_frames(), etc look at the frame attribute for every action in
# Env$actions. The list of actions is updated by add()/replot().
# NOTE: this seems backward: panels contain actions
#
# Environments created by new_environment() (e.g. the 'lenv') are children of
# Env, so expressions evaluated in 'lenv' will look in Env for anything not
# found in 'lenv'.
#
# What does 'clip' do?
#  * hypothesis: prevents rendering outside of a region
#
# -----------------------------------------------------------------------------
# Target design for refactor
#
# * A plot window contains multiple panels
# * There are 2 frames per panel
#   * The first frame is a small 'header' frame for titles
#   * The second frame is larger and where the data series is rendered,
#     including axis labels
#
# The first panel is where the main series is rendered. Panels added later are
# plotted below the first panel and are smaller.
#
#
#  plot window
#  |- data
#  |- subset
#  |- global settings
#  |
#  |- panels
#  |  |- panel #1
#  |  |  |- header
#  |  |  |  |- actions
#  |  |  |     |- title
#  |  |  |     |- time span
#  |  |  |
#  |  |  |- series
#  |  |     |- actions
#  |  |        |- axis major/minor tick marks
#  |  |        |- axis grid lines
#  |  |        |- axis labels
#  |  |        |- plotting
#  |  |
#  |  |- panel #2
#  |  |  |- ...
#  |  |  |  ...
#
#
#
#   ____________________________________________________________________________
#  /                                                                            \
# |   plot object / window                                                       |
# |                                                                              |
# |    ______________________________________________________________________    |
# |   /                                                                      \   |
# |  |   panel #1                                                             |  |
# |  |   __________________________________________________________________   |  |
# |  |  /                                                                  \  |  |
# |  | |  header frame                                                      | |  |
# |  |  \__________________________________________________________________/  |  |
# |  |   __________________________________________________________________   |  |
# |  |  /                                                                  \  |  |
# |  | |  series frame                                                      | |  |
# |  | |                                                                    | |  |
# |  | |                                                                    | |  |
# |  | |                                                                    | |  |
# |  | |                                                                    | |  |
# |  | |                                                                    | |  |
# |  | |                                                                    | |  |
# |  | |                                                                    | |  |
# |  | |                                                                    | |  |
# |  | |                                                                    | |  |
# |  | |                                                                    | |  |
# |  | |                                                                    | |  |
# |  | |                                                                    | |  |
# |  | |                                                                    | |  |
# |  | |                                                                    | |  |
# |  | |                                                                    | |  |
# |  |  \__________________________________________________________________/  |  |
# |   \______________________________________________________________________/   |
# |                                                                              |
# |    ______________________________________________________________________    |
# |   /                                                                      \   |
# |  |   panel #2                                                             |  |
# |  |   __________________________________________________________________   |  |
# |  |  /                                                                  \  |  |
# |  | |  header frame                                                      | |  |
# |  |  \__________________________________________________________________/  |  |
# |  |   __________________________________________________________________   |  |
# |  |  /                                                                  \  |  |
# |  | |  series frame                                                      | |  |
# |  | |                                                                    | |  |
# |  | |                                                                    | |  |
# |  | |                                                                    | |  |
# |  | |                                                                    | |  |
# |  |  \__________________________________________________________________/  |  |
# |   \______________________________________________________________________/   |
# |                                                                              |
#  \____________________________________________________________________________/
#
#
#
# TODO:
#  * use Env$x <- append(Env$x, foo) in cases like Env$x[[length(Env$x)+1]] <- foo
#  * "Do some checks on x" should be is.xts(x), or at least try.xts()
#  * "reset par" should use on.exit() when par() is set:
#      orig_par <- par(foo = "x", bar = "y", ...);  on.exit(par(orig_par))


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
           # x-coordinates for this column
           xcoords <- xx$get_xcoords(x[,1])
           lines(xcoords,x[,1],lwd=2,col=colors,lend=lend,lty=1,type="h",...)
         },
         p=, l=, b=, c=, o=, s=, S=, n={
           if(is.null(col))
             col <- xx$Env$theme$col

           if(length(lty) < NCOL(x)) lty <- rep(lty, length.out = NCOL(x))
           if(length(lwd) < NCOL(x)) lwd <- rep(lwd, length.out = NCOL(x))
           if(length(col) < NCOL(x)) col <- rep(col, length.out = NCOL(x))

           for(i in NCOL(x):1) {
             # x-coordinates for this column
             xcoords <- xx$get_xcoords(x[,i])
             lines(xcoords, x[,i], type=type, lend=lend, col=col[i],
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

isNullOrFalse <- function(x) {
  is.null(x) || identical(x, FALSE)
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
                     main.timespan=TRUE,
                     observation.based=FALSE,
                     ylim=NULL,
                     yaxis.same=TRUE,
                     yaxis.left=TRUE,
                     yaxis.right=TRUE,
                     yaxis.ticks=5,
                     major.ticks="auto",
                     minor.ticks=NULL,
                     grid.ticks.on="auto",
                     grid.ticks.lwd=1,
                     grid.ticks.lty=1,
                     grid.col="darkgray",
                     labels.col="#333333",
                     format.labels=TRUE,
                     grid2="#F5F5F5",
                     legend.loc=NULL,
                     extend.xaxis=FALSE){
  
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
                    yaxis.ticks=yaxis.ticks,
                    major.ticks=major.ticks,
                    minor.ticks=minor.ticks,
                    grid.ticks.on=grid.ticks.on,
                    grid.ticks.lwd=grid.ticks.lwd,
                    grid.ticks.lty=grid.ticks.lty,
                    grid.col=grid.col,
                    labels.col=labels.col,
                    format.labels=format.labels,
                    grid2=grid2,
                    legend.loc=legend.loc,
                    extend.xaxis=extend.xaxis)
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

  cs$Env$theme <-
    list(up.col   = up.col,
         dn.col   = dn.col,
         col      = col,
         rylab    = yaxis.right,
         lylab    = yaxis.left,
         bg       = bg,
         grid     = grid.col,
         grid2    = grid2,
         labels   = labels.col,

         # String rotation in degrees. See comment about 'crt'. Only supported by text()
         srt      = if (hasArg("srt")) eval.parent(plot.call$srt) else 0,

         # Rotation of axis labels:
         #   0: parallel to the axis (default),
         #   1: horizontal,
         #   2: perpendicular to the axis,
         #   3: vertical
         las      = if (hasArg("las")) eval.parent(plot.call$las) else 0,

         # magnification for axis annotation relative to current 'cex' value
         cex.axis = if (hasArg("cex.axis")) eval.parent(plot.call$cex.axis) else 0.9)
  # /theme

  # multiplier to magnify plotting text and symbols
  cs$Env$cex <- if (hasArg("cex")) eval.parent(plot.call$cex) else 0.6

  # lines of margin to the 4 sides of the plot: c(bottom, left, top, right)
  cs$Env$mar <- if (hasArg("mar")) eval.parent(plot.call$mar) else c(3,2,0,2)
  
  # check for colorset or col argument
  # if col has a length of 1, replicate to NCOL(x) so we can keep it simple
  # and color each line by its index in col
  if(hasArg("colorset")) col <- eval.parent(plot.call$colorset)
  if(length(col) < ncol(x)) col <- rep(col, length.out = ncol(x))
  cs$Env$format.labels <- format.labels
  cs$Env$yaxis.ticks <- yaxis.ticks
  cs$Env$major.ticks <- if (isTRUE(major.ticks)) "auto" else major.ticks
  cs$Env$minor.ticks <- if (isTRUE(minor.ticks)) "auto" else minor.ticks
  cs$Env$grid.ticks.on <- if (isTRUE(grid.ticks.on)) "auto" else grid.ticks.on
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
  cs$Env$extend.xaxis <- extend.xaxis
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
  cs$Env$main.timespan <- main.timespan
  cs$Env$ylab <- if (hasArg("ylab")) eval.parent(plot.call$ylab) else ""
  
  if(is.null(ylim)){
    if(isTRUE(multi.panel)){
      if(yaxis.same){
        # set the ylim for the first panel based on all the data
        yrange <- cs$create_ylim(cs$Env$xdata[subset,])
        # and recalculate ylim when drawing (fixed=FALSE)
        yfixed <- FALSE
      } else {
        # set the ylim for the first panel based on the first column
        yrange <- cs$create_ylim(cs$Env$xdata[subset, 1])
        # but do NOT recalculate ylim when drawing (fixed=TRUE)
        yfixed <- TRUE
      }
    } else {
      # set the ylim based on all the data if this is not a multi.panel plot
      yrange <- cs$create_ylim(cs$Env$xdata[subset,])
      # and recalculate ylim when drawing (fixed=FALSE)
      yfixed <- FALSE
    }

    cs$Env$constant_ylim <- range(cs$Env$xdata[subset], na.rm=TRUE)
  } else {
    # use the ylim arg passed in
    # but do NOT recalculate ylim when drawing (fixed=TRUE)
    yrange <- ylim
    yfixed <- TRUE
    cs$Env$constant_ylim <- ylim
  }

  if (isTRUE(multi.panel) && NCOL(x) > 1) {
    asp <- NCOL(x)
  } else {
    asp <- 3
  }
  
  cs$add_main_header(isTRUE(main.timespan))
  if (isTRUE(multi.panel)) {
    main_header <- "multi.panel"
  } else {
    main_header <- "none"
  }

  # create the chart's main panel
  main_panel <-
    cs$new_panel(ylim = yrange,
                 asp = asp,
                 envir = cs$Env,
                 is_ylim_fixed = yfixed,
                 use_get_ylim = TRUE,
                 draw_left_yaxis = yaxis.left,
                 draw_right_yaxis = yaxis.right,
                 header_type = main_header)

  # add x-axis grid, ticks, and labels to the panel
  xaxis_expr <-
    cs$xaxis_expr(use_major = !isNullOrFalse(major.ticks),
                  use_minor = !isNullOrFalse(minor.ticks))
  main_panel$add_action(xaxis_expr)

  # add the main panel to the chart
  cs$add_panel(main_panel)
  
  if(isTRUE(multi.panel)){

    n_cols <- NCOL(cs$Env$xdata)

    for(i in seq_len(n_cols)) {
      # create a local environment
      lenv <- cs$new_environment()
      lenv$xdata <- cs$Env$xdata[subset,i]
      lenv$label <- cs$Env$column_names[i]
      lenv$type <- cs$Env$type
      if(yaxis.same){
        lenv$ylim <- cs$Env$constant_ylim
        lenv$is_ylim_fixed <- FALSE
      } else {
        lenv$ylim <- cs$create_ylim(cs$Env$xdata[subset, i])
        lenv$is_ylim_fixed <- TRUE
      }

      # allow color and line attributes for each panel in a multi.panel plot
      lenv$lty <- cs$Env$lty[i]
      lenv$lwd <- cs$Env$lwd[i]
      lenv$col <- cs$Env$theme$col[i]

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

      if (i == 1) {
        # plot the main panel, which is already set up
        main_panel$add_action(exp, env = lenv, update_ylim = TRUE)
        cs$Env$label <- cs$Env$column_names[i]

      } else {
        # plot the remaining remaining panels
        this_panel <-
          cs$new_panel(lenv$ylim,
                       asp = n_cols,
                       use_get_ylim = TRUE,
                       draw_left_yaxis = yaxis.left,
                       draw_right_yaxis = yaxis.right,
                       is_ylim_fixed = lenv$is_ylim_fixed,
                       header_type = "multi.panel",
                       envir = lenv)

        # plot data
        this_panel$add_action(exp, env = lenv, update_ylim = TRUE)

        # add the panel to the chart
        cs$add_panel(this_panel)
      }
    }
  } else {
    if(type == "h" && NCOL(x) > 1) 
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
    main_panel$add_action(exp)

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

    # we can add points that are not necessarily at the points
    # on the main series, but need to ensure the new series only
    # has index values within the xdata subset
    if(xsubset == "") {
      subset.range <- xsubset
    } else {
      fmt <- "%Y-%m-%d %H:%M:%OS6"
      subset.range <- paste(format(start(xDataSubset), fmt),
                            format(end(xDataSubset), fmt), sep = "/")
    }

    xds <- .xts(, .index(xDataSubset), tzone=tzone(xdata))
    ta.y <- merge(ta, xds)[subset.range]
    if (!isTRUE(x$Env$extend.xaxis)) {
        xi <- .index(ta.y)
        xc <- .index(xds)

        xsubset <- which(xi >= xc[1] & xi <= xc[length(xc)])
        ta.y <- ta.y[xsubset]
    }

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
  lenv$xdata <- merge(x,xdata,retside=c(TRUE,FALSE))
  if(hasArg("ylim")) {
    ylim <- eval.parent(substitute(alist(...))$ylim)
  } else {
    ylim <- range(lenv$xdata[xsubset], na.rm=TRUE)
    if(all(ylim == 0)) ylim <- c(-1, 1)
  }
  lenv$ylim <- ylim
  
  if(is.na(on[1])){
    # add series to a new panel
    this_panel <- plot_object$new_panel(lenv$ylim, asp = 1, envir = lenv)

    # plot data
    this_panel$add_action(exp, env = lenv, update_ylim = FALSE)

    # add the panel to the chart
    plot_object$add_panel(this_panel)

  } else {
    for(i in on) {
      this_panel <- plot_object$get_panel(i)
      this_panel$add_action(exp, env = lenv)
    }
  }
  plot_object
}

# Add time series of lines to an existing xts plot
# author: Ross Bennett
lines.xts <- function(x, ..., main="", on=0, col=NULL, type="l", lty=1, lwd=1, pch=1){
  if(!is.na(on[1]))
    if(on[1] == 0) on[1] <- current.xts_chob()$get_last_action_panel()$id
  
  addSeries(x, ...=..., main=main, on=on, type=type, col=col, lty=lty, lwd=lwd, pch=pch)
}

# Add time series of points to an existing xts plot
# author: Ross Bennett
points.xts <- function(x, ..., main="", on=0, col=NULL, pch=1){
  if(!is.na(on[1]))
    if(on[1] == 0) on[1] <- current.xts_chob()$get_last_action_panel()$id
  
  addSeries(x, ...=..., main=main, on=on, type="p", col=col, pch=pch)
}

# Add vertical lines to an existing xts plot
# author: Ross Bennett
addEventLines <- function(events, main="", on=0, lty=1, lwd=1, col=1, ...){
  events <- try.xts(events)

  plot_object <- current.xts_chob()

  if(!is.na(on[1]))
    if(on[1] == 0) on[1] <- plot_object$get_last_action_panel()$id
  
  if(nrow(events) > 1){
    if(length(lty) == 1) lty <- rep(lty, nrow(events))
    if(length(lwd) == 1) lwd <- rep(lwd, nrow(events))
    if(length(col) == 1) col <- rep(col, nrow(events))
  }
  
  lenv <- plot_object$new_environment()
  lenv$main <- main
  lenv$plot_event_lines <- function(x, events, on, lty, lwd, col, ...){
    xdata <- x$Env$xdata
    xsubset <- x$Env$xsubset

    ypos <- x$get_panel(on)$ylim[2] * 0.995
    # we can add points that are not necessarily at the points on the main series
    subset.range <-
      paste(format(start(xdata[xsubset]), "%Y%m%d %H:%M:%OS6"),
            format(end(xdata[xsubset]), "%Y%m%d %H:%M:%OS6"),
            sep = "/")
    ta.adj <- merge(n=.xts(1:NROW(xdata[xsubset]),
                           .index(xdata[xsubset]), 
                           tzone=tzone(xdata)),
                    .xts(rep(1, NROW(events)),# use numeric for the merge
                         .index(events)))[subset.range]
    # should we not merge and only add events that are in index(xdata)?
    ta.y <- ta.adj[,-1]
    # the merge should result in NAs for any object that is not in events
    event.ind <- which(!is.na(ta.y))
    abline(v=x$get_xcoords()[event.ind], col=col, lty=lty, lwd=lwd)
    text(x=x$get_xcoords()[event.ind], y=ypos,
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
    lenv$xdata <- xdata
    ylim <- range(xdata[xsubset], na.rm=TRUE)
    lenv$ylim <- ylim

    # add series to a new panel
    this_panel <- plot_object$new_panel(lenv$ylim, asp = 1, envir = lenv)

    # plot data
    this_panel$add_action(exp, env = lenv, update_ylim = FALSE)

    # add the panel to the chart
    plot_object$add_panel(this_panel)

  } else {
    for(i in on) {
      this_panel <- plot_object$get_panel(i)
      this_panel$add_action(exp, env = lenv)
    }
  }
  plot_object
}

# Add legend to an existing xts plot
# author: Ross Bennett
addLegend <- function(legend.loc="topright", legend.names=NULL, col=NULL, ncol=1, on=0, ...){

  plot_object <- current.xts_chob()

  if(!is.na(on[1]))
    if(on[1] == 0) on[1] <- plot_object$get_last_action_panel()$id
  
  lenv <- plot_object$new_environment()
  lenv$plot_legend <- function(x, legend.loc, legend.names, col, ncol, on, bty, text.col, ...){
    if(is.na(on[1])){
      yrange <- c(0, 1)
    } else {
      yrange <- x$get_panel(on)$ylim
    }
    # this just gets the data of the main plot
    # TODO: get the data of panels[on]
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

    # add legend to a new panel
    this_panel <- plot_object$new_panel(c(0, 1), asp = 0.8, envir = lenv)

    # legend data
    this_panel$add_action(exp, env = lenv, update_ylim = FALSE)

    # add the panel to the chart
    plot_object$add_panel(this_panel)
  } else {
    for(i in on) {
      this_panel <- plot_object$get_panel(i)
      this_panel$add_action(exp, env = lenv)
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

    # we can add points that are not necessarily at the points
    # on the main series
    subset.range <- paste(start(xdata[xsubset]),
                          end(xdata[xsubset]),sep="/")
    ta.adj <- merge(n=.xts(1:NROW(xdata[xsubset]),
                           .index(xdata[xsubset]), 
                           tzone=tzone(xdata)),ta)[subset.range]
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
  lenv$xdata <- merge(x,xdata,retside=c(TRUE,FALSE))
  if(hasArg("ylim")) {
    ylim <- eval.parent(substitute(alist(...))$ylim)
  } else {
    ylim <- range(lenv$xdata[xsubset], na.rm=TRUE)
    if(all(ylim == 0)) ylim <- c(-1, 1)
  }
  lenv$ylim <- ylim
  
  if(is.na(on[1])){
    # add series to a new panel
    this_panel <- plot_object$new_panel(lenv$ylim, asp = 1, envir = lenv)

    # plot data
    this_panel$add_action(exp, env = lenv, update_ylim = FALSE)

    # add the panel to the chart
    plot_object$add_panel(this_panel)

  } else {
    for(i in on) {
      this_panel <- plot_object$get_panel(i)
      this_panel$add_action(exp, env = lenv)
    }
  }
  plot_object
}# polygon


# R/replot.R in quantmod with only minor edits to change class name to
# replot_xts and use the .plotxtsEnv instead of the .plotEnv in quantmod

new.replot_xts <- function(panel=1,asp=1,xlim=c(1,10),ylim=list(structure(c(1,10),fixed=FALSE))) {
  # global variables

  # 'Env' is mainly the environment for the plot window, but some elements are for panels/frames
  Env <- new.env()
  Env$active_panel_i <- panel
  Env$asp   <- 1
  Env$xlim  <- xlim  # vector: c(min, max) (same for every panel)
  Env$last_action_panel_id <- 1
  
  # setters
  set_panel <- function(n) { Env$active_panel_i <<- n }
  set_ylim  <- function(ylim) { Env$ylim <<- ylim }

  # getters
  get_panel <- function(n) { if (n == 0) get_last_action_panel() else Env$panels[[n]] }
  get_ylim  <- function() { update_panels(); get_active_panel()[["ylim_render"]] }
  get_active_panel <- function() { get_panel(Env$active_panel_i) }
  get_last_action_panel <- function() { get_panel(Env$last_action_panel_id) }
  
  create_ylim <-
  function(x, const_y_mult = 0.2)
  {
    # Create y-axis limits from 'x'. Jitter the max/min limits by
    # 'const_y_mult' if the max/min values are the same.
    lim <- range(x, na.rm = TRUE)

    if(isTRUE(all.equal(lim[1L], lim[2L]))) {
      # if max and min are the same
      if(lim[1L] == 0) {
        lim <- c(-1, 1)
      } else {
        lim <- lim[1L] * c(1 - const_y_mult, 1 + const_y_mult)
      }
    }

    return(lim)
  }

  # loop over panels and then actions
  render_panels <-
  function()
  {
      update_panels()

      # all panel header/series asp pairs
      all_asp <- lapply(Env$panels, function(p) p[["asp"]])
      all_asp <- do.call(c, all_asp)

      # panel header asp is always 5% of the total asp
      panel_header_asp <- 0.05 * sum(all_asp)

      # update panel header asp values
      header_loc <- seq(1, length(all_asp), by = 2)
      all_asp[header_loc] <- panel_header_asp

      # main header asp is always 7% of the grand total asp
      main_title_asp <- 0.07 * sum(all_asp)

      all_asp <- c(main_title_asp, all_asp)
      n_asp <- length(all_asp)

      ### main plot header
      plot.window(Env$xlim, c(0, 1))
      clip(par("usr")[1], par("usr")[2], 0, 1)
      eval(Env$main_header$expr, Env)
      ### /main plot header

      for (panel_n in seq_along(Env$panels)) {

          panel <- Env$panels[[panel_n]]
          # set the current active panel, so get_ylim() will use it
          Env$active_panel_i <- panel_n

          is_header <- TRUE  # header is always the first action
          #panel$render_header(Env$xlim, panel_ymax[panel_n])

          for (action in panel$actions) {

              if (is_header) {
                  is_header <- FALSE
                  asp <- panel_header_asp
                  asp_n <- 2 * panel_n
                  ylim <- c(0, 1)
              } else {
                  asp <- panel$asp["series"]
                  asp_n <- 2 * panel_n + 1
                  ylim <- panel$ylim_render
              }

              # scaled ylim
              ylim_scale <- all_asp / asp * abs(diff(ylim))

              ymin_adj <- sum(ylim_scale[-seq_len(asp_n)])
              ymax_adj <- sum(ylim_scale[-(asp_n:n_asp)])
              scaled_ylim <- c(ylim[1] - ymin_adj, ylim[2] + ymax_adj)

              plot.window(Env$xlim, scaled_ylim)

              if (attr(action, "clip")) {
                  clip(par("usr")[1], par("usr")[2], ylim[1], ylim[2])
              }

              action_env <- attr(action, "env")
              eval(action, action_env)
          }
      }
  }

  get_xcoords <- function(xts_object = NULL, at_posix = FALSE) {
    # unique index for all series (always POSIXct)
    xcoords <- Env$xycoords$x

    if (!is.null(xts_object)) {
      # get the x-coordinates for the observations in xts_object
      xcoords <- merge(.xts(seq_along(xcoords), xcoords), xts_object,
                       fill = na.locf,  # for duplicate index values
                       join = "right", retside = c(TRUE, FALSE))

      if (!isTRUE(Env$extend.xaxis)) {
        xc <- Env$xycoords$x
        xi <- .index(xcoords)

        xsubset <- which(xi >= xc[1] & xi <= xc[length(xc)])
        xcoords <- xcoords[xsubset]
      }

      if(Env$observation.based && !at_posix) {
        result <- drop(coredata(xcoords))
      } else {
        result <- .index(xcoords)
      }
    } else {
      if(Env$observation.based && !at_posix) {
        result <- seq_along(xcoords)
      } else {
        result <- xcoords
      }
    }

    return(result)
  }

  # main header
  Env$main_header <- list()
  add_main_header <-
  function(add_timespan = TRUE)
  {
    Env$main_header$expr <-
      expression({
        text(x = xlim[1],
             y = 0.98,
             labels = main,
             adj = NULL,
             pos = 4,
             offset = 0,
             cex = 1.1,
             col = theme$labels,
             font = 2)
      })

    if (add_timespan) {
      Env$main_header$expr <-
        c(Env$main_header$expr,
          expression({
            text(x = xlim[2],
                 y = 0.98,
                 labels = .makeISO8601(xdata[xsubset]),
                 adj = c(0, 0),
                 pos = 2,
                 offset = 0.5,
                 cex = 1,
                 col = theme$labels,
                 font = NULL)
          }))
    }
  }

  # panel functionality
  Env$panels <- list()
  new_panel <-
  function(ylim,
           asp,
           envir,
           ...,
           is_ylim_fixed = TRUE,
           use_get_ylim = FALSE,
           draw_left_yaxis = NULL,
           draw_right_yaxis = NULL,
           header_type = c("small", "multi.panel", "none"),
           title_timespan = FALSE)
  {
      panel <- new.env(TRUE, envir)
      panel$id <- NA
      panel$asp <- c(header = 0.25, series = asp)
      panel$ylim <- ylim
      panel$ylim_render <- ylim
      panel$is_ylim_fixed <- is_ylim_fixed

      panel$actions <- list()
      panel$add_action <-
      function(expr,
               env = Env,
               clip = TRUE,
               update_ylim = TRUE,
               ...)
      {
          if (!is.expression(expr)) {
              expr <- as.expression(expr)
          }

          action <- structure(expr, clip = clip, env = env,
                              update_ylim = update_ylim, ...)
          panel$actions <- append(panel$actions, list(action))

          Env$last_action_panel_id <<- panel$id
      }

      # NOTE: the header action must be the 1st action for a panel
      header_type <- match.arg(header_type)

      if (header_type == "none") {
          panel$main <- ""
          header_type <- "small"
      }

      if (header_type == "multi.panel") {
          header_expr <-
              expression({
                  text(x = get_xcoords()[2],
                       y = 0.9 * ylim[2] + 0.1 * ylim[1],
                       labels = label,
                       adj = c(0, 0),
                       pos = 4,
                       offset = 0,
                       cex = 0.9,
                       col = theme$labels,
                       font = NULL)
              })
      } else if (header_type == "small") {
          header_expr <-
              expression({
                  text(x = xlim[1],
                       y = 0.3,
                       labels = main,
                       adj = c(0, 0),
                       pos = 4,
                       offset = 0,
                       cex = 0.9,
                       col = 1,
                       font = NULL)
              })
      }

      panel$add_action(header_expr, env = panel)

      # y-axis
      if (is.null(draw_left_yaxis)) {
        draw_left_yaxis <- Env$theme$lylab
      }
      if (is.null(draw_right_yaxis)) {
        draw_right_yaxis <- Env$theme$rylab
      }
      if (use_get_ylim) {
          yaxis_action <- yaxis_expr(get_ylim(),
                                     draw_left_yaxis, draw_right_yaxis)
      } else {
          yaxis_action <- yaxis_expr(ylim,
                                     draw_left_yaxis, draw_right_yaxis)
      }
      panel$add_action(yaxis_action, env = panel, update_ylim = FALSE)

      # x-axis grid
      xaxis_action <- expression(x_grid_lines(xdata, grid.ticks.on, par("usr")[3:4]))
      panel$add_action(xaxis_action, env = panel, update_ylim = FALSE)

      return(panel)
  }

  add_panel <- function(panel)
  {
      if (is.na(panel$id)) {
          panel$id <- length(Env$panels) + 1
      } else {
          warning("ignoring second call to add_panel() for panel ",
                  panel$id, call. = FALSE)
      }

      # append the new panel
      Env$panels <- append(Env$panels, list(panel))
  }

  update_panels <- function(headers=TRUE) {

    for (panel_n in seq_along(Env$panels)) {
      panel <- Env$panels[[panel_n]]
      if (!panel$is_ylim_fixed) {
        # set ylim to +/-Inf when fixed = FALSE so update_panel() recalculates ylim
        panel$ylim_render <- c(Inf, -Inf)
        panel$is_ylim_fixed <- FALSE
      }
    }

    update_panel <- function(panel_n)
    {
      # recalculate the panel's ylim based on actions added to the panel

      panel <- get_panel(panel_n)
      is_fixed <- panel$is_ylim_fixed  # cache original value

      if (!is_fixed) {
        for (action in panel$actions) {

          action_update_ylim <- attr(action, "update_ylim")
          action_env <- attr(action, "env")
          action_data <- action_env$xdata

          if (!is.null(action_data) && action_update_ylim) {
            # some actions (e.g. addLegend) do not have 'xdata'
            dat.range <- create_ylim(action_data[Env$xsubset])

            # re-retrieve ylim; actions may be changed it
            new_ylim <-
              c(min(panel$ylim[1], dat.range, na.rm = TRUE),
                max(panel$ylim[2], dat.range, na.rm = TRUE))

            # set to new ylim values
            panel$ylim_render <- new_ylim
            panel$is_ylim_fixed <- is_fixed  # use original value
          }
        }
      }
    }
    # reset all ylim values, by looking for range(env[[1]]$xdata)
    # xdata should be either coming from Env or if lenv, lenv
    for (panel_n in seq_along(Env$panels)) {
        update_panel(panel_n)
    }

    update_xaxis <- function(panel, x_axis)
    {
      # Create x-axis values using index values from data from all panels
      for (action in panel$actions) {
        action_env <- attr(action, "env")
        action_data <- action_env$xdata

        if (!is.null(action_data)) {
          # some actions (e.g. addLegend) do not have 'xdata'

          action_xaxis <- .index(action_data[Env$xsubset])
          new_xaxis <- sort(unique(c(x_axis, action_xaxis)))

          if (isTRUE(Env$extend.xaxis)) {
            result <- new_xaxis
          } else {
            xaxis_rng <- range(x_axis, na.rm = TRUE)
            result <- new_xaxis[new_xaxis >= xaxis_rng[1L] &
                                new_xaxis <= xaxis_rng[2L]]
          }
        }
      }
      return(result)
    }

    x_axis <- .index(Env$xdata[Env$xsubset])
    for (panel in Env$panels) {
        x_axis <- update_xaxis(panel, x_axis)
    }

    # Create x/y coordinates using the combined x-axis index
    Env$xycoords <- xy.coords(x_axis, seq_along(x_axis))

    if (Env$observation.based) {
      Env$xlim <- c(1, length(get_xcoords()))
    } else {
      Env$xlim <- range(get_xcoords(), na.rm = TRUE)
    }
  }

  yaxis_expr <-
  function(ylim_expr,
           yaxis_left,
           yaxis_right)
  {
    # y-axis grid lines and labels
    exp <- substitute({
        segments(x0 = xlim[1], y0 = y_grid_lines(ylim_expr),
                 x1 = xlim[2], y1 = y_grid_lines(ylim_expr),
                 col = theme$grid,
                 lwd = grid.ticks.lwd,
                 lty = grid.ticks.lty)
    }, list(ylim_expr = substitute(ylim_expr)))

    if (yaxis_left) {
      # left y-axis labels
      exp <- c(exp,
          substitute({
            text(x = xlim[1],
                 y = y_grid_lines(ylim_expr),
                 labels = noquote(format(y_grid_lines(ylim_expr), justify = "right")),
                 col = theme$labels,
                 srt = theme$srt,
                 offset = 0.5,
                 pos = 2,
                 cex = theme$cex.axis,
                 xpd = TRUE)
          }, list(ylim_expr = substitute(ylim_expr)))
      )
    }

    if (yaxis_right) {
      # right y-axis labels
      exp <- c(exp,
          substitute({
            text(x = xlim[2],
                 y = y_grid_lines(ylim_expr),
                 labels = noquote(format(y_grid_lines(ylim_expr), justify = "right")),
                 col = theme$labels,
                 srt = theme$srt,
                 offset = 0.5,
                 pos = 4,
                 cex = theme$cex.axis,
                 xpd = TRUE)
          }, list(ylim_expr = substitute(ylim_expr)))
      )
    }

    # y labels
    exp <- c(exp, expression(title(ylab = ylab[1], mgp = c(1, 1, 0))))

    return(exp)
  }

  xaxis_expr <-
  function(use_major, use_minor)
  {
      # add observation level ticks on x-axis if < 400 obs.
      expr <- expression({
          if (NROW(xdata[xsubset]) < 400) {
              axis(1,
                   at = get_xcoords(),
                   labels = FALSE,
                   las = theme$las,
                   lwd.ticks = NULL,
                   mgp = NULL,
                   tcl = 0.3,
                   cex.axis = theme$cex.axis,
                   col = theme$labels,
                   col.axis = theme$grid2)
          }
      })

      # and major and/or minor x-axis ticks and labels
      values <- list()
      types <- c("major", "minor")[c(use_major, use_minor)]
      for (type in types) {
          if (type == "major") {
              values$.ticks.on <- quote(major.ticks)
              values$.labels <- quote(names(axt))
              values$.lwd.ticks <- 1.5
          } else {
              values$.ticks.on <- quote(minor.ticks)
              values$.labels <- FALSE
              values$.lwd.ticks <- 0.75
          }
          expr <- c(expr, substitute({
              xcoords <- get_xcoords()
              x_index <- get_xcoords(at_posix = TRUE)
              axt <- axTicksByTime(.xts(,x_index)[xsubset],
                                   ticks.on = .ticks.on,
                                   format.labels = format.labels)
              axis(1,
                   at = xcoords[axt],
                   labels = .labels,
                   las = theme$las,
                   lwd.ticks = .lwd.ticks,
                   mgp = c(3,1.5,0),
                   tcl = -0.4,
                   cex.axis = theme$cex.axis,
                   col = theme$labels,
                   col.axis = theme$labels)

          }, values))
      }

      return(expr)
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
  replot_env$add_main_header <- add_main_header
  replot_env$new_panel <- new_panel
  replot_env$add_panel <- add_panel
  replot_env$xaxis_expr <- xaxis_expr
  replot_env$yaxis_expr <- yaxis_expr
  replot_env$get_xcoords <- get_xcoords
  replot_env$update_panels <- update_panels
  replot_env$render_panels <- render_panels
  replot_env$set_panel <- set_panel
  replot_env$get_panel <- get_panel
  replot_env$set_ylim <- set_ylim
  replot_env$get_ylim <- get_ylim
  replot_env$create_ylim <- create_ylim
  replot_env$add_call <- add_call
  replot_env$get_last_action_panel <- get_last_action_panel

  replot_env$new_environment <- function() { new.env(TRUE, Env) }

  # function to plot the y-axis grid lines
  replot_env$Env$y_grid_lines <- function(ylim) {
    p <- pretty(ylim, Env$yaxis.ticks)
    p <- p[p >= ylim[1] & p <= ylim[2]]
    return(p)
  }

  # function to plot the x-axis grid lines
  replot_env$Env$x_grid_lines <- function(x, ticks.on, ylim)
  {
    if (isNullOrFalse(ticks.on)) {
      invisible()
    } else {
      if (isTRUE(ticks.on)) ticks.on <- "auto"

      xcoords <- get_xcoords()
      x_index <- get_xcoords(at_posix = TRUE)
      atbt <- axTicksByTime(.xts(, x_index), ticks.on = ticks.on)
      segments(xcoords[atbt], ylim[1L],
               xcoords[atbt], ylim[2L],
               col = Env$theme$grid,
               lwd = Env$grid.ticks.lwd,
               lty = Env$grid.ticks.lty)
    }
  }

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
  on.exit(par(xpd = oxpd$xpd, cex = ocex$cex, mar = omar$mar, bg = obg$bg))

  x$render_panels()

  do.call("clip", as.list(usr))  # reset clipping region
  # reset par
  invisible(x$Env$actions)
}
