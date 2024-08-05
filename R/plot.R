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
# There is a main plot object that contains the plot title (and optional
# timespan), the x-axis labels and tick marks, and a list of 'panel' objects.
# The main plot object contains the objects/functions below.
#
#   * Env: an environment holds all the plot information.
#   * add_main_header(): add the main plot header
#   * add_main_xaxis(): add the x-axis labels and ticks to the main plot.
#   * new_panel(): create a new panel and add it to the plot.
#   * get_xcoords(): get the x-coordinate values for the plot.
#   * get_panel(): get a specific panel.
#   * get_last_action_panel(): get the panel that had the last rendered action.
#   * new_environment: create a new environment with 'Env' as its parent.

# Functions that aren't intended to be called externally:
#
#   * update_panels(): re-calculate the x-axis and y-axis values.
#   * render_panels(): render all the plot panels.
#   * x_grid_lines(): plot the x-axis grid lines.
#   * create_ylim(): create y-axis max/min, handling when max(x) == min(x).

# The panel object is composed of the following fields:
#
#   * id: the numeric index of the panel in the plot's list of panels.
#   * asp: the x/y aspect ratio for the panel (relative vertical size).
#   * ylim: the ylim of the panel when it was created.
#   * ylim_render: the ylim of the panel to use when rendering.
#   * use_fixed_ylim: do not update the panel ylim based on all panels data
#   * header: the panel title.
#   * actions: a list of expressions used to render the panel.
#   * add_action(): a function to add an action to the list.
#
# The panel has the 'yaxis_expr' expression for rendering the y-axis min/max
# values, labels, and grid lines/ticks. It also contains the x-axis grid
# expression because we need the y-axis min/max values to know where to draw
# the x-axis grid lines on the panel.

# Other notes
#
# Environments created by new_environment() (e.g. the 'lenv') are children of
# Env, so expressions evaluated in 'lenv' will look in Env for anything not
# found in 'lenv'.
#

# Visual representation of plot structure
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
                        log=FALSE,
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

           # ensure pars have ncol(x) elements
           lty <- rep(lty, length.out = NCOL(x))
           lwd <- rep(lwd, length.out = NCOL(x))
           col <- rep(col, length.out = NCOL(x))

           for(i in NCOL(x):1) {
             # x-coordinates for this column
             xcoords <- xx$get_xcoords(x[,i])
             xi <- x[,i]
             if (isTRUE(log)) xi <- log(xi)
             lines(xcoords, xi, type=type, lend=lend, col=col[i],
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

#' Plotting xts Objects
#' 
#' Plotting for xts objects.
#' 
#' Possible values for arguments `major.ticks`, `minor.ticks`, and
#' `grid.ticks.on` include \sQuote{auto}, \sQuote{minute}, \sQuote{hours},
#' \sQuote{days}, \sQuote{weeks}, \sQuote{months}, \sQuote{quarters}, and
#' \sQuote{years}. The default is \sQuote{auto}, which attempts to determine
#' sensible locations from the periodicity and locations of observations. The
#' other values are based on the possible values for the `ticks.on`
#' argument of [`axTicksByTime()`].
#' 
#' @param x A xts object.
#' @param y Not used, always `NULL`.
#' @param \dots Any passthrough arguments for `lines()` and `points()`.
#' @param subset An ISO8601-style subset string.
#' @param panels Character vector of expressions to plot as panels.
#' @param multi.panel Either `TRUE`, `FALSE`, or an integer less than or equal
#'   to the number of columns in the data set. When `TRUE`, each column of the
#'   data is plotted in a separate panel. When an integer 'n', the data will be
#'   plotted in groups of 'n' columns per panel and each group will be plotted
#'   in a separate panel.
#' @param col Color palette to use.
#' @param up.col Color for positive bars when `type = "h"`.
#' @param dn.col Color for negative bars when `type = "h"`.
#' @param bg Background color of plotting area, same as in [`par()`].
#' @param type The type of plot to be drawn, same as in [`plot()`].
#' @param lty Set the line type, same as in [`par()`].
#' @param lwd Set the line width, same as in [`par()`].
#' @param lend Set the line end style, same as in [`par()`].
#' @param main Main plot title.
#' @param main.timespan Should the timespan of the series be shown in the top
#'   right corner of the plot?
#' @param observation.based When `TRUE`, all the observations are equally spaced
#'   along the x-axis. When `FALSE` (the default) the observations on the x-axis
#'   are spaced based on the time index of the data.
#' @param log Should the y-axis be in log scale? Default `FALSE`.
#' @param ylim The range of the y axis.
#' @param yaxis.same Should 'ylim' be the same for every panel? Default `TRUE`.
#' @param yaxis.left Add y-axis labels to the left side of the plot?
#' @param yaxis.right Add y-axis labels to the right side of the plot?
#' @param yaxis.ticks Desired number of y-axis grid lines. The actual number of
#'   grid lines is determined by the `n` argument to [`pretty()`].
#' @param major.ticks Period specifying locations for major tick marks and labels
#'   on the x-axis. See Details for possible values.
#' @param minor.ticks Period specifying locations for minor tick marks on the
#'   x-axis. When `NULL`, minor ticks are not drawn. See details for possible
#'   values.
#' @param grid.ticks.on Period specifying locations for vertical grid lines.
#'   See details for possible values.
#' @param grid.ticks.lwd Line width of the grid.
#' @param grid.ticks.lty Line type of the grid.
#' @param grid.col Color of the grid.
#' @param labels.col Color of the axis labels.
#' @param format.labels Label format to draw lower frequency x-axis ticks and
#'   labels passed to [`axTicksByTime()`]
#' @param grid2 Color for secondary x-axis grid.
#' @param legend.loc Places a legend into one of nine locations on the chart:
#'   bottomright, bottom, bottomleft, left, topleft, top, topright, right, or
#'   center. Default `NULL` does not draw a legend.
#' @param on Panel number to draw on. A new panel will be drawn if `on = NA`.
#'   The default, `on = 0`, will add to the active panel. The active panel is
#'   defined as the panel on which the most recent action was performed. Note
#'   that only the first element of `on` is checked for the default behavior to
#'   add to the last active panel.
#' @param extend.xaxis When `TRUE`, extend the x-axis before and/or after the
#'   plot's existing time index range, so all of of the time index values of
#'   the new series are included in the plot. Default `FALSE`.
#' 
#' @author Ross Bennett
#' 
#' @seealso [`addSeries()`], [`addPanel()`]
#' 
#' @references based on [`chart_Series()`][quantmod::quantmod] in \pkg{quantmod}
#' written by Jeffrey A. Ryan
#' 
#' @examples
#' 
#' \dontrun{
#' data(sample_matrix)
#' sample.xts <- as.xts(sample_matrix)
#' 
#' # plot the Close
#' plot(sample.xts[,"Close"])
#' 
#' # plot a subset of the data
#' plot(sample.xts[,"Close"], subset = "2007-04-01/2007-06-31")
#' 
#' # function to compute simple returns
#' simple.ret <- function(x, col.name){
#'   x[,col.name] / lag(x[,col.name]) - 1
#' }
#' 
#' # plot the close and add a panel with the simple returns
#' plot(sample.xts[,"Close"])
#' R <- simple.ret(sample.xts, "Close")
#' lines(R, type = "h", on = NA)
#' 
#' # add the 50 period simple moving average to panel 1 of the plot
#' library(TTR)
#' lines(SMA(sample.xts[,"Close"], n = 50), on = 1, col = "blue")
#' 
#' # add month end points to the chart
#' points(sample.xts[endpoints(sample.xts[,"Close"], on = "months"), "Close"], 
#'        col = "red", pch = 17, on = 1)
#' 
#' # add legend to panel 1
#' addLegend("topright", on = 1, 
#'           legend.names = c("Close", "SMA(50)"), 
#'           lty = c(1, 1), lwd = c(2, 1),
#'           col = c("black", "blue", "red"))
#' }
#' 
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
                     log=FALSE,
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

  # check for colorset or col argument
  if(hasArg("colorset")) {
    col <- eval.parent(plot.call$colorset)
  }
  # ensure pars have ncol(x) elements
  col <- rep(col, length.out = NCOL(x))
  lty <- rep(lty, length.out = NCOL(x))
  lwd <- rep(lwd, length.out = NCOL(x))

  # Small multiples with multiple pages behavior occurs when multi.panel is
  # an integer. (i.e. multi.panel=2 means to iterate over the data in a step
  # size of 2 and plot 2 panels on each page
  # Make recursive calls and return
  if(is.numeric(multi.panel)){
    multi.panel <- min(NCOL(x), multi.panel)
    idx <- seq.int(1L, NCOL(x), 1L)
    chunks <- split(idx, ceiling(seq_along(idx)/multi.panel))
    
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
                    log=log,
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
  
  cs$Env$format.labels <- format.labels
  cs$Env$yaxis.ticks <- yaxis.ticks
  cs$Env$major.ticks <- if (isTRUE(major.ticks)) "auto" else major.ticks
  cs$Env$minor.ticks <- if (isTRUE(minor.ticks)) "auto" else minor.ticks
  cs$Env$grid.ticks.on <- if (isTRUE(grid.ticks.on)) "auto" else grid.ticks.on
  cs$Env$grid.ticks.lwd <- grid.ticks.lwd
  cs$Env$grid.ticks.lty <- grid.ticks.lty
  cs$Env$type <- type
  cs$Env$lty <- lty
  cs$Env$lwd <- lwd
  
  cs$Env$lend <- lend
  cs$Env$legend.loc <- legend.loc
  cs$Env$extend.xaxis <- extend.xaxis
  cs$Env$observation.based <- observation.based
  cs$Env$log <- isTRUE(log)
  
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
  
  xdata_ylim <- cs$create_ylim(cs$Env$xdata[subset,])

  if(isTRUE(multi.panel)){

    n_cols <- NCOL(cs$Env$xdata)
    asp <- ifelse(n_cols > 1, n_cols, 3)

    if (hasArg("yaxis.same") && hasArg("ylim") && !is.null(ylim)) {
      warning("only 'ylim' or 'yaxis.same' should be provided; using 'ylim'")
    }

    for(i in seq_len(n_cols)) {
      # create a local environment for each panel
      lenv <- cs$new_environment()
      lenv$xdata <- cs$Env$xdata[subset,i]
      lenv$type <- cs$Env$type

      if (is.null(ylim)) {
        if (yaxis.same) {
          lenv$ylim <- xdata_ylim       # set panel ylim using all columns
          lenv$use_fixed_ylim <- FALSE  # update panel ylim when rendering
        } else {
          panel_ylim <- cs$create_ylim(lenv$xdata)
          lenv$ylim <- panel_ylim       # set panel ylim using this column
          lenv$use_fixed_ylim <- TRUE   # do NOT update panel ylim when rendering
        }
      } else {
        lenv$ylim <- ylim            # use the ylim argument value
        lenv$use_fixed_ylim <- TRUE  # do NOT update panel ylim when rendering
      }

      # allow color and line attributes for each panel in a multi.panel plot
      lenv$lty <- cs$Env$lty[i]
      lenv$lwd <- cs$Env$lwd[i]
      lenv$col <- cs$Env$theme$col[i]
      lenv$log <- isTRUE(log)

      exp <- quote(chart.lines(xdata[xsubset],
                               type=type,
                               lty=lty,
                               lwd=lwd,
                               lend=lend,
                               col=col,
                               log=log,
                               up.col=theme$up.col,
                               dn.col=theme$dn.col,
                               legend.loc=legend.loc))
      exp <- as.expression(add.par.from.dots(exp, ...))

      # create the panels
      this_panel <-
        cs$new_panel(lenv$ylim,
                     asp = asp,
                     envir = lenv,
                     header = cs$Env$column_names[i],
                     draw_left_yaxis = yaxis.left,
                     draw_right_yaxis = yaxis.right,
                     use_fixed_ylim = lenv$use_fixed_ylim,
                     use_log_yaxis = log)

      # plot data
      this_panel$add_action(exp, env = lenv)
    }
  } else {
    if(type == "h" && NCOL(x) > 1) 
      warning("only the univariate series will be plotted")

    if (is.null(ylim)) {
      yrange <- xdata_ylim      # set ylim using all columns
      use_fixed_ylim <- FALSE   # update panel ylim when rendering
    } else {
      yrange <- ylim            # use the ylim argument value
      use_fixed_ylim <- TRUE    # do NOT update panel ylim when rendering
    }

    # create the chart's main panel
    main_panel <-
      cs$new_panel(ylim = yrange,
                   asp = 3,
                   envir = cs$Env,
                   header = "",
                   use_fixed_ylim = use_fixed_ylim,
                   draw_left_yaxis = yaxis.left,
                   draw_right_yaxis = yaxis.right,
                   use_log_yaxis = log)

    exp <- quote(chart.lines(xdata[xsubset],
                             type=type, 
                             lty=lty,
                             lwd=lwd,
                             lend=lend,
                             col=theme$col,
                             log=log,
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


#' Add a panel to an existing xts plot
#' 
#' Apply a function to the data of an existing xts plot object and plot the
#' result on an existing or new panel. `FUN` should have arguments `x` or `R`
#' for the data of the existing xts plot object to be passed to. All other
#' additional arguments for `FUN` are passed through \dots.
#' 
#' @param FUN An xts object to plot.
#' @param main Main title for a new panel if drawn.
#' @param on Panel number to draw on. A new panel will be drawn if `on = NA`.
#' @param type The type of plot to be drawn, same as in [`plot()`].
#' @param col Color palette to use, set by default to rational choices.
#' @param lty Set the line type, same as in [`par()`].
#' @param lwd Set the line width, same as in [`par()`].
#' @param pch The type of plot to be drawn, same as in [`par()`].
#' @param \dots Additional named arguments passed through to `FUN` and any
#'   other graphical passthrough parameters.
#' 
#' @seealso [`plot.xts()`], [`addSeries()`]
#' 
#' @author Ross Bennett
#' 
#' @examples
#' 
#' library(xts)
#' data(sample_matrix)
#' sample.xts <- as.xts(sample_matrix)
#' 
#' calcReturns <- function(price, method = c("discrete", "log")){
#'   px <- try.xts(price)
#'   method <- match.arg(method)[1L]
#'   returns <- switch(method,
#'     simple = ,
#'     discrete = px / lag(px) - 1,
#'     compound = ,
#'     log = diff(log(px)))
#'   reclass(returns, px)
#' }
#' 
#' # plot the Close
#' plot(sample.xts[,"Close"])
#' # calculate returns 
#' addPanel(calcReturns, method = "discrete", type = "h")
#' # Add simple moving average to panel 1
#' addPanel(rollmean, k = 20, on = 1)
#' addPanel(rollmean, k = 40, col = "blue", on = 1)
#' 
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


#' Add a time series to an existing xts plot
#' 
#' Add a time series to an existing xts plot
#' 
#' @param x An xts object to add to the plot.
#' @param main Main title for a new panel if drawn.
#' @param on Panel number to draw on. A new panel will be drawn if `on = NA`.
#' @param type The type of plot to be drawn, same as in [`plot()`].
#' @param col Color palette to use, set by default to rational choices.
#' @param lty Set the line type, same as in [`par()`].
#' @param lwd Set the line width, same as in [`par()`].
#' @param pch The type of plot to be drawn, same as in [`par()`].
#' @param \dots Any other passthrough graphical parameters.
#' 
#' @author Ross Bennett
#' 
addSeries <- function(x, main="", on=NA, type="l", col=NULL, lty=1, lwd=1, pch=1, ...){
  plot_object <- current.xts_chob()
  lenv <- plot_object$new_environment()
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
    use_log <- isTRUE(eval.parent(substitute(alist(...))$log))
    this_panel <- plot_object$new_panel(lenv$ylim,
                                        asp = 1,
                                        envir = lenv,
                                        header = main,
                                        use_log_yaxis = use_log)

    # plot data
    this_panel$add_action(exp, env = lenv)

  } else {
    for(i in on) {
      plot_object$add_panel_action(i, exp, lenv)
    }
  }
  plot_object
}

# Add time series of lines to an existing xts plot
# author: Ross Bennett

#' @param pch the plotting character to use, same as in 'par'
#' @rdname plot.xts
lines.xts <- function(x, ..., main="", on=0, col=NULL, type="l", lty=1, lwd=1, pch=1){
  if(!is.na(on[1]))
    if(on[1] == 0) on[1] <- current.xts_chob()$get_last_action_panel()$id
  
  addSeries(x, ...=..., main=main, on=on, type=type, col=col, lty=lty, lwd=lwd, pch=pch)
}

# Add time series of points to an existing xts plot
# author: Ross Bennett

#' @param pch the plotting character to use, same as in 'par'
#' @rdname plot.xts
points.xts <- function(x, ..., main="", on=0, col=NULL, pch=1){
  if(!is.na(on[1]))
    if(on[1] == 0) on[1] <- current.xts_chob()$get_last_action_panel()$id
  
  addSeries(x, ...=..., main=main, on=on, type="p", col=col, pch=pch)
}

# Add vertical lines to an existing xts plot
# author: Ross Bennett


#' Add vertical lines to an existing xts plot
#' 
#' Add vertical lines and labels to an existing xts plot.
#' 
#' @param events An xts object of events and their associated labels. It is
#'   ensured that the first column of `events` is the event description/label.
#' @param main Main title for a new panel, if drawn.
#' @param on Panel number to draw on. A new panel will be drawn if `on = NA`.
#'   The default, `on = 0`, will add to the active panel. The active panel is
#'   defined as the panel on which the most recent action was performed. Note
#'   that only the first element of `on` is checked for the default behavior to
#'   add to the last active panel.
#' @param lty Set the line type, same as in [`par()`].
#' @param lwd Set the line width, same as in [`par()`].
#' @param col Color palette to use, set by default to rational choices.
#' @param \dots Any other passthrough parameters to [`text()`] to control how
#'   the event labels are drawn.
#' 
#' @author Ross Bennett
#' 
#' @examples
#' 
#' \dontrun{
#' library(xts)
#' data(sample_matrix)
#' sample.xts <- as.xts(sample_matrix)
#' events <- xts(letters[1:3], 
#'               as.Date(c("2007-01-12", "2007-04-22", "2007-06-13")))
#' plot(sample.xts[,4])
#' addEventLines(events, srt = 90, pos = 2)
#' }
#' 
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
  lenv$plot_event_lines <- function(x, events, on, lty, lwd, col, ...){
    xdata <- x$Env$xdata
    xsubset <- x$Env$xsubset

    panel <- x$get_active_panel()
    if (panel$use_log_yaxis) {
      ypos <- log(exp(panel$ylim_render[2]) * 0.995)
    } else {
      ypos <- panel$ylim_render[2] * 0.995
    }

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

  if(is.na(on[1])){
    xdata <- plot_object$Env$xdata
    xsubset <- plot_object$Env$xsubset
    lenv$xdata <- xdata
    ylim <- range(xdata[xsubset], na.rm=TRUE)
    lenv$ylim <- ylim

    # add series to a new panel
    this_panel <- plot_object$new_panel(lenv$ylim,
                                        asp = 1,
                                        envir = lenv,
                                        header = main)

    # plot data
    this_panel$add_action(exp, env = lenv)

  } else {
    for(i in on) {
      plot_object$add_panel_action(i, exp, lenv)
    }
  }
  plot_object
}

# Add legend to an existing xts plot
# author: Ross Bennett


#' Add Legend
#' 
#' Add a legend to an existing panel.
#' 
#' @param legend.loc One of nine locations: bottomright, bottom, bottomleft,
#'   left, topleft, top, topright, right, or center.
#' @param legend.names Character vector of names for the legend. When `NULL`,
#'   the column names of the current plot object are used.
#' @param col Fill colors for the legend. When `NULL`, the colorset of the
#'   current plot object data is used.
#' @param ncol Number of columns for the legend.
#' @param on Panel number to draw on. A new panel will be drawn if `on = NA`.
#'   The default, `on = 0`, will add to the active panel. The active panel is
#'   defined as the panel on which the most recent action was performed. Note
#'   that only the first element of `on` is checked for the default behavior to
#'   add to the last active panel.
#' @param \dots Any other passthrough parameters to [`legend()`].
#' 
#' @author Ross Bennett
#' 
addLegend <- function(legend.loc="topright", legend.names=NULL, col=NULL, ncol=1, on=0, ...){

  plot_object <- current.xts_chob()

  if(!is.na(on[1]))
    if(on[1] == 0) on[1] <- plot_object$get_last_action_panel()$id
  
  lenv <- plot_object$new_environment()
  lenv$plot_legend <- function(x, legend.loc, legend.names, col, ncol, on, bty, text.col, ...){
    if(is.na(on[1])){
      yrange <- c(0, 1)
    } else {
      panel <- x$get_active_panel()
      yrange <- panel$ylim_render
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
    this_panel <- plot_object$new_panel(ylim = c(0, 1),
                                        asp = 0.8,
                                        envir = lenv,
                                        header = "")

    # legend data
    this_panel$add_action(exp, env = lenv)

  } else {
    for(i in on) {
      plot_object$add_panel_action(i, exp, lenv)
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


#' Add a polygon to an existing xts plot
#' 
#' Draw a polygon on an existing xts plot by specifying a time series of y
#' coordinates. The xts index is used for the x coordinates and the first two
#' columns are the upper and lower y coordinates, respectively.
#' 
#' @param x An xts object to plot. Must contain 2 columns for the upper and
#'   the lower y coordinates for the polygon. The first column is interpreted
#'   as upper y coordinates and the second column as the lower y coordinates.
#' @param y `NULL`, not used.
#' @param main Main title for a new panel, if drawn.
#' @param on Panel number to draw on. A new panel will be drawn if `on = NA`.
#' @param col Color palette to use, set by default to rational choices.
#' @param \dots Any other passthrough parameters to [`par()`].
#' 
#' @author Ross Bennett
#' 
#' @references Based on code by Dirk Eddelbuettel from
#' <http://dirk.eddelbuettel.com/blog/2011/01/16/>
#' 
#' @examples
#' 
#' \dontrun{
#' library(xts)
#' data(sample_matrix)
#' x <- as.xts(sample_matrix)[,1]
#' ix <- index(x["2007-02"])
#' shade <- xts(matrix(rep(range(x), each = length(ix)), ncol = 2), ix)
#' 
#' plot(x)
#' 
#' # set on = -1 to draw the shaded region *behind* the main series
#' addPolygon(shade, on = -1, col = "lightgrey")
#' }
#' 
addPolygon <- function(x, y=NULL, main="", on=NA, col=NULL, ...){
  # add polygon to xts plot based on http://dirk.eddelbuettel.com/blog/2011/01/16/
  
  # some simple checks
  x <- try.xts(x)
  if(!is.null(y)) stop("y is not null")
  if(ncol(x) > 2) warning("more than 2 columns detected in x, only the first 2 will be used")
  
  plot_object <- current.xts_chob()
  lenv <- plot_object$new_environment()
  lenv$plot_lines <- function(x, ta, on, col, ...){
    xdata <- x$Env$xdata
    xsubset <- x$Env$xsubset
    xDataSubset <- xdata[xsubset]
    if(is.null(col)) col <- x$Env$theme$col

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
    # NAs in the coordinates break the polygon which is not the behavior we want
    ta.y <- na.omit(ta.y)
    
    # x coordinates
    n <- seq_len(NROW(ta.y))
    xx <- x$get_xcoords(ta.y)[c(1, n, rev(n))]

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
    this_panel <- plot_object$new_panel(ylim = lenv$ylim,
                                        asp = 1,
                                        envir = lenv,
                                        header = main)

    # plot data
    this_panel$add_action(exp, env = lenv)

  } else {
    for(i in on) {
      plot_object$add_panel_action(i, exp, lenv)
    }
  }
  plot_object
}# polygon


# Based on quantmod/R/replot.R
new.replot_xts <- function(panel=1,asp=1,xlim=c(1,10),ylim=list(structure(c(1,10),fixed=FALSE))) {
  # global variables

  # 'Env' is mainly the environment for the plot window, but some elements are for panels/frames
  Env <- new.env()
  Env$active_panel_i <- panel
  Env$asp   <- 1
  Env$xlim  <- xlim  # vector: c(min, max) (same for every panel)
  Env$last_action_panel_id <- 1
  
  # getters
  get_ylim  <- function() { update_panels(); get_active_panel()[["ylim_render"]] }
  get_xlim  <- function() { update_panels(); Env$xlim }
  get_active_panel <- function() { get_panel(Env$active_panel_i) }
  get_last_action_panel <- function() { get_panel(Env$last_action_panel_id) }
  get_panel <- function(n)
  {
      if (n == 0) {
          get_last_action_panel()
      } else if (n > 0) {
          Env$panels[[n]]
      } else {
          stop("'n' must be a positive integer")
      }
  }

  add_panel_action <-
  function(id,
           expr,
           env,
           clip = TRUE,
           where = c("last", "first", "background"),
           ...)
  {
      if (id < 0) {
          where <- "first"
      } else {
          where <- match.arg(where)
      }
      this_panel <- get_panel(abs(id))
      this_panel$add_action(expr, env, clip, where, ...)
  }

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
        adj <- sign(lim[1L]) * const_y_mult
        lim <- lim[1L] * c(1 - adj, 1 + adj)
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

      # main header asp is always 4% of the grand total asp
      main_title_asp <- 0.04 * sum(all_asp)

      all_asp <- c(main_title_asp, all_asp)
      n_asp <- length(all_asp)

      # render main plot header and x-axis
      plot.window(Env$xlim, c(0, 1))
      clip(par("usr")[1], par("usr")[2], 0, 1)
      eval(Env$main_header_expr, Env)  # header
      eval(Env$main_xaxis_expr,  Env)  # x-axis

      # render each panel
      for (panel_n in seq_along(Env$panels)) {

          panel <- Env$panels[[panel_n]]
          # set the current active panel for the entire plot
          Env$active_panel_i <- panel_n

          is_header <- TRUE  # header is always the first action

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
      temp_xts <- .xts(seq_along(xcoords), xcoords, tzone = tzone(xts_object))
      xcoords <- merge(temp_xts, xts_object,
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

  # main plot header
  Env$main_header_expr <- expression({
      local({
      text(x = xlim[1],
           y = 1.0,
           labels = main,
           adj = c(0, 1),
           cex = 1.1,
           col = theme$labels,
           font = 2)

      if (main.timespan) {
          text(x = xlim[2],
               y = 1.0,
               labels = paste(start(xdata[xsubset]),
                              end(xdata[xsubset]), sep = " / "),
               adj = c(1, 1),
               cex = 1,
               col = theme$labels,
               font = NULL)
      }
      }, new.env(TRUE, Env))
  })

  # main plot x-axis
  Env$main_xaxis_expr <- expression({
      local({
      # add observation level ticks on x-axis if < 400 obs.
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

      # and major and/or minor x-axis ticks and labels
      xcoords <- get_xcoords()
      x_index <- get_xcoords(at_posix = TRUE)
      x_data <- .xts(, x_index, tzone = tzone(xdata))[xsubset]

      use_major <- !isNullOrFalse(major.ticks)
      use_minor <- !isNullOrFalse(minor.ticks)

      types <- c("major", "minor")[c(use_major, use_minor)]
      for (type in types) {
          if (type== "major") {
              axt <- axTicksByTime(x_data,
                                    ticks.on = major.ticks,
                                    format.labels = format.labels)
              labels <- names(axt)
              lwd.ticks <- 1.5
          } else {
              axt <- axTicksByTime(x_data,
                                    ticks.on = minor.ticks,
                                    format.labels = format.labels)
              labels <- FALSE
              lwd.ticks <- 0.75
          }


          axis(1,
               at = xcoords[axt],
               labels = labels,
               las = theme$las,
               lwd.ticks = lwd.ticks,
               mgp = c(3,1.5,0),
               tcl = -0.4,
               cex.axis = theme$cex.axis,
               col = theme$labels,
               col.axis = theme$labels)
      }
      }, new.env(TRUE, Env))
  })

  # panel functionality
  Env$panels <- list()
  new_panel <-
  function(ylim,
           asp,
           envir,
           header,
           ...,
           use_fixed_ylim = FALSE,
           draw_left_yaxis = NULL,
           draw_right_yaxis = NULL,
           use_log_yaxis = FALSE,
           title_timespan = FALSE)
  {
      panel <- new.env(TRUE, envir)
      panel$id <- length(Env$panels) + 1
      panel$asp <- c(header = 0.25, series = asp)
      panel$ylim <- ylim
      panel$ylim_render <- ylim
      panel$use_fixed_ylim <- isTRUE(use_fixed_ylim)
      panel$draw_left_yaxis  <- ifelse(is.null(draw_left_yaxis),  Env$theme$lylab, draw_left_yaxis)
      panel$draw_right_yaxis <- ifelse(is.null(draw_right_yaxis), Env$theme$rylab, draw_right_yaxis)
      panel$use_log_yaxis <- isTRUE(use_log_yaxis)
      panel$header <- header

      ### actions
      panel$actions <- list()
      panel$add_action <-
      function(expr,
               env = Env,
               clip = TRUE,
               where = c("last", "first", "background"),
               ...)
      {
          if (!is.expression(expr)) {
              expr <- as.expression(expr)
          }

          action <- structure(expr, clip = clip, env = env, ...)
          panel$actions <-
              switch(match.arg(where),
                     last = {
                         # after all the existing actions
                         append(panel$actions, list(action))
                     },
                     first = {
                         # after the header and grid lines
                         append(panel$actions, list(action), after = 3)
                     },
                     background = {
                         # after the header (which must be the 1st panel action)
                         append(panel$actions, list(action), after = 1)
                     })

          Env$last_action_panel_id <<- panel$id
      }

      ### header
      # NOTE: this must be the 1st action for a panel
      header_expr <-
          expression({
              text(x = xlim[1],
                   y = 0.3,
                   labels = header,
                   adj = c(0, 0),
                   pos = 4,
                   offset = 0,
                   cex = 0.9,
                   col = theme$labels,
                   font = NULL)
          })
      panel$add_action(header_expr, env = panel)

      ### y-axis
      yaxis_expr <- expression({

          if (use_fixed_ylim) {
              # use the ylim argument
              yl <- ylim
          } else {
              # use the updated ylim based on all panel data
              yl <- ylim_render
          }

          # y-axis grid line labels and locations
          if (use_log_yaxis) {
              ylim_series <- exp(ylim_render)
              # labels are based on the raw series values
              grid_lbl <- pretty(ylim_series, Env$yaxis.ticks)
              grid_lbl <- grid_lbl[grid_lbl >= ylim_series[1] & grid_lbl <= ylim_series[2]]
              # locations are based on the log series values
              grid_loc <- log(grid_lbl)
          } else {
              grid_loc <- pretty(yl, Env$yaxis.ticks)
              grid_loc <- grid_loc[grid_loc >= yl[1] & grid_loc <= yl[2]]
              grid_lbl <- grid_loc
          }

          # draw y-axis grid lines
          segments(x0 = xlim[1], y0 = grid_loc,
                   x1 = xlim[2], y1 = grid_loc,
                   col = theme$grid,
                   lwd = grid.ticks.lwd,
                   lty = grid.ticks.lty)

          # draw left y-axis grid labels
          if (draw_left_yaxis) {
              text(x = xlim[1],
                   y = grid_loc,
                   labels = format(grid_lbl, justify = "right"),
                   col = theme$labels,
                   srt = theme$srt,
                   offset = 0.5,
                   pos = 2,
                   cex = theme$cex.axis,
                   xpd = TRUE)
          }

          # draw right y-axis grid labels
          if (draw_right_yaxis) {
              text(x = xlim[2],
                   y = grid_loc,
                   labels = format(grid_lbl, justify = "right"),
                   col = theme$labels,
                   srt = theme$srt,
                   offset = 0.5,
                   pos = 4,
                   cex = theme$cex.axis,
                   xpd = TRUE)
          }

          # draw y-axis label
          title(ylab = ylab[1], mgp = c(1, 1, 0))
      })
      panel$add_action(yaxis_expr, env = panel)

      # x-axis grid
      xaxis_action <- expression(x_grid_lines(xdata, grid.ticks.on, par("usr")[3:4]))
      panel$add_action(xaxis_action, env = panel)

      # append the new panel to the panel list
      Env$panels <- append(Env$panels, list(panel))

      return(panel)
  }

  update_panels <- function(headers=TRUE) {

    # Recalculate each panel's 'ylim_render' value based on the
    # 'xdata' of every action in the panel
    for (panel_n in seq_along(Env$panels)) {

      panel <- get_panel(panel_n)

      if (!panel$use_fixed_ylim) {
        # set 'ylim_render' to +/-Inf when ylim is NOT fixed, so
        # it will be updated to include all the panel's data
        panel$ylim_render <- c(Inf, -Inf)

        # calculate a new ylim based on all the panel's data
        for (action in panel$actions) {

          action_env <- attr(action, "env")
          action_data <- action_env$xdata

          if (!is.null(action_data)) {
            # some actions (e.g. addLegend) do not have 'xdata'
            dat.range <- create_ylim(action_data[Env$xsubset])

            # calculate new ylim based on the combination of the panel's
            # original ylim and the action's 'xdata' ylim
            new_ylim <-
              c(min(panel$ylim[1], dat.range, na.rm = TRUE),
                max(panel$ylim[2], dat.range, na.rm = TRUE))

            # set to new ylim values
            panel$ylim_render <- new_ylim
          }
        }
      }

      if (panel$use_log_yaxis) {
          panel$ylim_render <- log(panel$ylim_render)
      }
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


  # return
  replot_env <- new.env()
  class(replot_env) <- c("replot_xts","environment")
  replot_env$Env <- Env
  replot_env$new_panel <- new_panel
  replot_env$get_xcoords <- get_xcoords
  replot_env$update_panels <- update_panels
  replot_env$render_panels <- render_panels
  replot_env$get_panel <- get_panel
  replot_env$add_panel_action <- add_panel_action
  replot_env$get_xlim <- get_xlim
  replot_env$get_ylim <- get_ylim
  replot_env$create_ylim <- create_ylim
  replot_env$get_active_panel <- get_active_panel
  replot_env$get_last_action_panel <- get_last_action_panel

  replot_env$new_environment <- function() { new.env(TRUE, Env) }

  # function to plot the x-axis grid lines
  replot_env$Env$x_grid_lines <- function(x, ticks.on, ylim)
  {
    if (isNullOrFalse(ticks.on)) {
      invisible()
    } else {
      if (isTRUE(ticks.on)) ticks.on <- "auto"

      xcoords <- get_xcoords()
      x_index <- get_xcoords(at_posix = TRUE)
      atbt <- axTicksByTime(.xts(, x_index, tzone = tzone(x)),
                            ticks.on = ticks.on)

      segments(xcoords[atbt], ylim[1L],
               xcoords[atbt], ylim[2L],
               col = Env$theme$grid,
               lwd = Env$grid.ticks.lwd,
               lty = Env$grid.ticks.lty)
    }
  }

  return(replot_env)
}

str.replot_xts <- function(object, ...) {
  print(str(unclass(object)))
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
  # reset par
  on.exit(par(xpd = oxpd$xpd, cex = ocex$cex, mar = omar$mar, bg = obg$bg))

  x$render_panels()

  do.call("clip", as.list(usr))  # reset clipping region

  invisible(x$Env$actions)
}
