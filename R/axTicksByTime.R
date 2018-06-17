#
#   xts: eXtensible time-series 
#
#   Copyright (C) 2008  Jeffrey A. Ryan jeff.a.ryan @ gmail.com
#
#   Contributions from Joshua M. Ulrich
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


axTicksByTime <-
function(x, ticks.on = "auto", k = 1, labels = TRUE, format.labels = TRUE,
         ends = TRUE, gt = 2, lt = 30)
{
  # if a vector of times/dates, convert to dummy xts object
  if (timeBased(x)) {
    x <- xts(rep(1, length(x)), x)
  }

  ticks.on <- ticks.on[1L]
  # special-case for "secs" and "mins"
  if (ticks.on == "secs" || ticks.on == "mins") {
    ticks.on <- substr(ticks.on, 1L, 3L)
  }

  tick.opts <- c("years", "months", "weeks", "days", "hours",
    "minutes", "seconds")
  ticks.on <- match.arg(ticks.on, c("auto", tick.opts))

  if (ticks.on == "auto") {
    tick.k.opts <- c(10, 5, 2, 1, 6, 1, 1, 1, 4, 2, 1, 30, 15, 1, 1)
    tick.opts <- rep(tick.opts, c(4, 2, 1, 1, 3, 3, 1))
    is <- structure(rep(0, length(tick.opts)), .Names = tick.opts)
    for (i in 1:length(tick.opts)) {
      ep <- endpoints(x, tick.opts[i], tick.k.opts[i])
      is[i] <- length(ep) - 1
      if (is[i] > lt) {
        break
      }
    }
    loc <- rev(which(is > gt & is < lt))[1L]
    cl <- tick.opts[loc]
    ck <- tick.k.opts[loc]
  } else {
    cl <- ticks.on[1L]
    ck <- k
  }

  if (is.null(cl) || is.na(cl) || is.na(ck)) {
    ep <- c(0, NROW(x))
  } else {
    ep <- endpoints(x, cl, ck)
  }
  if (ends) {
    ep <- ep + c(rep(1, length(ep) - 1), 0)
  }

  if (labels) {
    if (is.logical(format.labels) || is.character(format.labels)) {
      # format by platform...
      unix <- (.Platform$OS.type == "unix")
      # ...and level of time detail
      fmt <- switch(periodicity(x)$scale,
        weekly    = ,
        daily     = if (unix) "%b %d%n%Y" else "%b %d %Y",
        minute    = ,
        hourly    = if (unix) "%b %d%n%H:%M" else "%b %d %H:%M",
        seconds   = if (unix) "%b %d%n%H:%M:%S" else "%b %d %H:%M:%S",
                    if (unix) "%n%b%n%Y" else "%b %Y")

      # special case yearqtr index
      if (inherits(index(x), "yearqtr")) {
        fmt <- "%Y-Q%q"
      }

      if (is.character(format.labels)) {
        fmt <- format.labels
      }
      names(ep) <- format(index(x)[ep], fmt)
    } else {
      names(ep) <- as.character(index(x)[ep])
    }
  }
  ep
}
