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

.mean_by_column_message <-
function(caller)
{
    if (getOption("xts.message.period.apply.mean", TRUE)) {
        message("NOTE: `", caller, "(..., FUN = mean)` operates by column, unlike other math\n  ",
                "functions (e.g. median, sum, var, sd). Please use `FUN = colMeans` instead,\n  ",
                "and use `FUN = function(x) mean(x)` to take the mean of all columns. Set\n  ",
                "`options(xts.message.period.apply.mean = FALSE)` to suppress this message.")
    }

    # changing this behavior will break code in the following dependencies:
    #
    # ATAforecasting/R/ATA_Find_Multi_Freq.R
    # bidask/R/utils.R
    # dsa/R/HelperFunctions.R # {.tomonth}
    # RavenR/inst/doc/Introduction_to_RavenR.R
    # RavenR/inst/doc/Introduction_to_RavenR.Rmd
    # RavenR/R/rvn_apply_wyearly.R
    # RavenR/R/rvn_monthly_vbias.R
    # rts/man/apply.monthly.Rd
    # rts/man/period.apply.Rd
    # RWDataPlyr/R/xts_helperFunctions.R
}


#' Apply Function Over Specified Interval
#' 
#' Apply a specified function to data over intervals specified by `INDEX`. The
#' intervals are defined as the observations from `INDEX[k]+1` to `INDEX[k+1]`,
#' for `k = 1:(length(INDEX)-1)`.
#' 
#' Similar to the rest of the apply family, `period.apply()` calculates the
#' specified function's value over a subset of data. The primary difference is
#' that `period.apply()` applies the function to non-overlapping intervals of a
#' vector or matrix.
#' 
#' Useful for applying functions over an entire data object by any
#' non-overlapping intervals. For example, when `INDEX` is the result of a
#' call to `endpoints()`.
#' 
#' `period.apply()` checks that `INDEX` is sorted, unique, starts with 0, and
#' ends with `nrow(x)`. All those conditions are true of vectors returned by
#' `endpoints()`.
#' 
#' @param x The data that `FUN` will be applied to.
#' @param INDEX A numeric vector of index breakpoint locations. The vector
#'   should begin with 0 and end with `nrow(x)`.
#' @param FUN A function to apply to each interval in `x`.
#' @param \dots Additional arguments for `FUN`.
#' 
#' @return An object with `length(INDEX) - 1` observations, assuming `INDEX`
#'   starts with 0 and ends with `nrow(x)`.
#' 
#' @note When `FUN = mean` the results will contain one column for every
#' column in the input, which is different from other math functions (e.g.
#' `median`, `sum`, `prod`, `sd`, etc.).
#' 
#' `FUN = mean` works by column because the default method `stats::mean`
#' previously worked by column for matrices and data.frames. R Core changed the
#' behavior of `mean` to always return one column in order to be consistent
#' with the other math functions. This broke some \pkg{xts} dependencies and
#' `mean.xts()` was created to maintain the original behavior.
#' 
#' Using `FUN = mean` will print a message that describes this inconsistency.
#' To avoid the message and confusion, use `FUN = colMeans` to calculate means
#' by column and use `FUN = function(x) mean` to calculate one mean for all the
#' data. Set `options(xts.message.period.apply.mean = FALSE)` to suppress this
#' message.
#' 
#' @author Jeffrey A. Ryan, Joshua M. Ulrich
#' 
#' @seealso [`endpoints()`] [`apply.monthly()`]
#' 
#' @keywords utilities
#' @examples
#' 
#' zoo.data <- zoo(rnorm(31)+10,as.Date(13514:13744,origin="1970-01-01"))
#' ep <- endpoints(zoo.data,'weeks')
#' period.apply(zoo.data, INDEX=ep, FUN=function(x) colMeans(x))
#' period.apply(zoo.data, INDEX=ep, FUN=colMeans)                  #same
#' 
#' period.apply(letters,c(0,5,7,26), paste0)
#' 
`period.apply` <-
function(x, INDEX, FUN, ...)
{
    if (deparse(substitute(FUN))[1] == "mean") {
      .mean_by_column_message("period.apply")
    }
    x <- try.xts(x, error = FALSE)
    FUN <- match.fun(FUN)

    if(!isOrdered(INDEX)) {
      # isOrdered returns FALSE if there are duplicates
      INDEX <- sort(unique(INDEX))
    }
    if(INDEX[1] != 0) {
      INDEX <- c(0, INDEX)
    }
    if(last(INDEX) != NROW(x)) {
      INDEX <- c(INDEX, NROW(x))
    }

    xx <- sapply(1:(length(INDEX) - 1), function(y) {
                   FUN(x[(INDEX[y] + 1):INDEX[y + 1]], ...)
                })
    if(is.vector(xx))
      xx <- t(xx)
    xx <- t(xx)
    if(is.null(colnames(xx)) && NCOL(x)==NCOL(xx))
      colnames(xx) <- colnames(x)
    reclass(xx, x[INDEX])
}

#' @rdname apply.monthly
`apply.daily` <-
function(x,FUN, ...)
{
  if (deparse(substitute(FUN))[1] == "mean") {
    .mean_by_column_message("apply.daily")
  }
  ep <- endpoints(x,'days')
  period.apply(x,ep,FUN, ...)
}

#' @rdname apply.monthly
`apply.weekly` <-
function(x,FUN, ...)
{
  if (deparse(substitute(FUN))[1] == "mean") {
    .mean_by_column_message("apply.weekly")
  }
  ep <- endpoints(x,'weeks')
  period.apply(x,ep,FUN, ...)
}


#' Apply Function over Calendar Periods
#' 
#' Apply a specified function to each distinct period in a given time series
#' object.
#' 
#' Simple mechanism to apply a function to non-overlapping time periods, e.g.
#' weekly, monthly, etc. Different from rolling functions in that this will
#' subset the data based on the specified time period (implicit in the call),
#' and return a vector of values for each period in the original data.
#' 
#' Essentially a wrapper to the \pkg{xts} functions `endpoints()` and
#' `period.apply()`, mainly as a convenience.
#' 
#' @param x A time-series object coercible to xts.
#' @param FUN A function to apply to each period.
#' @param \dots Additional arguments to `FUN`.
#'
#' @return A vector of results produced by `FUN`, corresponding to the
#' appropriate periods.
#'
#' @note When `FUN = mean` the results will contain one column for every
#' column in the input, which is different from other math functions (e.g.
#' `median`, `sum`, `prod`, `sd`, etc.).
#' 
#' `FUN = mean` works by column because the default method `stats::mean`
#' previously worked by column for matrices and data.frames. R Core changed the
#' behavior of `mean` to always return one column in order to be consistent
#' with the other math functions. This broke some \pkg{xts} dependencies and
#' `mean.xts()` was created to maintain the original behavior.
#' 
#' Using `FUN = mean` will print a message that describes this inconsistency.
#' To avoid the message and confusion, use `FUN = colMeans` to calculate means
#' by column and use `FUN = function(x) mean` to calculate one mean for all the
#' data. Set `options(xts.message.period.apply.mean = FALSE)` to suppress this
#' message.
#' 
#' @author Jeffrey A. Ryan
#' 
#' @seealso [`endpoints()`], [`period.apply()`], [`to.monthly()`]
#' 
#' @keywords utilities
#' @examples
#' 
#' xts.ts <- xts(rnorm(231),as.Date(13514:13744,origin="1970-01-01"))
#' 
#' start(xts.ts)
#' end(xts.ts)
#' 
#' apply.monthly(xts.ts,colMeans)
#' apply.monthly(xts.ts,function(x) var(x))
#' 
`apply.monthly` <-
function(x,FUN, ...)
{
  if (deparse(substitute(FUN))[1] == "mean") {
    .mean_by_column_message("apply.monthly")
  }
  ep <- endpoints(x,'months')
  period.apply(x,ep,FUN, ...)
}

#' @rdname apply.monthly
`apply.quarterly` <-
function(x,FUN, ...)
{
  if (deparse(substitute(FUN))[1] == "mean") {
    .mean_by_column_message("apply.quarterly")
  }
  ep <- endpoints(x,'quarters')
  period.apply(x,ep,FUN, ...)
}

#' @rdname apply.monthly
`apply.yearly` <-
function(x,FUN, ...)
{
  if (deparse(substitute(FUN))[1] == "mean") {
    .mean_by_column_message("apply.yearly")
  }
  ep <- endpoints(x,'years')
  period.apply(x,ep,FUN, ...)
}

period_apply <- function(x, INDEX, FUN, ...) {
  fun <- substitute(FUN)
  e <- new.env()

  if (INDEX[1] != 0) {
    INDEX <- c(0, INDEX)
  }

  if (INDEX[length(INDEX)] != NROW(x)) {
    INDEX <- c(INDEX, NROW(x))
  }

  pl <- .Call(C_xts_period_apply, x, INDEX, fun, e)

  .xts(do.call(rbind, pl), .index(x)[INDEX], tclass = tclass(x), tzone = tzone(x))
}
