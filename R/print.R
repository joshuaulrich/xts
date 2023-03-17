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

print.xts <-
  function(x,
           fmt,
           ...,
           show.rows = 10,
           max.rows = 100)
{
  check.TZ(x)

  nr <- NROW(x)
  nc <- NCOL(x)

  if (missing(max.rows)) {
    # the user didn't specify a value; use the global option value if it's
    # set; if it's not set, use the default value
    max.rows <- getOption("xts.print.max.rows", max.rows)
  }

  # 'max' in print.default() takes precedence over 'show.rows'
  if (hasArg("max")) {
    # 'max' is the number of *elements* (not rows) to print
    if (nr < 1) {
      show.rows <- 0
    } else {
      # convert 'max' to 'show.rows'
      max.arg <- match.call()$max
      if (!is.null(max.arg)) {
        show.rows <- trunc(max.arg / nc)
      }
    }
  } else if (missing(show.rows)) {
    # the user didn't specify a value; use the global option value if it's
    # set; if it's not set, use the default value
    show.rows <- getOption("xts.print.show.rows", show.rows)
  }

  if (missing(fmt)) {
    fmt <- tformat(x)
  }
  if (is.null(fmt)) {
    fmt <- TRUE
  }

  if (!hasArg("quote")) {
    quote <- FALSE
  }
  if (!hasArg("right")) {
    right <- TRUE
  }

  if (nr > max.rows && nr > 2 * show.rows) {
    # 'show.rows' can't be more than 2*nrow(x) or observations will be printed
    # twice, once before the "..." and once after.
    seq.row <- seq_len(show.rows)
    seq.col <- seq_len(nc)
    seq.n <- (nr - show.rows + 1):nr

    index <- c(as.character(index(x)[seq.row]),
               "...",
               as.character(index(x)[seq.n]))

    # as.matrix() to ensure we have dims
    # unclass() avoids as.matrix() method dispatch
    m <- as.matrix(unclass(x))

    # convert to data.frame to format each column individually
    m <- data.frame(m[c(seq.row, seq.n), seq.col, drop = FALSE])
    m[] <- lapply(m, format)
    m <- as.matrix(m)

    # insert blank row between top and bottom rows
    y <- rbind(utils::head(m, show.rows),
               rep("", nc),
               utils::tail(m, show.rows))

    rownames(y) <- format(index, justify = "right")
    colnames(y) <- colnames(m[, seq.col, drop = FALSE])
  } else {
    y <- coredata(x, fmt)
  }

  if (length(y) == 0) {
    if (!is.null(dim(x))) {
      p <- structure(vector(storage.mode(y)), dim = dim(x),
                     dimnames = list(format(index(x)), colnames(x)))
      print(p)
    } else {
      cat('Data:\n')
      print(vector(storage.mode(y)))
      cat('\n')
      cat('Index:\n')
      index <- index(x)
      if (length(index) == 0) {
        print(index)
      } else {
        print(str(index))
      }
    }
  } else {
    # ensure 'y' has dims and row names
    if (is.null(dim(y))) {
      y_names <- as.character(index(x))
      y <- matrix(y, nrow = length(y), dimnames = list(y_names, NULL))
    }
    # Create column names as column indexes.
    if (is.null(colnames(y))) {
      colnames(y) <- paste0("[,", seq_len(ncol(y)), "]")
    }

    print(y, quote = quote, right = right, ...)
  }

  invisible(x)
}
