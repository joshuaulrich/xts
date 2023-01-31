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
           show.nrows = 10,
           trunc.rows = 40,
           trunc.cols = NULL)
{
  check.TZ(x)

  nr <- NROW(x)
  nc <- NCOL(x)

  # 'max' in print.default() takes precedence over 'show.nrows'
  if (hasArg("max")) {
    # 'max' is the number of *elements* (not rows) to print
    if (nr < 1) {
      show.nrows <- 0
    } else {
      # convert 'max' to 'show.nrows'
      max.arg <- match.call()$max
      if (!is.null(max.arg)) {
        show.nrows <- trunc(max.arg / nc)
      }
    }
  } else if (missing(show.nrows)) {
    show.nrows <- getOption("xts.print.show.nrows", 10)
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

  if (!is.null(trunc.cols)) {
    if (isTRUE(trunc.cols)) {
      # capture print output until 1 column would wrap
      capture.printed.cols <-
        function(i, obj)
      {
        o <- utils::capture.output(print.default(obj[, seq_len(i)]))
        max(nchar(o)) # max characters printed for this number of cols
      }

      pcols <- sapply(seq_len(nc), capture.printed.cols, obj = x, ...)
      nc <- which.min(getOption("width") / pcols)
    } else if (is.numeric(trunc.cols) && is.finite(trunc.cols)) {
      nc <- trunc.cols
    }
  }

  seq.max <- seq_len(max)
  seq.col <- seq_len(nc)

  if (nr > trunc.rows + 1) {
    index <- as.character(index(x))
    index <- c(index[seq.max], "...", index[(nr-max+1):nr])
    y <- rbind(
      format(as.matrix(x[seq.max, seq.col])),
      format(matrix(rep("", nc), nrow = 1)),
      format(as.matrix(x[(nr-max+1):nr, seq.col]))
    )
    rownames(y) <- format(index, justify = "right")
    colnames(y) <- colnames(x[, seq.col])
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
    # ensure 'y' has dims
    if (is.null(dim(y))) {
      dim(y) <- c(length(y), 1)
    }
    # Create column names as right-justified column indexes. They're left-
    # justified by default, which is different than if there are column names.
    if (is.null(colnames(y))) {
      cindex <- utils::capture.output(print(y[1,,drop = FALSE]))[1]
      cindex <- sub("\\s+$", "", cindex)
      cindex <- paste0(strsplit(cindex, "]")[[1]], "]")
      cchar <- nchar(cindex)
      if (NCOL(y) == 1) {
        cindex <- cindex[1]
      }
      cindex[1] <- substring(cindex[1], cchar[1] - min(cchar), cchar[1])
      colnames(y) <- cindex
    }

    print(y, quote = quote, right = right, ...)
  }

  invisible(x)
}
