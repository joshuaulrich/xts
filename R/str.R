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


`str.xts` <-
function(object, ..., ncols = 5)
{

  is.data.empty <- is.null(dim(object)) || sum(dim(object)) == 0
  is.zero.index <- (length(.index(object)) == 0)

  nr <- NROW(object)
  nc <- ifelse(is.data.empty, 0, NCOL(object))

  # "zero-length" xts
  #    * index length == 0, but tclass and tzone are set
  #    * NROW == 0
  #    * NCOL >  0 and may have column names
  # examples:
  #   str(.xts(1, 1)["1900"])
  #   str(.xts(cbind(a = 1, b = 2), 1)["1900"])
  is.zero.length <- (is.zero.index && nr == 0 && !is.data.empty)

  # "zero-width" xts
  #    * index length > 0
  #    * NROW == 0
  #    * NCOL == 0
  # example:
  #   str(.xts(, 1:5))
  is.zero.width <- (!is.zero.index && is.data.empty)

  # "empty" xts
  #    * index length == 0, but tclass and tzone are set
  #    * NROW == 0
  #    * NCOL == 0
  # example:
  #   str(.xts(, numeric(0)))
  #   str(.xts(matrix()[0,0], numeric(0)))
  is.empty <- (is.zero.index && is.data.empty)

  if (is.empty) {
    header <- "An empty xts object"
  } else if (is.zero.length) {
    header <- "A zero-length xts object"
  } else {
    # zero-width and regular xts objects
    if (is.zero.width) {
      header <- "A zero-width xts object on"
    } else {
      header <- "An xts object on"
    }
    time.range <- sub("/", " / ", .makeISO8601(object), fixed = TRUE)
    header <- paste(header, time.range, "containing:")
  }

  cat(header, "\n")

  # Data
  cat(sprintf("  Data:    %s [%d, %d]\n",
              storage.mode(object), nr, nc))

  # Column names
  cnames <- colnames(object)
  if (!is.null(cnames)) {

    if (nc > ncols) {
      more <- nc - ncols
      cname.str <- sprintf("%s ... with %d more %s",
                           paste(cnames[seq_len(ncols)], collapse = ", "),
                           more,
                           ifelse(more > 1, "columns", "column"))
    } else {
      cname.str <- paste(colnames(object), collapse = ", ")
    }

    cat(sprintf("  Columns: %s\n", cname.str))
  }

  # Index
  cat(sprintf("  Index:   %s [%d] (TZ: \"%s\")\n",
              paste(tclass(object), collapse = ","),
              length(.index(object)),
              tzone(object)))

  if (!is.null(CLASS(object))) {
    cat(sprintf("  Original class: '%s'\n", CLASS(object)))
  }

  xts.attr <- xtsAttributes(object)
  if (!is.null(xts.attr)) {
      cat("  xts Attributes:\n")
      str(xts.attr, ..., comp.str = "   $ ", no.list = TRUE)
  }

  invisible(NULL)
}
