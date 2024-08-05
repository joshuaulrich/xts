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


# This function corresponds to the ISO 8601 standard
# for specifying dates and times as described in
# the ISO 8601:2004e standard.
#
#  See:
#    http://en.wikipedia.org/wiki/ISO_8601
#    http://www.iso.org/iso/support/faqs/faqs_widely_used_standards/widely_used_standards_other/date_and_time_format.htm
#
# This implementation is currently restricted
# to interval based parsing, with basic or
# extended formats, and duration strings.
# Currently the duration must be in basic format
#  e.g. PnnYnnMnnDTnnHnnMnnS
#
# The return value is a list of start and
# end times, in POSIXt space.
#
# Copyright 2009. Jeffrey A. Ryan. All rights reserved.
# This is licensed under the GPL version 2 or later
#

#' Create an ISO8601 string from a time series object
#' 
#' This function uses the `start()` and `end()` of a time series object to
#' create an ISO8601 string that spans the time range of the object.
#' 
#' This is not exported an therefore not part of the official xts API.
#' 
#' @param x A time series object with `start()` and `end()` methods.
#' 
#' @return A character vector of length one describing the ISO-style format
#' for the range of a given time series object.
#' 
#' @noRd
#' @examples
#'
#' data(sample_matrix)
#' x <- as.xts(sample_matrix)
#' .makeISO8601(x)
#'
.makeISO8601 <- function(x) {
  paste(start(x), end(x), sep = "/")
}


#' Internal ISO 8601:2004(e) Time Parser
#' 
#' This function replicates most of the ISO standard for parsing times and
#' time-based ranges in a universally accepted way. The best documentation is
#' the official ISO page as well as the Wikipedia entry for ISO 8601:2004.
#' 
#' The basic idea is to create the endpoints of a range, given a string
#' representation. These endpoints are aligned in POSIXct time to the zero
#' second of the day at the beginning, and the 59.9999th second of the 59th
#' minute of the 23rd hour of the final day.
#' 
#' For dates prior to the epoch (1970-01-01) the ending time is aligned to the
#' 59.0000 second. This is due to a bug/feature in the \R implementation of
#' `as.POSIXct()` and `mktime0()` at the C-source level. This limits the
#' precision of ranges prior to 1970 to 1 minute granularity with the current
#' \pkg{xts} workaround.
#' 
#' Recurring times over multiple days may be specified using the "T" notation.
#' See the examples for details.
#' 
#' @param x A character string conforming to the ISO 8601:2004(e) rules.
#' @param start Lower constraint on range.
#' @param end Upper constraint of range
#' @param tz Timezone (tzone) to use internally.
#' 
#' @return A two element list with an entry named \sQuote{first.time} and
#'   one named \sQuote{last.time}.
#' 
#' @note There is no checking done to test for a properly constructed ISO
#' format string. This must be correctly entered by the user.
#' 
#' When using durations, it is important to note that the time of the duration
#' specified is not necessarily the same as the realized periods that may be
#' returned when applied to an irregular time series. This is not a bug, it is
#' a standards and implementation gotcha.
#' 
#' @author Jeffrey A. Ryan
#' 
#' @references <https://en.wikipedia.org/wiki/ISO_8601>\cr
#' <https://www.iso.org/iso-8601-date-and-time-format.html>
#' 
#' @aliases ISO8601 parseISO8601
#' @rdname parseISO8601
#' 
#' @keywords utilities
#' @examples
#' 
#' # the start and end of 2000
#' .parseISO8601('2000')
#' 
#' # the start of 2000 and end of 2001
#' .parseISO8601('2000/2001')
#' 
#' # May 1, 2000 to Dec 31, 2001
#' .parseISO8601('2000-05/2001')
#' 
#' # May 1, 2000 to end of Feb 2001
#' .parseISO8601('2000-05/2001-02')
#' 
#' # Jan 1, 2000 to Feb 29, 2000; note the truncated time on the LHS
#' .parseISO8601('2000-01/02')
#' 
#' # 8:30 to 15:00 (used in xts subsetting to extract recurring times)
#' .parseISO8601('T08:30/T15:00')
#' 
.parseISO8601 <- function(x, start, end, tz="") {
 # x: character vector of length 1 in ISO8601:2004(e) format
 # start: optional earliest time
 # end:   optional latest time
 # tz:    optional tzone to create with
 as_numeric <- function(.x) {
   # simple helper function
   if(gsub(" ","",.x)=="")
     NULL
   else as.numeric(.x)
 }
 x <- gsub("NOW",format(Sys.time(),"%Y%m%dT%H%M%S"),x)
 x <- gsub("TODAY",format(Sys.Date(),"%Y%m%d"),x)

 if(identical(grep("/|(--)|(::)", x), integer(0))) {
   x <- paste(x,x,sep="/")
 }
 intervals <- unlist(strsplit(x, "/|(--)|(::)"))
 # e.g. "/2009":  "" "xxx" end of defined, needs context
 # e.g. "2009/":  "xxx"    start of defined, needs context

 # check for duration specification
 DURATION <- ""
 if(length(intervals)==2L) {
   if(substr(intervals[1],0,1)=="P") {
     # duration on LHS
     DURATION <- intervals[1]
     DURATION_LHS <- TRUE
     intervals[1] <- ""
   }
   if(substr(intervals[2],0,1)=="P") {
     # duration on RHS
     DURATION <- intervals[2]
     DURATION_LHS <- FALSE
     intervals <- intervals[1]
   }
   # leave alone if no duration
 }

 parse.side <- function(x, startof) {
   if( is.na(x) || !nzchar(x))
     return(c(NULL))
   basic <- gsub(":|-", "", x, perl=TRUE) #, extended=TRUE)
   date.time <- unlist(strsplit(basic, " |T"))

   # dates
   date <- date.time[1]
   if(!missing(startof) && nchar(basic)==2L) {
      startof <- gsub(":|-", "", startof, perl=TRUE) #, extended=TRUE)
      if(nchar(startof) - nchar(date) >= 4) {
      # FIXME 200901/2009 needs to work, fix is ex-post now
      # pad to last place of startof
      # with startof values
      sstartof <- substr(startof,0,nchar(startof)-nchar(date))
      date <- paste(sstartof,date,sep="")
   }
   }
   date <- sprintf("%-8s", date)
   YYYY <- substr(date,0,4)
   MM <- substr(date,5,6)
   DD <- substr(date,7,8)

   # times
   time <- date.time[2]
   if( !is.na(time)) {
   time <- sprintf("%-6s", time)
   H <- substr(time,0,2)
   M <- substr(time,3,4)
   S <- substr(time,5,10000L)
   } else H<-M<-S<-""

   # return as list
   c(as.list(c(
     year=as_numeric(YYYY),
     month=as_numeric(MM),
     day=as_numeric(DD),
     hour=as_numeric(H),
     min=as_numeric(M),
     sec=as_numeric(S)
     )
   ),tz=tz)
 }

 s <- e <- NA
 if(nzchar(intervals[1]))  # LHS
   s <- as.POSIXlt(do.call(firstof, parse.side(intervals[1])))
 if(length(intervals) == 2L) { # RHS
   e <- as.POSIXlt(do.call(lastof,  parse.side(intervals[2],intervals[1])))
   if(is.na(e))
     e <- as.POSIXlt(do.call(lastof,  parse.side(intervals[2])))
 }
 if(is.na(s) && is.na(e) && !nzchar(DURATION) && intervals[1L] != "") {
   warning("cannot determine first and last time from ", x)
   return(list(first.time=NA_real_,last.time=NA_real_))
 }
 if(!missing(start)) {
   start <- as.numeric(start)
   #s <- as.POSIXlt(structure(max(start, as.numeric(s), na.rm=TRUE),
   #        class=c("POSIXct","POSIXt"),tz=tz))
   s <- as.POSIXlt(.POSIXct(max(start, as.numeric(s), na.rm=TRUE),tz=tz))
 }
 if(!missing(end)) {
   end <- as.numeric(end)
   #e <- as.POSIXlt(structure(min(end, as.numeric(e), na.rm=TRUE),
   #        class=c("POSIXct","POSIXt"),tz=tz))
   e <- as.POSIXlt(.POSIXct(min(end, as.numeric(e), na.rm=TRUE),tz=tz))
 }
 if(nzchar(DURATION)) {
    parse_duration <- function(P) {
      # TODO:
      #  strip leading P from string
      #  convert second M (min) to 'm' IFF following a T
      #  remove/ignore T
      #  convert extended format (PYYYYMMDD) to basic format (PnnYnnMnnD)
      P <- gsub("P","",P)
      P <- gsub("T(.*)M","\\1m",P)
      n <- unlist(strsplit(P, "[[:alpha:]]"))
      d <- unlist(strsplit(gsub("[[:digit:]]", "", P),""))
      dur.vec <- list(as.numeric(n),unname(c(Y=6,M=5,D=4,H=3,m=2,S=1)[d]))
      init.vec <- rep(0, 9)
      init.vec[dur.vec[[2]]] <- dur.vec[[1]]
      init.vec
    }

   if(DURATION_LHS) {
     s <- as.POSIXct(structure(as.list(mapply(`-`,e,parse_duration(DURATION))),
                    class=c("POSIXlt","POSIXt"), tzone=attr(e,"tzone")))
   } else {
     e <- as.POSIXct(structure(as.list(mapply(`+`,s,parse_duration(DURATION))),
                    class=c("POSIXlt","POSIXt"), tzone=attr(e,"tzone")))
   }
 }

 list(first.time=as.POSIXct(s),last.time=as.POSIXct(e))
}
