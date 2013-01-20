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
.makeISO8601 <- function(x) {
  paste(start(x),end(x),sep="/")
}

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
     mon=as_numeric(MM),
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
 if(!missing(start)) {
   start <- as.numeric(start)
   #s <- as.POSIXlt(structure(max(start, as.numeric(s), na.rm=TRUE),
   #        class=c("POSIXt","POSIXct"),tz=tz))
   s <- as.POSIXlt(.POSIXct(max(start, as.numeric(s), na.rm=TRUE),tz=tz))
 }
 if(!missing(end)) {
   end <- as.numeric(end)
   #e <- as.POSIXlt(structure(min(end, as.numeric(e), na.rm=TRUE),
   #        class=c("POSIXt","POSIXct"),tz=tz))
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
                    class=c("POSIXt","POSIXlt"), tzone=attr(e,"tzone")))
   } else {
     e <- as.POSIXct(structure(as.list(mapply(`+`,s,parse_duration(DURATION))),
                    class=c("POSIXt","POSIXlt"), tzone=attr(e,"tzone")))
   }
 }

 list(first.time=as.POSIXct(s),last.time=as.POSIXct(e))
}

.makeISO8601TT <- function(x, from, to) {
  # parse T08:30/T15:00 queries across all
  # available days into a vector of character
  # strings compatible with the xts/ISO8601
  # subsetting.  Thanks to Brian Peterson for
  # the challenge ;-)
  ep <- endpoints(x, on='days')
  start_of <- (ep+1)[-length(ep)]
  end_of   <- ep[-1]
  if(as.numeric(from) < as.numeric(to)) {
    paste(format(index(x[start_of]),paste("%Y%m%d",from,sep="T")),
          format(index(x[end_of  ]),paste("%Y%m%d",to  ,sep="T")),sep="/")
  } else {
    c(
    paste(format(index(x[start_of]),"%Y%m%d"),
          format(index(x[end_of  ]),paste("%Y%m%d",to  ,sep="T")),sep="/"),
    paste(format(index(x[start_of]),paste("%Y%m%d",from,sep="T")),
          format(index(x[end_of  ]),"%Y%m%d"),sep="/")
    )
  }
}
