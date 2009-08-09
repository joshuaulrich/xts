# This function corresponds to the ISO 8601 standard
# for specifying dates and times as described in
# the ISO 8601:2004e standard.
#
#
#
#
# This implementation is currently restricted
# to interval based parsing, with basic or
# extended formats.
#
# The return value is a list of start and
# end times, in POSIXt space.
#
# Copyright 2009. Jeffrey A. Ryan. All rights reserved.
# This is licensed under the GPL version 3

parseISO8601 <-
function(x) {
 # possibly include a lower= and upper= bound
 # as prescribed by context.
 as_numeric <- function(.x) {
   if(gsub(" ","",.x)=="")
     NULL
   else as.numeric(.x)
 }
 intervals <- unlist(strsplit(x, "/|(--)"))
 # "" "xxx" end of defined, needs context
 # "xxx"    start of defined, needs context

 parse.side <- function(x, startof) {
   if( is.na(x) || !nzchar(x))
     return(c(NULL))
   basic <- gsub(":|-", "", x, perl=TRUE, extended=TRUE)
   date.time <- unlist(strsplit(basic, " |T"))

   # dates
   date <- date.time[1]
   if(!missing(startof) &&
      (nchar(startof) - nchar(date)) >= 4) {
      # FIXME 200901/2009 needs to work, fix is ex-post now
      # pad to last place of startof
      # with startof values
      startof <- gsub(":|-", "", startof, perl=TRUE, extended=TRUE)
      sstartof <- substr(startof,0,nchar(startof)-nchar(date))
      date <- paste(sstartof,date,sep="")
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
   S <- substr(time,5,6)
   } else H<-M<-S<-""

   # return as list
   as.list(c(
     year=as_numeric(YYYY),
     mon=as_numeric(MM),
     day=as_numeric(DD),
     hour=as_numeric(H),
     min=as_numeric(M),
     sec=as_numeric(S)
     )
   )
 }

 s <- e <- NA
 if(nzchar(intervals[1]))
   s <- do.call(firstof, parse.side(intervals[1]))
 if(length(intervals) == 2L) {
   e <- do.call(lastof,  parse.side(intervals[2],intervals[1]))
   if(is.na(e))
     e <- do.call(lastof,  parse.side(intervals[2]))
 }
 list(s,e)
}
