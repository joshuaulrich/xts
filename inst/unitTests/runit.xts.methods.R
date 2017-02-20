#
# RUnit tests for the following 'xts' methods:
# rbind
# cbind
#

## test reclass works and throws error
## test xtsAttributes, both CLASS and USER
## test all.equal(CLASS) and !all.equal(CLASS) cases
#
## Create datums :)
#data(sample_matrix)
#date.index <- as.Date(rownames(sample_matrix))
#s1 <- 1:45
#s2 <- 46:90
#s3 <- 91:135
#s4 <- 136:180
#
## create raw 'xts' object
#xtsX <- xts( sample_matrix, order.by=date.index )
## create 'matrix' object
#xtsM <- as.xts( sample_matrix )
## create 'data.frame' object
#xtsDF <- as.xts( data.frame(sample_matrix) )
## create 'zoo' object
#xtsZ <- as.xts( zoo(sample_matrix, date.index) )
## create 'irts' object
#require(tseries)
#xtsIR <- as.xts( irts(as.POSIXct(date.index), sample_matrix) )
## create 'ts' object
#xts.ts <- as.xts( ts(sample_matrix, start=as.numeric(date.index)[1]) )
## create 'timeSeries' object
#xts.TS <- as.xts( timeSeries(sample_matrix, charvec=date.index) )
#
##################################################
## everything :)
#test.cbind_xts <- function() {
#  # Separate 'Date' and 'POSIXt' index
#  ccD <- cbind( xtsZ[,1:2],
#                xts.ts[,3:4] )
#  ccP1 <- cbind( xtsM[,1],
#                 xtsDF[,2],
#                 xtsI[,3],
#                 xtsIR[,4] )
#  ccP2 <- cbind( xtsM[,1],
#                 xtsDF[,2],
#                 xtsIR[,3],
#                 xts.TS[,4] )
#  colnames(xtsX) <- colnames(ccD) <- colnames(ccP1) <- colnames(ccP2) <- NULL
#  checkIdentical(ccP1, ccP2)
##  checkIdentical(ccD, xtsX)
#}
#
#test.rbind_xts <- function() {
#  # Separate 'Date' and 'POSIXt' index
#  rrD <- rbind( xtsZ[c(s1,s2),],
#                xts.ts[c(s3,s4),] )
#  rrP1 <- rbind( xtsM[s1,],
#                 xtsDF[s2,],
#                 xtsI[s3,],
#                 xtsIR[s4,] )
#  rrP2 <- rbind( xtsM[s1,],
#                 xtsDF[s2,],
#                 xtsIR[s3,],
#                 xts.TS[s4,] )
#  checkIdentical(rrP1, rrP2)
##  checkIdentical(rrD, xtsX)
#}
#
##################################################
## matrix
#test.cbind_matrix <- function() {
#  cc <- cbind( xtsM[,1],
#               xtsM[,2],
#               xtsM[,3],
#               xtsM[,4] )
#  xts.M <- xtsM
#  # Remove time-zone attribute of POSIXct index
#  #attr(attr(xts.M, "index"), "tzone") <- NULL
#  colnames(xts.M) <- colnames(cc) <- NULL
#  checkIdentical( cc, xts.M )
#}
#
#test.rbind_matrix <- function() {
#  rr <- rbind( xtsM[s1,],
#               xtsM[s2,],
#               xtsM[s3,],
#               xtsM[s4,] )
#  xts.M <- xtsM
#  # Remove time-zone attribute of POSIXct index
#  #attr(attr(xts.M, "index"), "tzone") <- NULL
#  checkIdentical( rr, xts.M )
#}
#
##################################################
## zoo
#test.cbind_zoo <- function() {
#  cc <- cbind( xtsZ[,1],
#               xtsZ[,2],
#               xtsZ[,3],
#               xtsZ[,4] )
#  xzoo <- xtsZ
#  #rownames(xzoo) <- NULL
#  colnames(xzoo) <- colnames(cc) <- NULL
#  checkIdentical( cc, xzoo )
#}
#
#test.rbind_zoo <- function() {
#  rr <- rbind( xtsZ[s1,],
#               xtsZ[s2,],
#               xtsZ[s3,],
#               xtsZ[s4,] )
#  checkIdentical( rr, xtsZ )
#}
#
##################################################
## data.frame
#test.cbind_data.frame <- function() {
#  cc <- cbind( xtsDF[,1],
#               xtsDF[,2],
#               xtsDF[,3],
#               xtsDF[,4] )
#  xts.DF <- xtsDF
#  colnames(xts.DF) <- colnames(cc) <- NULL
#  checkIdentical( cc, xts.DF )
#}
#
#test.rbind_data.frame <- function() {
#  rr <- rbind( xtsDF[s1,],
#               xtsDF[s2,],
#               xtsDF[s3,],
#               xtsDF[s4,] )
#  xts.DF <- xtsDF
#  # Remove time-zone attribute of POSIXct index
#  attr(attr(xts.DF, "index"), "tzone") <- NULL
#  checkIdentical( rr, xts.DF )
#}
#
##################################################
## tseries
#test.cbind_ts <- function() {
#  cc <- cbind( xts.ts[,1],
#               xts.ts[,2],
#               xts.ts[,3],
#               xts.ts[,4] )
#  #xtsts <- xts.ts
#  colnames(xts.ts) <- colnames(cc) <- NULL
#  checkIdentical( cc, xts.ts )
#}
#
#test.rbind_ts <- function() {
#  rr <- rbind( xts.ts[s1,],
#               xts.ts[s2,],
#               xts.ts[s3,],
#               xts.ts[s4,] )
#  checkIdentical( rr, xts.ts )
#}
#
##################################################
## irregular time-series
#test.cbind_irts <- function() {
#  cc <- cbind( xtsIR[,1],
#               xtsIR[,2],
#               xtsIR[,3],
#               xtsIR[,4] )
#  xts.IR <- xtsIR
#  colnames(xts.IR) <- colnames(cc) <- NULL
#  checkIdentical( cc, xts.IR )
#}
#
#test.rbind_irts <- function() {
#  rr <- rbind( xtsIR[s1,],
#               xtsIR[s2,],
#               xtsIR[s3,],
#               xtsIR[s4,] )
#  xts.IR <- xtsIR
#  # Remove time-zone attribute of POSIXct index
#  attr(attr(xts.IR, "index"), "tzone") <- NULL
#  checkIdentical( rr, xts.IR )
#}
#
##################################################
## timeSeries
#test.cbind_timeSeries <- function() {
#  cc <- cbind( xts.TS[,1],
#               xts.TS[,2],
#               xts.TS[,3],
#               xts.TS[,4] )
#  xTS <- xts.TS
#  colnames(xTS) <- colnames(cc) <- NULL
#  checkIdentical( cc, xTS )
#}
#
#test.rbind_timeSeries <- function() {
#  rr <- rbind( xts.TS[s1,],
#               xts.TS[s2,],
#               xts.TS[s3,],
#               xts.TS[s4,] )
#  xTS <- xts.TS
#  # Remove time-zone attribute of POSIXct index
#  attr(attr(xTS, "index"), "tzone") <- NULL
#  checkIdentical( rr, xTS )
#}
