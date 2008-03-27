#--#
#--# RUnit tests for the following 'xts' methods:
#--# rbind
#--# cbind
#--#
#--
#--# test reclass works and throws error
#--# test xtsAttributes, both CLASS and USER
#--# test all.equal(CLASS) and !all.equal(CLASS) cases
#--
#--# Create datums :)
#--data(sample_matrix)
#--date.index <- as.Date(rownames(sample_matrix))
#--s1 <- 1:45
#--s2 <- 46:90
#--s3 <- 91:135
#--s4 <- 136:180
#--
#--# create 'zoo' object
#--xts.zoo <- as.xts( zoo(sample_matrix, date.index) )
#--# create 'data.frame' object
#--xts.df <- as.xts( data.frame(sample_matrix) )
#--# create 'its' object
#--xts.its <- as.xts( its(sample_matrix) )
#--# create 'irts' object
#--xts.irts <- as.xts( irts(as.POSIXct(date.index), sample_matrix) )
#--# create 'ts' object
#--xts.ts <- as.xts( ts(sample_matrix, start=as.numeric(date.index)[1]) )
#--# create 'timeSeries' object
#--xts.TS <- as.xts( timeSeries(sample_matrix, charvec=date.index) )
#--
#--# Combined subset should equal original object
#--test.cbind.zoo.zoo <- function() {
#--  cc <- cbind( xts.zoo[,1],
#--               xts.zoo[,2],
#--               xts.zoo[,3],
#--               xts.zoo[,4] )
#--  checkIdentical( cc, xts.zoo )
#--}
#--
#--#test.cbind.zoo.oth <- function() {
#--#  cc <- cbind( xts.zoo[,1],
#--#               xts.zoo[,2],
#--#               xts.zoo[,3] )
#--#  checkIdentical( cc, xts.zoo )
#--#}
#--
#--# Combined subset should equal original object
#--test.rbind.zoo.zoo <- function() {
#--  rr <- rbind( xts.zoo[s1,],
#--               xts.zoo[s2,],
#--               xts.zoo[s3,],
#--               xts.zoo[s4,] )
#--  checkIdentical( rr, xts.zoo )
#--}
#--
#--# data.frame, cbind
#--test.cbind.df.df <- function() {
#--  cc <- cbind( xts.df[,1],
#--               xts.df[,2],
#--               xts.df[,3],
#--               xts.df[,4] )
#--  checkIdentical( cc, xts.df )
#--}
#--
#--# data.frame, rbind
#--test.rbind.df.df <- function() {
#--  rr <- rbind( xts.df[s1,],
#--               xts.df[s2,],
#--               xts.df[s3,],
#--               xts.df[s4,] )
#--  checkIdentical( rr, xts.df )
#--}
#--
#--# tseries, cbind
#--test.cbind.ts <- function() {
#--  cc <- cbind( xts.ts[,1],
#--               xts.ts[,2],
#--               xts.ts[,3],
#--               xts.ts[,4] )
#--  checkIdentical( cc, xts.ts )
#--}
#--
#--# tseries, rbind
#--test.rbind.ts <- function() {
#--  rr <- rbind( xts.ts[s1,],
#--               xts.ts[s2,],
#--               xts.ts[s3,],
#--               xts.ts[s4,] )
#--  checkIdentical( rr, xts.ts )
#--}
#--
#--# its, cbind
#--test.cbind.its <- function() {
#--  cc <- cbind( xts.its[,1],
#--               xts.its[,2],
#--               xts.its[,3],
#--               xts.its[,4] )
#--  checkIdentical( cc, xts.its )
#--}
#--
#--# its, rbind
#--test.rbind.its <- function() {
#--  rr <- rbind( xts.its[s1,],
#--               xts.its[s2,],
#--               xts.its[s3,],
#--               xts.its[s4,] )
#--  checkIdentical( rr, xts.its )
#--}
#--
#--# irregular time-series, cbind
#--test.cbind.irts <- function() {
#--  cc <- cbind( xts.irts[,1],
#--               xts.irts[,2],
#--               xts.irts[,3],
#--               xts.irts[,4] )
#--  checkIdentical( cc, xts.irts )
#--}
#--
#--# irregular time-series, rbind
#--test.rbind.irts <- function() {
#--  rr <- rbind( xts.irts[s1,],
#--               xts.irts[s2,],
#--               xts.irts[s3,],
#--               xts.irts[s4,] )
#--  checkIdentical( rr, xts.irts )
#--}
#--
#--# timeSeries, cbind
#--test.cbind.TS <- function() {
#--  cc <- cbind( xts.TS[,1],
#--               xts.TS[,2],
#--               xts.TS[,3],
#--               xts.TS[,4] )
#--  checkIdentical( cc, xts.TS )
#--}
#--
#--# timeSeries, cbind
#--test.rbind.TS <- function() {
#--  rr <- rbind( xts.TS[s1,],
#--               xts.TS[s2,],
#--               xts.TS[s3,],
#--               xts.TS[s4,] )
#--  checkIdentical( rr, xts.TS )
#--}
