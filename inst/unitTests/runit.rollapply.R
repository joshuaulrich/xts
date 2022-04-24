test.rollapply_width_is_list <- function() {
  orig_tz <- Sys.getenv("TZ")
  new_tz <- "UTC"
  Sys.setenv(TZ = new_tz)
  on.exit(Sys.setenv(TZ = orig_tz))

  x <- .xts(1:6, 1:6)
  r1 <- rollapply(x, width = list(-(3:5)), FUN = mean)
  checkTrue(.index(r1) == 6)
  checkTrue(coredata(r1)[1,1] == 2)
  checkIdentical(tclass(r1), c("POSIXct", "POSIXt"))
  checkIdentical(tzone(r1), new_tz)

  y <- xts(1:6, .Date(1:6))
  r2 <- rollapply(y, width = list(-(3:5)), FUN = mean)
  checkTrue(.index(r2) == 86400*6)
  checkTrue(coredata(r2)[1,1] == 2)
  checkIdentical(tclass(r2), "Date")
  checkIdentical(tzone(r2), "UTC")
}
