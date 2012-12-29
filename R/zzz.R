# internal package environment for use with lines.xts
.xtsEnv <- new.env()

.onLoad <- function(libname, pkgname)
{
#  if(Sys.getenv("TZ") == "") {
#    packageStartupMessage("xts now requires a valid TZ environment variable to be set")
#    packageStartupMessage(" no TZ var is set, setting to TZ=GMT")
#    Sys.setenv(TZ="GMT")
#  } else {
#    packageStartupMessage("xts now requires a valid TZ environment variable to be set")
#    packageStartupMessage(" your current TZ:",paste(Sys.getenv("TZ")))
#  }
}

if(getRversion() < "2.11.0") {
    .POSIXct <- function(xx, tz = NULL)
    structure(xx, class = c("POSIXt", "POSIXct"), tzone = tz)
}
