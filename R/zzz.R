.onLoad <- function(lib, pkg)
{
  if(Sys.getenv("TZ") == "") {
    packageStartupMessage("xts now requires a valid TZ environment variable to be set")
    packageStartupMessage(" no TZ var is set, setting to TZ=GMT")
    Sys.setenv(TZ="GMT")
  } else {
    packageStartupMessage("xts now requires a valid TZ environment variable to be set")
    packageStartupMessage(" your current TZ:",paste(Sys.getenv("TZ")))
  }
}
