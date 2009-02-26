.onLoad <- function(lib, pkg)
{
  if(Sys.getenv("TZ") == "") {
    message("xts now requires a valid TZ variable to be set")
    message(" no TZ var is set, setting to TZ=GMT")
    Sys.setenv(TZ="GMT")
  } else {
    message("xts now requires a valid TZ variable to be set")
    message(" your current TZ:", paste(Sys.getenv("TZ")))
  }
}
