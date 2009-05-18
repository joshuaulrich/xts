.onLoad <- function(lib, pkg)
{
  if(Sys.getenv("TZ") == "") {
    cat("xts now requires a valid TZ variable to be set\n")
    cat(" no TZ var is set, setting to TZ=GMT\n")
    Sys.setenv(TZ="GMT")
  } else {
    cat("xts now requires a valid TZ variable to be set\n")
    cat(" your current TZ:",paste(Sys.getenv("TZ")),"\n")
  }
}
