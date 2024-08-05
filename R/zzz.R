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


# internal package environment for use with lines.xts
# Do we still need this env?
.xtsEnv <- new.env()

# Environment for our xts chart objects (xts_chob)
.plotxtsEnv <- new.env()

register_s3_method <-
function(pkg, generic, class, fun = NULL)
{
  stopifnot(is.character(pkg), length(pkg) == 1L)
  stopifnot(is.character(generic), length(generic) == 1L)
  stopifnot(is.character(class), length(class) == 1L)

  if (is.null(fun)) {
    fun <- get(paste0(generic, ".", class), envir = parent.frame())
  } else {
    stopifnot(is.function(fun))
  }

  if (isNamespaceLoaded(pkg)) {
    registerS3method(generic, class, fun, envir = asNamespace(pkg))
  }

  # Always register hook in case package is later unloaded & reloaded
  setHook(
    packageEvent(pkg, "onLoad"),
    function(...) {
      registerS3method(generic, class, fun, envir = asNamespace(pkg))
    }
  )
}

.onAttach <- function(libname, pkgname)
{
  warn_dplyr_lag <- getOption("xts.warn_dplyr_breaks_lag", TRUE)

  dplyr_will_mask_lag <- conflictRules("dplyr")
  if (is.null(dplyr_will_mask_lag)) {
    dplyr_will_mask_lag <- TRUE
  } else {
    dplyr_will_mask_lag <- all(dplyr_will_mask_lag$exclude != "lag")
  }

  if (warn_dplyr_lag && dplyr_will_mask_lag) {
      ugly_message <- "
######################### Warning from 'xts' package ##########################
#                                                                             #
# The dplyr lag() function breaks how base R's lag() function is supposed to  #
# work, which breaks lag(my_xts). Calls to lag(my_xts) that you type or       #
# source() into this session won't work correctly.                            #
#                                                                             #
# Use stats::lag() to make sure you're not using dplyr::lag(), or you can add #
# conflictRules('dplyr', exclude = 'lag') to your .Rprofile to stop           #
# dplyr from breaking base R's lag() function.                                #
#                                                                             #
# Code in packages is not affected. It's protected by R's namespace mechanism #
# Set `options(xts.warn_dplyr_breaks_lag = FALSE)` to suppress this warning.  #
#                                                                             #
###############################################################################"
    if ("package:dplyr" %in% search()) {
      packageStartupMessage(ugly_message)
    } else {
      setHook(packageEvent("dplyr", "attach"),
              function(...) packageStartupMessage(ugly_message))
    }
  }
}

.onLoad <- function(libname, pkgname)
{
  if (getRversion() < "3.6.0") {
    register_s3_method("timeSeries", "as.timeSeries", "xts")
    if (utils::packageVersion("zoo") < "1.8.5") {
      # xts:::as.zoo.xts was copied to zoo:::as.zoo.xts in zoo 1.8-5
      register_s3_method("zoo", "as.zoo", "xts")
    }
  }
  invisible()
}

.onUnload <- function(libpath) {
  library.dynam.unload("xts", libpath)
}

if(getRversion() < "2.11.0") {
    .POSIXct <- function(xx, tz = NULL)
    structure(xx, class = c("POSIXct", "POSIXt"), tzone = tz)
}
