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

.onUnload <- function(libpath) {
  library.dynam.unload("xts", libpath)
}

if(getRversion() < "2.11.0") {
    .POSIXct <- function(xx, tz = NULL)
    structure(xx, class = c("POSIXct", "POSIXt"), tzone = tz)
}
