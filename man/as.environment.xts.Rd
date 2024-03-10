\name{as.environment.xts}
\alias{as.environment.xts}
\title{ Coerce an \sQuote{xts} Object to an Environment by Column }
\description{
Method to automatically convert an \sQuote{xts} object to an environment
containing vectors representing each column of the original xts object. Each
objects will be named according to the column name it is exracted by.
}
\usage{
\method{as.environment}{xts}(x)
}
\arguments{
  \item{x}{ an \code{xts} object }
}
\details{
An experimental tool to convert \code{xts} objects into environments
for simplifying use withing the standard R formula/data paradigm.
}
\value{
An \code{environment} containing \code{ncol(x)} vectors
extracted by column from \code{x}.  Note that environments
do not preserve (or have knowledge) of column position, a.k.a order.
}
\author{ Jeffrey A. Ryan }
\examples{
x <- xts(1:10, Sys.Date()+1:10)
colnames(x) <- "X"
y <- xts(1:10, Sys.Date()+1:10)
colnames(x) <- "Y"
xy <- cbind(x,y)
colnames(xy)
e <- as.environment(xy)    # currently using xts-style positive k 
ls(xy)
ls.str(xy)
}
\keyword{ manip }% __ONLY ONE__ keyword per line
