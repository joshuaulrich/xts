\name{xtsInternals}
\alias{.dimnames.xts}
\alias{dimnames.xts<-}
\title{ Internal Documentation }
\description{
This help file is to help in development of xts, as well
as provide some clarity and insight into its purpose and implementation.

Last modified: 2008-08-06 by Jeffrey A. Ryan
Version: 0.5-0 and above

The \pkg{xts} package xts designed as a drop-in replacement
for the very popular \pkg{zoo} package.  Most all functionality
of zoo has been extended or carries into the xts package.

Notable changes in direction include the use of time-based
indexing, at first explicitely, now implicitely.

An \code{xts} object consists of data in the form of a matrix,
an index - ordered and increasing, either numeric or integer, and additional
attributes for use internally, or for end-user purposes.

The current implementation enforces two major rules on
the object.  One is that the index must be coercible to numeric, by
way of \code{as.POSIXct}.  There are defined types that meet this criteria.
See \code{timeBased} for details.

The second requirement is that the object cannot have rownames.
The motivation from this comes in part from the work
Matthew Doyle has done in his data.table class, in the package of the
same name.  Rownames in \R must be character vectors, and as such
are inefficient in both storage and conversion.  By eliminating
the rownames, and providing a numeric index of \R internal type \code{REAL}
or \code{INTEGER}, it is possible to maintain a connection
to standard \R date and time classes via the POSIXct functions, while at
at the same time maximizing efficiencies in data handling.

User level functions \code{index}, as well as conversion to
other classes proceeds as if there were rownames.  The code
for \code{index} automatically converts time to numeric in both extraction
and replacement functionality. This provides a level of
abstraction to facilitate internal, and external package use and inter-operability.

There is also new work on providing a C-level API to some of the
xts functionality to facilitate external package developers to
utilize the fast utility routines such as subsetting and merges, without
having to call only from \R.  Obviously this places far
more burden on the developer to not only understand the
internal xts implementation, but also to understand all of
what is documented for R-internals (and much that isn't).

There is no current documentation for this API.  The adventure
starts here.  Future documentation is planned, not implemented.

}
\author{ Jeffrey A. Ryan }
\keyword{ utilities }
