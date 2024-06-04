### About

xts is an [R](https://www.r-project.org) package that provides an extension of
the [zoo](https://CRAN.R-project.org/package=zoo) class.  zoo's strength comes
from its simplicity of use (it's very similar to base R functions), and its
overall flexibility (you can use *anything* as an index).  The xts extension
was motivated by the ability to improve performance by imposing reasonable
constraints, while providing a truly time-based structure.

### xts for enterprise

Available as part of the Tidelift Subscription.

The maintainers of `xts` and thousands of other packages are working with Tidelift to deliver commercial support and maintenance for the open source dependencies you use to build your applications. Save time, reduce risk, and improve code health, while paying the maintainers of the exact dependencies you use. [Learn more.](https://tidelift.com/?utm_source=cran-xts&utm_medium=referral&utm_campaign=enterprise&utm_term=repo)

### Supporting xts development

If you are interested in supporting the ongoing development and maintenance of xts, please consider [becoming a sponsor](https://github.com/sponsors/joshuaulrich).

### Installation

The current release is available on [CRAN](https://CRAN.R-project.org/package=xts),
which you can install via:

```r
install.packages("xts")
```

To install the development version, you need to clone the repository and build
from source, or run one of:

```r
# lightweight
remotes::install_github("joshuaulrich/xts")
# or
devtools::install_github("joshuaulrich/xts")
```

You will need tools to compile C, C++, and Fortran code. See the relevant
appendix in the [R Installation and Administration manual](https://cran.r-project.org/doc/manuals/r-release/R-admin.html)
for your operating system:

- [Windows](https://cran.r-project.org/doc/manuals/r-release/R-admin.html#The-Windows-toolset)
- [MacOS](https://cran.r-project.org/doc/manuals/r-release/R-admin.html#macOS) (the [R for Mac OS X Developer's Page](https://mac.r-project.org/) might also be helpful)
- [Unix-alike](https://cran.r-project.org/doc/manuals/r-release/R-admin.html#Essential-and-useful-other-programs-under-a-Unix_002dalike)

### Getting Started

You can create xts objects using `xts()` and `as.xts()`.

Note that `as.xts()` currently expects the date/times to be in the row names
for matrix and data.frame objects, or in the names for vector. You can also
use the `dateFormat` argument to control whether the names should be converted
to `Date` or `POSIXct`. See `help(as.xts.methods)` for details.

```r
n <- 10
series <- rnorm(n)

# POSIXct (date/time) index
datetimes <- seq(as.POSIXct("2017-03-27"), length.out = n, by = "days")
library(xts)
x <- xts(series, datetimes)
```

In addition to the usual ways you can subset matrix and zoo objects, you can
also subset xts objects using character strings that adhere to the
[ISO-8601 standard](https://en.wikipedia.org/wiki/ISO_8601), which is the
internationally recognized and accepted way to represent dates and times.
Using the data from the prior code block, here are some examples:

```r
# March, 2017
x["2017-03"]
#                   [,1]
# 2017-03-27  0.25155453
# 2017-03-28 -0.09379529
# 2017-03-29  0.44600926
# 2017-03-30  0.18095782
# 2017-03-31 -1.45539421

# March 30th through April 2nd
x["2017-03-30/2017-04-02"]
#                  [,1]
# 2017-03-30  0.1809578
# 2017-03-31 -1.4553942
# 2017-04-01 -0.4012951
# 2017-04-02 -0.5331497

# Beginning of the series to April 1st
x["/2017-04-01"]
#                   [,1]
# 2017-03-27  0.25155453
# 2017-03-28 -0.09379529
# 2017-03-29  0.44600926
# 2017-03-30  0.18095782
# 2017-03-31 -1.45539421
# 2017-04-01 -0.40129513
```

You can aggregate a univariate series, or open-high-low-close (OHLC) data, into
a lower frequency OHLC series with the `to.period()` function. There are also
convenience functions for some frequencies (e.g. `to.minutes()`, `to.daily()`,
`to.yearly()`, etc).

```r
data(sample_matrix)
x <- as.xts(sample_matrix)
to.period(x, "months")
#              x.Open   x.High    x.Low  x.Close
# 2007-01-31 50.03978 50.77336 49.76308 50.22578
# 2007-02-28 50.22448 51.32342 50.19101 50.77091
# 2007-03-31 50.81620 50.81620 48.23648 48.97490
# 2007-04-30 48.94407 50.33781 48.80962 49.33974
# 2007-05-31 49.34572 49.69097 47.51796 47.73780
# 2007-06-30 47.74432 47.94127 47.09144 47.76719

to.monthly(x)  # result has a 'yearmon' index
#           x.Open   x.High    x.Low  x.Close
# Jan 2007 50.03978 50.77336 49.76308 50.22578
# Feb 2007 50.22448 51.32342 50.19101 50.77091
# Mar 2007 50.81620 50.81620 48.23648 48.97490
# Apr 2007 48.94407 50.33781 48.80962 49.33974
# May 2007 49.34572 49.69097 47.51796 47.73780
# Jun 2007 47.74432 47.94127 47.09144 47.76719
```

The `period.apply()` function allows you apply a custom function to non-
overlapping intervals. You specify the intervals using a vector similar to the
output of `endpoints()`. Like `to.period()` there are convenience functions,
like `apply.daily()`, `apply.quarterly()`, etc.

```r
# Average monthly value for each column
period.apply(x, endpoints(x, "months"), colMeans)
#                Open     High      Low    Close
# 2007-01-31 50.21140 50.31528 50.12072 50.22791
# 2007-02-28 50.78427 50.88091 50.69639 50.79533
# 2007-03-31 49.53185 49.61232 49.40435 49.48246
# 2007-04-30 49.62687 49.71287 49.53189 49.62978
# 2007-05-31 48.31942 48.41694 48.18960 48.26699
# 2007-06-30 47.47717 47.57592 47.38255 47.46899

#                Open     High      Low    Close
# 2007-01-31 50.21140 50.31528 50.12072 50.22791
# 2007-02-28 50.78427 50.88091 50.69639 50.79533
# 2007-03-31 49.53185 49.61232 49.40435 49.48246
# 2007-04-30 49.62687 49.71287 49.53189 49.62978
# 2007-05-31 48.31942 48.41694 48.18960 48.26699
# 2007-06-30 47.47717 47.57592 47.38255 47.46899
```

###### Have a question?

Ask your question on [Stack Overflow](https://stackoverflow.com/questions/tagged/r)
or the [R-SIG-Finance](https://stat.ethz.ch/mailman/listinfo/r-sig-finance)
mailing list (you must subscribe to post).

###### Want hands-on experience?

- [DataCamp course on importing and managing financial data](https://www.datacamp.com/courses/importing-and-managing-financial-data-in-r)
- [DataCamp course on manipulating time series with xts & zoo](https://www.datacamp.com/courses/manipulating-time-series-data-in-r)

### Contributing

Please see the [Contributing Guide](https://github.com/joshuaulrich/xts/wiki/Contributing-Guide).

### See Also

- [quantmod](https://CRAN.R-project.org/package=quantmod): quantitative financial modeling framework
- [TTR](https://CRAN.R-project.org/package=TTR): functions for technical trading
rules
- [zoo](https://CRAN.R-project.org/package=zoo): class for regular and irregular time series

### Author

Jeffrey Ryan, [Joshua Ulrich](https://about.me/joshuaulrich)

