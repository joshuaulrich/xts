# xts 0.14.0 (2024-06-05)

* `addEventLines()` and `addLegend()` now draw on multiple panels when `on` is
  a vector. Thanks to Ethan Smith for the report.
  ([#420](https://github.com/joshuaulrich/xts/issues/420))

* Replace `SET_TYPEOF()` in merge.c because it will error when it tries to
  convert a REAL to an INTEGER. Thanks to Kurt Hornik for the report!
  ([#419](https://github.com/joshuaulrich/xts/issues/419))

* Fix crash when 'j' is not an integer and in [0, 1) (e.g. `j = 0.1`). Also
  throw a warning when 'j' is not an integer.
  ([#413](https://github.com/joshuaulrich/xts/issues/413))
  ([#415](https://github.com/joshuaulrich/xts/issues/415))

* Fix plot header when `layout()` is used to draw multiple plots on a single
  device. Thanks to Dirk Eddelbuettel for the report and testing!
  ([#412](https://github.com/joshuaulrich/xts/issues/412))

* Fix plot legend location when the y-axis is log scale.
  ([#407](https://github.com/joshuaulrich/xts/issues/407))

# xts 0.13.2 (2024-01-21)

* Print a message when `period.apply()` is called with `FUN = mean` because it
  calculates the mean for each column, not all the data in the subset like it
  does for all other functions. The message says to use `FUN = colMeans` for
  current behavior and `FUN = function(x) mean(x)` to calculate the mean for
  all the data. This information is also included in the help files. The option
  `xts.message.period.apply.mean = FALSE` suppresses the message.
  ([#124](https://github.com/joshuaulrich/xts/issues/124))

* Fix error when `print.xts()` is called 'quote' or 'right' arguments.
  ([#401](https://github.com/joshuaulrich/xts/issues/401))

* Fix `addPolygon()` so it renders when `observation.based = TRUE`.
  ([#403](https://github.com/joshuaulrich/xts/issues/403))

* Print trailing zeros for index value with fractional seconds, so every index
  value has the same number of characters.
  ([#404](https://github.com/joshuaulrich/xts/issues/404))

* Add ability to log scale the y-axis in `plot.xts()`.
  ([#103](https://github.com/joshuaulrich/xts/issues/103))

* Actually change the underlying index values when 'tclass' is changed from a
  class with a timezone (e.g. POSIXct) to one without a timezone (e.g. Date).
  Add a warning when this happens, with a global option to always suppress the
  warning.
  ([#311](https://github.com/joshuaulrich/xts/issues/311)).

* Significantly refactor the internals of `plot.xts()`.
  ([#408](https://github.com/joshuaulrich/xts/issues/408))

# xts 0.13.1 (2023-04-16)

* Ignore attribute order in `all.equal()`. Attribute order shouldn't matter.
  That can be checked with `identical()`.

* Only call `tzone()` and `tclass()` once in `check.TZ()`. Calling these
  functions multiple times throws multiple warnings for xts objects created
  before the tclass and tzone were attached to the index instead of the xts
  object.
  ([#306](https://github.com/joshuaulrich/xts/issues/306))

* Add instructions to update old objects. Old xts objects do not have tclass
  and tzone attributes on the index. Add a function to update the object
  attributes and add a note to the warning to show how to use it.
  ([#306](https://github.com/joshuaulrich/xts/issues/306))

* Return 'POSIXct' if object has no 'tclass'. An empty string is not a valid
  'tclass', so it can cause an error.

* Add notes on `plot.xts()` nomenclature and structure. Also add ASCII art to
  illustrate definitions and layout.
  ([#103](https://github.com/joshuaulrich/xts/issues/103))

* Remove 'tis' support. The implementation was not even a bare minimum, and
  it's not clear it even worked correctly.
  ([#398](https://github.com/joshuaulrich/xts/issues/398))

* Register missing S3 methods and update signatures. With R-devel (83995-ish),
  `R CMD check` notes these S3 methods are not registered. It also notes that
  the signatures for `as.POSIXct.tis()` and `str.replot_xts()` do not match
  the respective generics.

  It also thinks `time.frequency()` is a S3 method because `time()` is a
  generic. The function isn't exported, so renaming won't break any external
  code. Thanks to Kurt Hornik for the report.
  ([#398](https://github.com/joshuaulrich/xts/issues/398))

* Format each column individually before printing. The top/bottom rows could
  have a different number of decimal places and there are often multiple
  variying spaces between columns. For example:

                                close      volume          ma         bsi
      2022-01-03 09:31:00     476.470  803961.000          NA   54191.000
      2022-01-03 09:32:00     476.700  179476.000          NA   53444.791
      2022-01-03 09:33:00     476.540  197919.000          NA  -16334.994
                      ...
      2023-03-16 14:52:00    394.6000  46728.0000    392.8636  28319.4691
      2023-03-16 14:53:00    394.6500  64648.0000    392.8755  15137.6857
      2023-03-16 14:54:00    394.6500  69900.0000    392.8873  -1167.9368

  There are 4 spaces between the index and the 'close' column, 2 between
  'close' and 'volume', 4 between 'volume' and 'ma', and 2 between 'ma' and
  'bsi'. There should be a consistent number of spaces between the columns. Most
  other classes of objects print with 1 space between the columns.

  The top rows have 3 decimals and the bottom rows have 4. These should also be
  the same.
  ([#321](https://github.com/joshuaulrich/xts/issues/321))

* Only convert printed index values to character. Converting the entire index
  to character is time-consuming for xts objects with many observations. It can
  take more than a second to print an xts object with 1mm observations.

* Make column names based on number of columns. The original code was a lot
  more complicated because it tried to account for truncating the number of
  printed columns. That functionality was removed because of how complicated
  it was. So now we can simply create printed column names from the number of
  columns.
  ([#395](https://github.com/joshuaulrich/xts/issues/395))

* Fix `xts()` for zero-row data.frame. The `xts()` constructor would create an
  object with a list for coredata when 'x' is a data.frame with no rows. It
  needs to convert 'x' to a matrix and throw an error if 'x' is a list.
  ([#394](https://github.com/joshuaulrich/xts/issues/394))

* Reduce instances when `dplyr::lag()` warning is shown. The warning was shown
  whenever it detected dplyr is installed, even if the user wasn't actively
  using dplyr. That caused an excessive amount of noise when other packages
  attached xts (e.g. quantmod). Thanks to Duncan Murdoch for the report and
  suggested fix!
  ([#393](https://github.com/joshuaulrich/xts/issues/393))

* Keep colname when only one non-time-based column. The subset `x[, -which.col]`
  would return a vector when the data frame has a time-based column and only
  one additional column. Do not drop dimensions, so 'x' will still be a
  data.frame in this case.
  ([#391](https://github.com/joshuaulrich/xts/issues/391))

* Treat NA the same as NULL for start or end values. NULL represents an
  undefined index value. NA represents an unknown or missing index value. xts
  does not allow NA as index values. Subsetting an xts or zoo object by NA
  returns a zero-length object. So a NA (unknown) index value is essentially
  the same as an undefined index value.
  ([#345](https://github.com/joshuaulrich/xts/issues/345),
  [#383](https://github.com/joshuaulrich/xts/pull/383))

* Warn and remove NA when `periodicity()` called on date-time with NA.
  Otherwise the uninformative error below will be thrown.
  ([#289](https://github.com/joshuaulrich/xts/issues/289))

      Error in try.xts(x, error = "'x' needs to be timeBased or xtsible") :
        'x' needs to be timeBased or xtsible

* Account for TZ when making names for `split.xts()`. `as.yearmon.POSIXct()`
  always sets `tz = "GMT"` when calling `as.POSIXlt()`, regardless of the xts'
  index tzone.  That can cause the `as.yearmon()` results to be different days
  for GMT and the index's timezone.

  Use `format.POSIXct()` for "months" because it checks for a 'tzone' attribute
  before converting to POSIXlt and calling `format.POSIXlt()`.  The conversion
  to POSIXlt is important because it checks and uses the 'tzone' attribute
  before considering the 'tz' argument. So it effectively ignores the
  `tz = "GMT"` setting in `as.yearmon()`. This is also the reason for calling
  `as.POSIXlt()` before calling `as.yearqtr()`.
  ([#392](https://github.com/joshuaulrich/xts/issues/392))

# xts 0.13.0 (2023-02-21)

## New Features

* Added a xts method for `na.fill()` to significantly increase performance when
  'fill' is a scalar.
  ([#259](https://github.com/joshuaulrich/xts/issues/259))

* `as.xts()` will look for a time-based column in a data.frame if it cannot
  create an index from the row names.
  ([#381](https://github.com/joshuaulrich/xts/issues/381))

* Change `print()` xts method to only show the first and last 'show.rows' rows
  if number of rows is > 'max.rows'.
  ([#321](https://github.com/joshuaulrich/xts/issues/321))

* Made `str()` output more descriptive for xts objects. It now differentiates
  between xts objects that are empty, zero-width, or zero-length, and defines
  each type of object. It also adds column names to the output.
  ([#168](https://github.com/joshuaulrich/xts/issues/168),
  [#378](https://github.com/joshuaulrich/xts/issues/378))

* Add startup warning that `dplyr::lag()` breaks method dispatch, which means
  calls to `lag(my_xts)` won't work any more.

* Added open-ended time of day subsetting ranges. This allows users to subset
  by time of day from the start/end of the day without providing the start/end
  times (00:00:00.000/23:59:59.999).

  For example:
      x["/T1800"]  # between the start of the day and 5pm
      x["T0500/"]  # between 5am and the end of the day

  Thanks to Chris Katsulis for the suggestion!
  ([#243](https://github.com/joshuaulrich/xts/issues/243))

* Updated `to.period()` to accept custom 'endpoints' via the 'period' argument.
  Now you can aggregate on something other than the times that 'endpoints()'
  supports. Thanks to Ethan B. Smith for the suggestion!
  ([#302](https://github.com/joshuaulrich/xts/issues/302))

## Fixes

* Fixed typo and expand `period.apply()` documentation. ([#205](https://github.com/joshuaulrich/xts/issues/205))
    The original description has:
      * "the data from INDEX[k] to INDEX[k+1]"
    But that's not consistent with the code. It should be:
      * "the data from INDEX[k]+1 to INDEX[k+1]"

* Calls to `merge.xts()` on zero-width objects now match `merge.zoo()`.
  Previously, `merge.xts()` would return empty xts objects if called on two or
  more zero-width xts objects. `merge.zoo()` would return a zero-width object
  with the correct index.
  ([#227](https://github.com/joshuaulrich/xts/issues/227),
  [#379](https://github.com/joshuaulrich/xts/issues/379))

* Fixed `Ops.xts()` so it always returned an object with the same class as the
  first (left-hand side) argument. It previously returned an xts object even
  if the first argument was a subclass of xts.
  ([#49](https://github.com/joshuaulrich/xts/issues/49))

## Other

* Migrated unit tests from RUnit to tinytest. Thanks Mark van der Loo!

* Updated the `endpoints()` documentation to make it clearer that the result
  is based on the UNIX epoch (midnight 1970, UTC). Thanks to GitHub user
  Eluvias for the suggestion!
  ([#299](https://github.com/joshuaulrich/xts/issues/299))

* Fixed `reclass()` to ensure it always adds index attributes from the
  'match.to' argument. It was not copying `tclass`, `tzone`, or `tformat` from
  'match.to' to the result object.
  ([#43](https://github.com/joshuaulrich/xts/issues/43))

* Removed an unnecessary check in `na.locf()` (which is not user-facing).
  Thanks to GitHub user @cgiachalis for the suggestion!
  ([#307](https://github.com/joshuaulrich/xts/issues/307))

* Updated C entry points so they're not able to accidentally be found via
  dynamic lookup (i.e. `.Call("foo", ...)`). This makes each call to the C
  code a few microseconds faster, which is nice.
  ([#260](https://github.com/joshuaulrich/xts/issues/260))

* Made `merge.xts()` results consistent with `merge.zoo()` for zero-length xts
  objects with columns. The result of `merge.xts()` did not include the
  columns of any objects that had one or more columns, but zero rows. A join
  should include all the columns of the joined objects, regardless of the
  number of rows in the object. This is consistent with `merge.zoo()`. Thanks
  to Ethan B. Smith for the report and testing!
  ([#222](https://github.com/joshuaulrich/xts/issues/222))

# xts 0.12.2 (2022-10-16)

* `Ops.xts()` no longer changes column names (via `make.names()`) when the two
  objects do not have identical indexes. This makes it consistent with
  `Ops.zoo()`.
  ([#114](https://github.com/joshuaulrich/xts/issues/114))

* Subsetting a zero-length xts object now returns an object with the same
  storage type as the input. It previously always returned a 'logical' xts
  object.
  ([#376](https://github.com/joshuaulrich/xts/issues/376))

* `tclass()` and `tzone()` now return the correct values for zero-length xts
  objects, instead of the defaults in the `.xts()` constructor. Thanks to Andre
  Mikulec for the report and suggested patch!
  ([#255](https://github.com/joshuaulrich/xts/issues/255))

* `first()` and `last()` now return a zero-length xts object when `n = 0`. They
  previously returned the entire object. This is consistent with the default
  `head()` and `tail()` functions, and data.table's `first()` and `last()`
  functions. Thanks to Ethan B. Smith for the report and patch!
  ([#350](https://github.com/joshuaulrich/xts/issues/350))

* `plot.xts()` now has a `yaxis.ticks` argument to control the number of y-axis
  grid lines, instead of always drawing 5 grid lines. Thanks to Fredrik
  Wartenberg for the feature request and patch!
  ([#374](https://github.com/joshuaulrich/xts/issues/374))

* Subsetting a zero-width xts now returns an object with the same class, tclass,
  tzone, and xtsAttributes as the input. Thanks to @shikokuchuo for the report!
  ([#359](https://github.com/joshuaulrich/xts/issues/359))

* `endpoints()` now always returns last observation. Thanks to GitHub user
  Eluvias for the report.
  ([#300](https://github.com/joshuaulrich/xts/issues/300))

* Ensure `endpoints()` errors for every 'on' value when `k < 1`. It was not
  throwing an error for `k < 1` for `on` of "years", "quarters", or "months".
  Thanks to Eluvias for the report.
  ([#301](https://github.com/joshuaulrich/xts/issues/301))

* Fix `window()` for yearmon and yearqtr indexes. In xts < 0.11-0, `window.zoo()`
  was dispatched when `window()` was called on a xts object because there was no
  `window.xts()` method. `window.zoo()` supports additional types of values for the
  `start` argument, and possibly other features. So this fixes a breaking change
  in xts >= 0.11-0. Thanks to @annaymj for the report.
  ([#312](https://github.com/joshuaulrich/xts/issues/312))

* Clarify whether `axTicksByTime()` returns index timestamps or locations (e.g.
  1, 2, 3). Thanks to @ggrothendieck for the suggestion and feedback.
  ([#354](https://github.com/joshuaulrich/xts/issues/354))

* Fix merge on complex types when 'fill' is needed. `merge()` would throw an
  error because it treated 'fill' as double instead of complex. Thanks to
  @ggrothendieck for the report.
  ([#346](https://github.com/joshuaulrich/xts/issues/346))

* Add a message to tell the user how to disable 'xts_check_TZ' warning. Thanks
  to Jerzy Pawlowski for the nudge.
  ([#113](https://github.com/joshuaulrich/xts/issues/113))

* Update `rbind()` to handle xts objects without dim attribute. `rbind()` threw
  an obscure error if one of the xts objects does not have a dim attribute. We
  can handle this case even though all xts objects should always have a dim
  attribute.
  ([#361](https://github.com/joshuaulrich/xts/issues/361))

* `split.xts()` now always return a named list, which makes it consistent with
  `split.zoo()`. Thanks to Gabor Grothendieck for the report.
  ([#357](https://github.com/joshuaulrich/xts/issues/357))

* xts objects with a zero-length POSIXct index now return a zero-length POSIXct
  vector instead of a zero-length integer vector. Thanks to Jasper Schelfhout
  for the report and PR!
  ([#363](https://github.com/joshuaulrich/xts/issues/363),
  [#364](https://github.com/joshuaulrich/xts/pull/364))

* Add suffixes to output of `merge.xts()`. The suffixes are consistent with
  `merge.default()` and not `merge.zoo()`, because `merge.zoo()` automatically
  uses "." as a separator between column names, but the default method doesn't.
  Thanks to Pierre Lamarche for the nudge. Better late than never?
  ([#38](https://github.com/joshuaulrich/xts/issues/38),
  [#371](https://github.com/joshuaulrich/xts/issues/371))

## Changes to plotting functionality

* You can now omit the data time range from the upper-right portion of a plot
  by setting `main.timespan = FALSE`.
  ([#247](https://github.com/joshuaulrich/xts/issues/247))

* Fix `addEventLines()` when plotted objects have a 'yearmon' index. The ISO-8601
  range string was not created correctly. Thanks to @paessens for the report.
  ([#353](https://github.com/joshuaulrich/xts/issues/353))

* Make 'ylim' robust against numerical precision issues by replacing `==` with
  `all.equal()`. Thanks to @bollard for the report, PR, and a ton of help
  debugging intermediate solutions!
  ([#368](https://github.com/joshuaulrich/xts/issues/368))

* Series added to a panel now extend the panel's y-axis. Previously the y-axis
  limits were based on the first series' values and not updated when new series
  were added. So values of the new series did not appear on the plot if they
  were outside of the original series' min/max. Thanks to Vitalie Spinu for the
  report and help debugging and testing!
  ([#360](https://github.com/joshuaulrich/xts/issues/360))

* All series added to any panel of a plot now update the x-axis of all panels.
  So the entire plot's x-axis will include every series' time index values
  within the original plot's time range. This behavior is consistent with
  `chart_Series()`. Thanks to Vitalie Spinu for the report and help debugging
  and testing!
  ([#360](https://github.com/joshuaulrich/xts/issues/360),
  [#216](https://github.com/joshuaulrich/xts/issues/216))

* All y-values are now plotted for series that have duplicate index values, but
  different data values. Thanks to Vitalie Spinu for the report and help
  debugging and testing!
  ([#360](https://github.com/joshuaulrich/xts/issues/360))

* Adding a series can now extend the x-axis before/after the plot's existing
  time index range, so all of the new series' time index values are included in
  the plot. This is FALSE by default to maintain backward compatibility. Thanks
  to Vitalie Spinu for the report and help debugging and testing!
  ([#360](https://github.com/joshuaulrich/xts/issues/360))

# xts 0.12.1 (2020-09-09)

*  Various function could change the tclass of xts objects. This would happen
   in calls to reclass(), period.apply(), and for logical operations on
   POSIXct indexes. Thanks to Tom Andrews for the report and testing, and to
   Panagiotis Cheilaris for contributing test cases.
   ([#322](https://github.com/joshuaulrich/xts/issues/322),
   [#323](https://github.com/joshuaulrich/xts/pull/323))

*  plot.xts() now supports y-axis labels via 'ylab'. Thanks to Jasen Mackie
   for the suggestion and PR.
   ([#333](https://github.com/joshuaulrich/xts/issues/333),
   [#334](https://github.com/joshuaulrich/xts/pull/334))

*  The API header has been updated to fix the signatures of do_merge_xts() and
   is_xts, which did not return a SEXP as required of functions callable by
   .Call(). Thanks to Tomas Kalibera for the report ([#317](https://github.com/joshuaulrich/xts/issues/317)),
   and Dirk Eddelbuettel for the PR ([#337](https://github.com/joshuaulrich/xts/pull/337)).
   This is a breaking change, but is required to avoid the potential for a segfault.

*  Michael Chirico added an internal isUTC() function to recognize many UTC-
   equivalent time zones.
   ([#319](https://github.com/joshuaulrich/xts/issues/319))

*  first() now operates correctly on non-xts objects when 'n = -1'. Previously
   it would always return the last two values. Thanks to GitHub user vxg20
   for the report.
   ([#325](https://github.com/joshuaulrich/xts/issues/325))

*  The .xts() constructor would create an xts object with row names if 'x' had
   row names. This shouldn't happen, because xts objects do not have or
   support row names.
   ([#298](https://github.com/joshuaulrich/xts/issues/298))

*  Claymore Marshall added many examples of time-of-day subsetting to
   ?subset.xts. He also fixed a bug in time-of-day subsetting where subsetting
   by hour only returned wrong results.
   ([#304](https://github.com/joshuaulrich/xts/issues/304),
   [#326](https://github.com/joshuaulrich/xts/issues/326),
   [#328](https://github.com/joshuaulrich/xts/pull/328))

# xts 0.12-0 (2020-01-19)

*  All the index-attributes have been removed from the xts object and are now
   only attached to the index itself. We took great care to maintain backward
   compatibility, and throw warnings when deprecated functions are called and
   when index-attributes are found on the xts object. But there still may be
   some breaking changes lurking in edge cases.
   ([#245](https://github.com/joshuaulrich/xts/issues/245))

   * @SamoPP found one edge case  where an error was thrown when index()
     was called on an xts object with an index that had no tclass attribute.
     ([#297](https://github.com/joshuaulrich/xts/issues/297))
   * ...which led Joshua to find that the index setting functions did not
     always copy index attributes.
     ([#305](https://github.com/joshuaulrich/xts/issues/305))

*  Several binary operations (e.g. +, -, !=, <, etc.) on variations of
   uncommon xts objects with other xts, matrix, or vector objects, could
   result in malformed xts objects.  Some examples of the types of uncommon
   xts objects: no dim attribute, zero-width, zero-length.
   ([#295](https://github.com/joshuaulrich/xts/issues/295))

*  Calling as.matrix() on an xts object without a dim attribute no longer
   throws an error. ([#294](https://github.com/joshuaulrich/xts/issues/294))

*  merge.xts() now honors check.names = FALSE.
   ([#293](https://github.com/joshuaulrich/xts/issues/293))

*  The possible values for major.ticks, minor.ticks, and grid.ticks.on in the
   Details section of ?plot.xts have been corrected. Thanks to Harvey Smith
   (@harvey131) for the report and patch.
   ([#291](https://github.com/joshuaulrich/xts/issues/291))

*  as.zoo.xts() is now only registered for zoo versions prior to 1.8-5. Methods
   to convert an object to another class should reside in the package that
   implements the target class. Thanks to Kurt Hornik for the report.
   ([#287](https://github.com/joshuaulrich/xts/issues/287))

*  .parseISO8601() no longer has a potential length-1 logical error. Thanks to
   Kurt Hornik for the report.
   ([#280](https://github.com/joshuaulrich/xts/issues/280))

*  endpoints() now honors k > 0 when on = "quarters". Thanks to @alkment for
   the report.
   ([#279](https://github.com/joshuaulrich/xts/issues/279))

*  Performance for the period.XYZ() functions (sum, prod, min, max) is much
   faster. Thanks to Chris Katsulis for the report, and Harvey Smith
   (@harvey131) for several examples.
   ([#278](https://github.com/joshuaulrich/xts/issues/278))

*  merge.xts() now creates shorter column names when passed unnamed objects.
   This is now consistent with zoo.
   ([#248](https://github.com/joshuaulrich/xts/issues/248))

*  Time-of-day performance is ~200x faster, thanks to StackOverflow
   user3226167 ([#193](https://github.com/joshuaulrich/xts/issues/193)).

# xts 0.11-2 (2018-11-05)

*  The to.period() family of functions now use the index timezone when
   converting intraday index values to daily values (or lower frequency).
   Thanks to Garrett See and Gabor Grothendieck for the reports.
   ([#53](https://github.com/joshuaulrich/xts/issues/53),
   [#277](https://github.com/joshuaulrich/xts/issues/277))

*  Make column names for merge() results with unnamed objects shorter and more
   like zoo. This also makes na.fill() much faster. NOTE: This may break
   existing code for integer unnamed objects.
   ([#248](https://github.com/joshuaulrich/xts/issues/248)
   [#259](https://github.com/joshuaulrich/xts/issues/259))

*  Fix subset when 'index(x)' and 'i' contain duplicates. Thanks to Stack
   Overflow user [scs](https://stackoverflow.com/users/4024268/scs) for the
   report, and Philippe Verspeelt for debugging.
   ([#275](https://github.com/joshuaulrich/xts/issues/275))

*  Fix if-statement in xts constructor that may use a logical with length > 1.
   Thanks to @HughParsonage for the report and PR.
   ([#270](https://github.com/joshuaulrich/xts/issues/270),
   [#272](https://github.com/joshuaulrich/xts/pull/272))

*  Register shift.time.xts() method. Thanks to Philippe Verspeelt for the
   report and PR.
   ([#268](https://github.com/joshuaulrich/xts/issues/268),
   [#273](https://github.com/joshuaulrich/xts/pull/273))

*  Conditionally register S3 methods for as.timeSeries.xts() and as.fts.xts()
   when their respective packages are available (as requested by CRAN). Note
   that this means these two functions are no longer exported. This may break
   some existing code that calls the methods directly, though 'best practice'
   is to let method dispatch determine which method to invoke.

# xts 0.11-1 (2018-09-12)

*  Fix regression in .xts() that caused 'tclass' argument/attribute to be
   incorrectly set to POSIXct regardless of user-provided value. Thanks to
   @Eluvias for the report and Tom Andrews for the PR.
   ([#249](https://github.com/joshuaulrich/xts/issues/249),
   [#250](https://github.com/joshuaulrich/xts/pull/250))

*  Fix performance regression when xts object is subset by a date-time vector.
   Thanks to Tom Andrews for the report, and the PR to fix the bug in my patch.
   ([#251](https://github.com/joshuaulrich/xts/issues/251),
   [#263](https://github.com/joshuaulrich/xts/issues/263),
   [#264](https://github.com/joshuaulrich/xts/pull/264))

*  Restore behavior from 0.10-2 so subsetting an empty xts object by a date-
   time vector returns an empty xts object instead of throwing an error.
   Thanks to @alkment for the report.
   ([#252](https://github.com/joshuaulrich/xts/issues/252))

*  Add ability for merge.xts() to handle multiple character or complex xts
   objects. Thanks to Ken Williams for the report.
   ([#44](https://github.com/joshuaulrich/xts/issues/44))

*  Add ability to use "quarters" to specify tick/grid mark locations on plots.
   This ran but produced an incorrect result in 0.10-2 and threw an error in
   0.11-0. Thanks to Marc Weibel for the report.
   ([#256](https://github.com/joshuaulrich/xts/issues/256))

*  Fix illegal read reported by valgrind. Thanks to Tom Andrews for the report
   and PR.
   ([#236](https://github.com/joshuaulrich/xts/issues/236),
   [#261](https://github.com/joshuaulrich/xts/pull/261))

# xts 0.11-0 (2018-07-16)

*  Fix make.index.unique() to always return a unique and sorted index. Thanks
   to Chris Katsulis for the report and example.
   ([#241](https://github.com/joshuaulrich/xts/issues/241))

*  Add window.xts() method and completely refactor the internal binary search
   function it depends on. Thanks to Corwin Joy for the PR, tests, review, and
   patience.
   ([#100](https://github.com/joshuaulrich/xts/pull/100),
   [#240](https://github.com/joshuaulrich/xts/pull/240))

*  Better axis tick mark locations for plots. Thanks to Dirk Eddelbuettel for
   the report. Also incorporate axTicksByTime2() into axTicksByTime() to
   reduce code duplication from the migration of quantmod::chart_Series() to
   xts::plot.xts().
   ([#246](https://github.com/joshuaulrich/xts/issues/246),
   [#74](https://github.com/joshuaulrich/xts/issues/74))

*  Add details to plot.xts() parameters that are periodicity, now that RStudio
   has argument completion. Thanks to Evelyn Mitchell for the PR.
   ([#154](https://github.com/joshuaulrich/xts/issues/154))

*  periodicity() now warns instead of errors if the xts object contains less
   than 2 observations.
   ([#230](https://github.com/joshuaulrich/xts/issues/230))

*  first() and last() now keep dims when they would otherwise be dropped by a
   regular row subset. This is consistent with head() and tail(). Thanks to
   Davis Vaughan for the report.
   ([#226](https://github.com/joshuaulrich/xts/issues/226))

*  Fix subset when ISO8601 string is outside the valid range, so it returns no
   data instead of all rows.
   ([#96](https://github.com/joshuaulrich/xts/issues/96))

*  Avoid partial name matches from parse.side() (inside .parseISO8601())
   results that are passed to firstof() and lastof(). Thanks to @gp2x for the
   report and the patch.
   ([#231](https://github.com/joshuaulrich/xts/issues/231))

*  na.locf.xts() now loops over columns of multivariate objects in C code,
   instead of in R. This should improve speed and memory performance. Thanks to
   Chris Katsulis and Tom Andrews for their reports and patches.
   ([#232](https://github.com/joshuaulrich/xts/issues/232),
   [#233](https://github.com/joshuaulrich/xts/issues/233),
   [#234](https://github.com/joshuaulrich/xts/issues/234),
   [#235](https://github.com/joshuaulrich/xts/pull/235),
   [#237](https://github.com/joshuaulrich/xts/pull/237))

*  Change plot.xts() default 'pch = 0' (rectangles) to 'pch = 1' (circles) so
   it looks more like base and zoo plots.
   ([#203](https://github.com/joshuaulrich/xts/issues/203))

# xts 0.10-2 (2018-03-15)

*  na.locf.xts() and na.omit.xts() now support character xts objects. Thanks to
   Ken Williams and Samo Pahor for the reports.
   ([#42](https://github.com/joshuaulrich/xts/issues/42))

*  na.locf.xts() now honors 'x' and 'xout' arguments by dispatching to the next
   method. Thanks to Morten Grum for the report.
   ([#215](https://github.com/joshuaulrich/xts/issues/215))

*  coredata.xts() now functions the same as coredata.zoo() on zero-length
   objects, and only removes xts-related attributes. Thanks to Vincent Guyader
   for the report.
   ([#223](https://github.com/joshuaulrich/xts/issues/223))

*  plot.xts() no longer ignores 'col.up' and 'col.dn' when 'type="h"'
   ([#224](https://github.com/joshuaulrich/xts/issues/224)).
   Thanks to Charlie Friedemann for the report. This was inadvertently broken
   as part of the fix for [#210](https://github.com/joshuaulrich/xts/issues/210).

# xts 0.10-1 (2017-12-20)

*  'ylim' values passed to 'addSeries' and 'addPolygon' via '...' are now
   captured and honored ([#220](https://github.com/joshuaulrich/xts/issues/220)).

*  'addPolygon' now checks for ylim of zeros, as 'addSeries' does
   ([#164](https://github.com/joshuaulrich/xts/issues/164)).

*  The 'base::as.Date.numeric' method is no longer over-ridden. The exported,
   but not registered, method in zoo should prevent any change in behavior.

*  Series added to an existing plot are now given the same index values as
   the main panel ([#216](https://github.com/joshuaulrich/xts/issues/216)).
   There still may be some weird behavior if the new data does not have
   observations within the timespan of the main panel data, but no observations
   on the same timestamps.

*  Existing 'par' values are now captured and reset before returning from
   plotting functions
   ([#217](https://github.com/joshuaulrich/xts/issues/217)).

*  User-defined 'col' values are now honored when 'type="h"'
   ([#210](https://github.com/joshuaulrich/xts/issues/210)).

*  Values passed to plotting functions are now copied from the calling
   environment. This enables plotting arguments to be objects passed
   through multiple layers of function calls.

*  indexFormat is now generic, consistent with indexFormat<-
   ([#188](https://github.com/joshuaulrich/xts/issues/188)).

*  Calling as.matrix() on a zero-width xts object now behaves consistently
   with zoo, and no longer throws an error
   ([#130](https://github.com/joshuaulrich/xts/issues/130)).

*  Fix weird result in merge.xts() when 'fill' argument is NULL or a zero-
   length vector
   ([#261](https://github.com/joshuaulrich/xts/issues/261)).

*  Fixed bug in endpoints() due to sub-second representation error via using
   integer division (%/%) with non- integer arguments
   ([#202](https://github.com/joshuaulrich/xts/issues/202)).

*  endpoints() gained sub-second accuracy on Windows
   ([#202](https://github.com/joshuaulrich/xts/issues/202)).

*  plot.xts() no longer errors when called on an object containing a constant
   value. It chooses ylim values +/-20% from the series value
   ([#156](https://github.com/joshuaulrich/xts/issues/156)).

*  plot.xts() now places y-axis labels in the same location on the plot,
   regardless of data periodicity
   ([#85](https://github.com/joshuaulrich/xts/issues/85)).

*  rbind.xts() now throws an error if passed an xts object with different
   number of observations in the index and data (e.g., zero-width)
   ([#98](https://github.com/joshuaulrich/xts/issues/98)).

# xts 0.10-0 (2017-07-07)

Major changes include:
*  A new plot.xts() that is incompatible with earlier versions of plot.xts().
*  Moved development from R-Forge to GitHub.
*  New xts FAQ.

Other, less disruptive changes include:

*  merge.xts() now throws an error if the index contains non-finite values
   ([#174](https://github.com/joshuaulrich/xts/issues/174)).

*  Constructors xts() and .xts() now ensure order.by and index arguments do not
   contain non-finite values. Many xts functions, most notably merge.xts(),
   expect all index values to be finite. Missing index values usually indicate
   an error, and always occurred at the end of the index
   ([#173](https://github.com/joshuaulrich/xts/issues/173),
   ([#194](https://github.com/joshuaulrich/xts/issues/194),
   ([#199](https://github.com/joshuaulrich/xts/issues/199)).

*  Fixed bug in endpoints() when called on sparse data that have the same month
   and day, but different years
   ([#169](https://github.com/joshuaulrich/xts/issues/169)).

*  Fixed bug in [.xts did not do the same checks on logical xts objects as it
   does for all other data types
   ([#163](https://github.com/joshuaulrich/xts/issues/163)).

*  Fixed bug that caused split.xts() to error if 'f' is a character vector with
   more than 1 element
   ([#134](https://github.com/joshuaulrich/xts/issues/134)).

*  Fixed bug that crashed R if 'k' argument to lag.xts() was not an integer and
   would be NA when coerced to integer
   ([#152](https://github.com/joshuaulrich/xts/issues/152)).

*  period.apply() now checks to ensure the object's index is unique and sorted,
   and sets INDEX <- sort(unique(INDEX)) if it is not. It also ensures INDEX
   starts with 0 and ends with NROW(x)
   ([#171](https://github.com/joshuaulrich/xts/issues/171)).

*  All references to the 'its' package have been removed, since it is now
   archived on CRAN at the request of its maintainer.

*  Fixed bug that crashed R when merge.xts() was called on an empty xts object
   and more than one non-xts object
   ([#157](https://github.com/joshuaulrich/xts/issues/157)).

*  Fixed bug that did not set the index's tzone attribute to UTC when
   index<-.xts or indexClass<- were called and 'value' did not have a tzone
   attribute
   ([#148](https://github.com/joshuaulrich/xts/issues/148)).

*  Fixed a bug in endpoints() that caused incorrect results if the index was
   less than the epoch
   ([#144](https://github.com/joshuaulrich/xts/issues/144)).

*  Fixed a bug that caused diff.xts() on a logical xts object to return an
   object with a POSIXct index.

*  index.xts() works even if the package containing the class for the index
   is not attached (it needs to be loaded, however).

*  [.xts now returns NA if a one-column xts object is subsect by NA, instead
   of throwing an uninformative error
   ([#97](https://github.com/joshuaulrich/xts/issues/97)).

*  Fixed bugs that would crash R when [.xts was called a certain way and 'j'
   contained NA values
   ([#97](https://github.com/joshuaulrich/xts/issues/97),
   [#181](https://github.com/joshuaulrich/xts/issues/181)).

*  Fixed a bug in endpoints() where 1 second would be subtracted for any date
   in the year 1969. The subtraction is only supposed to occur on
   1969-12-31 23:59:59.9... to work around behavior in strptime().

*  timeBasedSeq() now honors hour/min/sec 'BY' values
   ([#91](https://github.com/joshuaulrich/xts/issues/91)).

*  [.xts now throws an error if 'j' is character and not one of the column
   names. This is consistent with [.zoo and [.matrix
   ([#48](https://github.com/joshuaulrich/xts/issues/48)).

*  timeBasedSeq() now works correctly when resolution is "days" the sequence
   includes a daylight saving time change
   ([#67](https://github.com/joshuaulrich/xts/issues/67)).

*  Explicitly set indexTZ="UTC" for all index classes that do not have a TZ
   ([#66](https://github.com/joshuaulrich/xts/issues/66)).  indexTZ="GMT" is also allowed.

*  Fixed as.xts() when called on an 'mts' object ([#64](https://github.com/joshuaulrich/xts/issues/64)).

*  Moved development from R-Forge to GitHub.

*  Fixed bug in to.period() that errored when name=NULL
   ([#2](https://github.com/joshuaulrich/xts/issues/2)).

*  Fixed bug in `.index*` functions that did not account for timezones
   ([#3](https://github.com/joshuaulrich/xts/issues/3)).

*  Fixed bug that allowed index<-.xts to produce an unsorted index
   ([#4](https://github.com/joshuaulrich/xts/issues/4)).

*  Fixed bug so subsetting a zero-width xts object with a zero-length 'i'
   vector no longer returns an object with column names
   ([#5](https://github.com/joshuaulrich/xts/issues/5)).

*  Updated [.xts to handle 'i' containing multiple zeros (e.g. subsetting by a
   "logical" column of an integer xts object).

*  endpoints() now errors if k < 0.

# xts 0.9-7 (2014-01-02)

*  Fixed bug that caused logical operators on xts objects to drop the 'tzone'
   attribute ([#10](https://github.com/joshuaulrich/xts/issues/10)).

*  Fixed bug that ignored 'which.i' argument to [.xts on zero-width xts
   objects ([#12](https://github.com/joshuaulrich/xts/issues/12)).

*  Fixed bug where xts() does not sort 'order.by' if x is missing
   ([#13](https://github.com/joshuaulrich/xts/issues/13)).

*  Fixed bug where setting dimnames to NULL would break as.xts()
   ([#14](https://github.com/joshuaulrich/xts/issues/14)).

*  Added checks to period.sum/prod/min/max to ensure INDEX is in [0,nrow(x)].

*  Fixed missing argument to na_locf() in the C/C++ xtsAPI (Dirk Eddelbuettel).

# xts 0.9-5 (2013-06-24)

*  Increased zoo dependency version to 1.7-10 for changes in C code.

*  Fixed several minor issues in the C/C++ xtsAPI (Dirk Eddelbuettel).

# xts 0.9-4 (2013-06-09)

*  Fixed bug where the index was missing the 'tzone' attribute.

*  Fixed to.period() bug when 'indexAt' is "firstof" or "lastof". (bug [#15](https://github.com/joshuaulrich/xts/issues/15),
   patch [#35](https://github.com/joshuaulrich/xts/issues/35), thanks to Garrett See)

*  Fixed subsetting bug on zero-width xts objects that returned NA data and an
   NA index ([#16](https://github.com/joshuaulrich/xts/issues/16)).

*  xts' merge() method now has 'drop' and 'check.names' arguments to match
   the zoo merge() method.

*  'index<-' now correctly handles UTC Date objects when resetting index
   values. '.index<-' behaved correctly.

*  xts' rollapply() method now handles the 'fill' argument.

*  Added several functions to the C/C++ API:
   - make_index_unique
   - make_unique
   - endpoints
   - do_merge_xts
   - na_omit_xts
   - na_locf

*  Fixed xts' rollapply() method when input has one column, but function
   output has more than one column.

# xts 0.9-3 (2013-01-20)

*  No user-visible changes.

# xts 0.9-2 (2013-01-18)

*  Added C/C++ xtsAPI (Dirk Eddelbuettel)

*  Added tzone() and tclass() functions as aliases to indexTZ() and
   indexClass(), respectively. Eventually will Deprecate/Defunct the former.

# xts 0.9-1 (2012-12-29)

*  xts() now ignores timezone arguments when order.by is Date class, with a
   warning.

# xts 0.8-8 (2012-11-06)

*  Modified str() output to make use of proper ISO-8601 range formating

*  Fixed bug in reclass() when 'tzone' of object is different than system TZ.

*  Fixed bug in xts() that dropped dims when 'x' is a 1-column matrix or
   data.frame.

*  [.xts no longer warns if 'i' is zero-length.

*  to.period() now checks that 'x' is not zero-length/width.

*  Fixed edge case in Ops.xts where two objects with no common index create
   an invalid 'xts' object with no index.

*  to.monthly() and to.quarterly() now default to drop.time=TRUE.

*  Internal .drop.time() now changes the index class to Date. This affects
   the to.period() family of functions.

*  Restore Date index/tclass conversion to POSIXct with a UTC timezone via
   integer division instead of double-precision division.

# xts 0.8-6 (2012-03-25)

*  Revert Date index/tclass conversion to POSIXct with a UTC timezone to
   previous behavior as in 0.8-2.

# xts 0.8-5 (2012-03-24)

*  A Date index/tclass is now internally converted to POSIXct with a UTC
   timezone ensure proper conversion regardless of user TZ settings.

*  tclass is now an argument to .xts()

*  Fix endpoints() properly handles millisecond time stamps (and microsecond
   on not Windows).

*  Subsetting zero-width xts objects now behaves like zoo, with NA values
   returned for valid row requests.

# xts 0.8-2 (2011-08-09)

*  Fixed bug in lag() and diff() for character coredata.

*  Fixed subsetting bug that returned a contiguous chunk of data even when
   a non-contiguous 'i' was provided.

*  Fixed bug that ignored FinCenter/TZ for timeDate index

*  period.apply() now only sets colnames if the number of columns in the input
   and output are equal.

*  Fixed periodicity() when scale = "yearly"

*  Fixed [.xts when row-subsetting via a POSIXct vector, which returned an
   object full of NA.

*  Added '...' to axis() call inside of plot.xts() to allow for 'cex.axis'
   and 'cex.lab' to be passed in.

*  Fixed axes=FALSE issue in plot.xts().

*  Dependency now on 1.7-0 or better of zoo (R-forge at present)
   This build now links to C code moved from xts to zoo. At present
   this is only for zoo_lag (used in lag and lagts)

*  Added 'drop' and 'fromLast' arguments to make.index.unique().

*  Added adj.time() and shift.time()

*  Fixed na.locf() bug that would fill trailing NA larger than 'maxgap'
   observations ([#22](https://github.com/joshuaulrich/xts/issues/22))

*  Updated indexFormat() documentation and add an example

# xts 0.8-0 (2011-02-22)

*  Fix print formatting ([#27](https://github.com/joshuaulrich/xts/issues/27))

*  Fix bug related to na.locf() and zero-width objects ([#28](https://github.com/joshuaulrich/xts/issues/28))

*  Add .RECLASS = FALSE after '...' for as.xts.*() methods. This makes all
   as.xts.*() methods one-way (i.e. not reclass-able). Objects converted to
   xts via try.xts() can still be converted back to their original class
   via relcass().

*  Fix bug that caused colnames to be dropped if object is subset by time
   that is not in the index.

# xts 0.7-5 (2010-08-21)

*  try.xts and reclass now are more efficient on xts objects,
   no longer appending a .RECLASS attribute. This penalty 
   (copying) is shifted to classes that are internally converted
   to xts.

# xts 0.7-4 (2010-08-04)

*  internal attributes of index are now maintaining
   timezone (tzone), time class (tclass) information.

*  `[.xts` method is now using new C code. This may revert
   back as character-based objects are not supported. Changed
   for future code refactoring into zoo, as well as performance
   gains on integer, double and logical values. Also added in
   checks for NAs.  drop=TRUE now works correctly in all known
   applications.

*  (cbind)merge.xts and rbind.xts now copy index attributes
   to handle internal changes to index characteristics (in C code)

*  indexTZ.Rd updated to provide information regarding internal
   changes.  Also indexTZ<- is now exported to facilitate 
   timezone changing of xts objects.

# xts 0.7-1 (2010-05-08)

*  subsecond ISO8601 subsetting on dates
   before 1970 (epoch) is disabled. This is due to a bug
   in the R implementation of POSIX handling of fractional
   seconds pre-1970.  10 microsecond granularity is still
   functional for all other times. Thanks to Andreas Noack Jensen
   for the early bug report.

*  new 'tzone' arg in xts constructor and 'tz' in .parseISO8601
   allows for future support of non-system TZ dependent indexing

*  internal index attribute (numeric) now can have attributes
   set (tzone is currently the only one used in xts). These should
   remain during all xts operations. Still experimental.

*  naCheck has been exposed at the C level for use in packages
   "LinkingTo: xts".  See ?xtsAPI for more details.

# xts 0.7-0 (2010-01-26)

*  A new NEWS file.  

*  print.xts now passes ...

*  endpoints speedup and bug fix (thanks Ismail Onur Filiz)  

*  na.omit bug on logical and NaN fixes (thanks Fabrizio Pollastri
   and Koert Kuipers)

*  fromLast=FALSE for na.locf.xts.  Matching to zoo. (thanks
   to Sandor Benczik)

*  LGLSXP support in leadingNA (R fun naCheck)

*  fixed logical and NA 'j' subsetting. Thanks Koert Kuipers.

*  as.xts and as.timeSeries fixes for timeSeries changes

*  merge and subset now support dimensionless xts (non-standard).
   merge segfault fixed when merging all 3 or more zero-width xts objects
   and only zero-width objects.  Thanks to Koert Kuipers for the report.

*  added which.i to [.xts to return i values found via
   ISO8601 subset string

*  new lines.xts and plot.xts, similar to methods in zoo

*  lastof now has sec to 10 microsecond precision, and subsec
   arg to append to secs.

*  xts() further consistency in NROW/index check

*  align.time error checks for positive n= values (thanks Brian Peterson)

*  toPeriod updates in C, almost exported. ~600-1200x faster

*  new lag_xts in C.  Increased speed and index flexibility.

*  endpoints 'days' bug fix 

*  .makeISO8601 function to create ISO8601 compliant string
   from xts objects
