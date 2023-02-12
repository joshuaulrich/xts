# timeBasedSeq tests

# 1999 to 2008 by year, Date
info_msg <- "test.tbs_1999_to_2008_by_year_Date"
tbs <- timeBasedSeq('1999/2008')
bench <- seq(as.Date("1999-01-01"),as.Date("2008-01-01"),by='year')
expect_equivalent(tbs, bench, info = info_msg)

# 1999 to 2008 by year, retclass='Date'
info_msg <- "test.tbs_1999_to_2008_by_year_retclassDate"
tbs <- timeBasedSeq('1999/2008', retclass='Date')
bench <- seq(as.Date("1999-01-01"),as.Date("2008-01-01"),by='year')
expect_equivalent(tbs, bench, info = info_msg)

# 1999 to 2008 by year, retclass="POSIXct"
info_msg <- "test.tbs_1999_to_2008_by_year"
tbs <- timeBasedSeq('1999/2008',retclass='POSIXct')
bench <- seq(as.POSIXct("1999-01-01"),as.POSIXct("2008-01-01"),by='year')
expect_equivalent(tbs, bench, info = info_msg)

# MONTHLY sequences
# defaults to yearmon from the zoo package
# NB: these differ by ~4.16e-5 on Solaris and rhub's windows-x86_64-devel
info_msg <- "test.tbs_199901_to_200801_by_month"
tbs <- timeBasedSeq('199901/200801')
bench <- as.yearmon(seq(as.Date("1999-01-01"),as.Date("2008-01-01"),by='month'))
expect_equivalent(tbs, bench, tolerance = 1e-4, info = info_msg)

info_msg <- "test.tbs_199901_to_2008_by_month"
tbs <- timeBasedSeq('199901/2008')
bench <- as.yearmon(seq(as.Date("1999-01-01"),as.Date("2008-12-01"),by='month'))
expect_equivalent(tbs, bench, tolerance = 1e-4, info = info_msg)

info_msg <- "test.tbs_1999_to_200801_by_month"
tbs <- timeBasedSeq('1999/200801')
bench <- as.yearmon(seq(as.Date("1999-01-01"),as.Date("2008-01-01"),by='month'))
expect_equivalent(tbs, bench, tolerance = 1e-4, info = info_msg)

# retclass=Date
info_msg <- "test.tbs_199901_to_200801_by_month_Date"
tbs <- timeBasedSeq('199901/200801', retclass='Date')
bench <- seq(as.Date("1999-01-01"),as.Date("2008-01-01"),by='month')
expect_equivalent(tbs, bench, info = info_msg)

info_msg <- "test.tbs_199901_to_2008_by_month_Date"
tbs <- timeBasedSeq('199901/2008', retclass='Date')
bench <- seq(as.Date("1999-01-01"),as.Date("2008-12-01"),by='month')
expect_equivalent(tbs, bench, info = info_msg)

info_msg <- "test.tbs_1999_to_200801_by_month_Date"
tbs <- timeBasedSeq('1999/200801', retclass='Date')
bench <- as.Date(seq(as.Date("1999-01-01"),as.Date("2008-01-01"),by='month'))
expect_equivalent(tbs, bench, info = info_msg)

# retclass=POSIXct
info_msg <- "test.tbs_199901_to_200801_by_month_POSIXct"
tbs <- timeBasedSeq('199901/200801', retclass='POSIXct')
bench <- seq(as.POSIXct("1999-01-01"),as.POSIXct("2008-01-01"),by='month')
expect_equivalent(tbs, bench, info = info_msg)

info_msg <- "test.tbs_199901_to_2008_by_month_POSIXct"
tbs <- timeBasedSeq('199901/2008', retclass='POSIXct')
bench <- as.POSIXct(seq(as.POSIXct("1999-01-01"),as.POSIXct("2008-12-01"),by='month'))
expect_equivalent(tbs, bench, info = info_msg)

info_msg <- "test.tbs_1999_to_200801_by_month_POSIXct"
tbs <- timeBasedSeq('1999/200801', retclass='POSIXct')
bench <- seq(as.POSIXct("1999-01-01"),as.POSIXct("2008-01-01"),by='month')
expect_equivalent(tbs, bench, info = info_msg)
