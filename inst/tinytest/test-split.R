nm_minutes <- c("1970-01-01 00:00:00", "1970-01-01 00:01:00")

# 'f' is character, but length(f) > 1
info_msg <- "test.split_character_f_not_endpoints"
x <- .xts(1:5, 1:5)
f <- letters[1:nrow(x)]
expect_identical(split(x,f), split(as.zoo(x),f), info = info_msg)

info_msg <- "test.split_returns_named_list"
qtr_2020 <- paste0("2020 Q", 1:4)
qtr_2021 <- paste0("2021 Q", 1:4)

info_msg <- "quarterly data split by year"
x_q <- xts(1:8, as.yearqtr(c(qtr_2020, qtr_2021)))
nm_q <- names(split(x_q, "years"))
expect_identical(c("2020", "2021"), nm_q, info = info_msg)

# names formatted as yearqtr
info_msg <- "monthly data split by quarter"
x_mo <- xts(1:12, as.yearmon(2020 + 0:11/12))
nm_mo <- names(split(x_mo, "quarters"))
expect_identical(qtr_2020, nm_mo, info = info_msg)

# names formatted as yearmon
info_msg <- "daily data split by month"
x_day <- xts(1:10, .Date(-5:4))
nm_day <- names(split(x_day, "months"))
expect_identical(c("Dec 1969", "Jan 1970"), nm_day, info = info_msg)

# names formatted as Date
info_msg <- "hourly data split by day"
x_hr <- .xts(1:10, -5:4 * 3600, tzone = "UTC")
nm_hr <- names(split(x_hr, "days"))
expect_identical(c("1969-12-31", "1970-01-01"), nm_hr, info = info_msg)

info_msg <- "second data split by minute"
x_sec <- .xts(1:120, 1:120 - 1, tzone = "UTC")
nm_sec <- names(split(x_sec, "minutes"))
expect_identical(nm_minutes, nm_sec, info = info_msg)

if (.Machine$sizeof.pointer == 8) {
    # only run on 64-bit systems because this fails on 32-bit systems due to
    # precision issues
    #
    # ?.Machine says:
    # sizeof.pointer: the number of bytes in a C 'SEXP' type. Will be '4' on
    #     32-bit builds and '8' on 64-bit builds of R.
    info_msg <- "microsecond data split by milliseconds"
    t1 <- as.POSIXct(nm_minutes[1], tz = "UTC")
    us <- seq(1e-4, 2e-1, 1e-4)
    x_us <- xts(seq_along(us), t1 + us)
    nm_ms <- names(split(x_us, "milliseconds"))
    nm_target <- format(t1 + seq(0, 0.2, 0.001), "%Y-%m-%d %H:%M:%OS3")
    expect_identical(nm_target, nm_ms, info = info_msg)
}

# names correct when object TZ vs GMT are on different sides of split breaks (#392)
info_msg <- "yearmon: object TZ and GMT are different days"
x_tz <- .xts(1:3, c(1632481200, 1633042800, 1635724800), tzone = "Europe/Berlin")
expect_identical(names(split(x_tz, "months")),
                 paste(c("Sep", "Oct", "Nov"), "2021"),
                 info = info_msg)

info_msg <- "yearqtr: object TZ and GMT are different days"
expect_identical(names(split(x_tz, "quarters")),
                 c("2021 Q3", "2021 Q4"),
                 info = info_msg)
