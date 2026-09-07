# .parseTimeOfDay unit tests
.parseTimeOfDay <- xts:::.parseTimeOfDay  # local copy

info_msg <- "hour only"
expect_identical(.parseTimeOfDay("09"), list(tz = "UTC", hour = 9), info = info_msg)
expect_identical(.parseTimeOfDay("9"),  list(tz = "UTC", hour = 9), info = info_msg)
expect_identical(.parseTimeOfDay("0"),  list(tz = "UTC", hour = 0), info = info_msg)
expect_identical(.parseTimeOfDay("23"), list(tz = "UTC", hour = 23), info = info_msg)

info_msg <- "hour and minute"
hm <- list(tz = "UTC", hour = 9, min = 30)
expect_identical(.parseTimeOfDay("0930"),  hm, info = info_msg)
expect_identical(.parseTimeOfDay("09:30"), hm, info = info_msg)
expect_identical(.parseTimeOfDay("930"),   hm, info = info_msg)   # unpadded hour

info_msg <- "hour, minute, and second"
hms <- list(tz = "UTC", hour = 9, min = 30, sec = 0)
expect_identical(.parseTimeOfDay("093000"),   hms, info = info_msg)
expect_identical(.parseTimeOfDay("09:30:00"), hms, info = info_msg)
expect_identical(.parseTimeOfDay("0930:00"),  hms, info = info_msg)   # mixed colons
expect_identical(.parseTimeOfDay("09:3000"),  hms, info = info_msg)   # mixed colons

info_msg <- "colons are optional"
expect_identical(.parseTimeOfDay("0100"),   .parseTimeOfDay("01:00"),    info = info_msg)
expect_identical(.parseTimeOfDay("013000"), .parseTimeOfDay("01:30:00"), info = info_msg)

info_msg <- "unpadded single-digit hour"
expect_identical(.parseTimeOfDay("1"),     list(tz = "UTC", hour = 1), info = info_msg)
expect_identical(.parseTimeOfDay("13000"), list(tz = "UTC", hour = 1, min = 30, sec = 0), info = info_msg)

info_msg <- "boundary times 00:00:00 and 23:59:59"
expect_identical(.parseTimeOfDay("000000"), list(tz = "UTC", hour = 0,  min = 0,  sec = 0),  info = info_msg)
expect_identical(.parseTimeOfDay("235959"), list(tz = "UTC", hour = 23, min = 59, sec = 59), info = info_msg)

info_msg <- "subseconds do not disturb hour, minute, second"
expect_identical(.parseTimeOfDay("09:30:00.5")[c("tz", "hour", "min", "sec")],
                 list(tz = "UTC", hour = 9, min = 30, sec = 0), info = info_msg)

info_msg <- "malformed time strings are rejected"
expect_error(.parseTimeOfDay("09:5"),  pattern = "not a valid time of day", info = info_msg)
expect_error(.parseTimeOfDay("1:2:3"), pattern = "not a valid time of day", info = info_msg)
expect_error(.parseTimeOfDay("ab"),    pattern = "not a valid time of day", info = info_msg)

info_msg <- "hour out of range"
expect_error(.parseTimeOfDay("24"),   pattern = "hours must be in 00-23", info = info_msg)
expect_error(.parseTimeOfDay("2400"), pattern = "hours must be in 00-23", info = info_msg)

info_msg <- "minute out of range"
expect_error(.parseTimeOfDay("0960"), pattern = "minutes must be in 00-59", info = info_msg)

info_msg <- "second out of range"
expect_error(.parseTimeOfDay("095960"), pattern = "seconds must be in 00-59", info = info_msg)
