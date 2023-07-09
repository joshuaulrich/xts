x <- xts(cbind(1:10, 1:10), .Date(1:10))

# NOTE: expected value is (2 + 2) to account for
#       (1) column header: "             [,1]  [,2]"
#       (2) message: "[ reached getOption("max.print") -- omitted 6 rows ]"
print_output <- utils::capture.output(print(x, max = 4))
expect_true(length(print_output) == (2 + 2), info = "'max' argument is respected'")

print_output <- utils::capture.output(print(x, max = 4, show.nrows = 10))
expect_true(length(print_output) == (2 + 2),
            info = "'max' takes precedence over 'show.nrows'")

expect_silent(p <- print(drop(x[, 1])),
              info = "print.xts() does not error when object doesn't have dims")

print_output <- utils::capture.output(print(drop(x[1:2, 1])))
expect_true(all(grepl("1970-01", print_output[-1])),
            info = "print.xts() output shows index when object doesn't have dims")

# 'show.nrows' > 'trunc.rows'
print_output <- utils::capture.output(print(x, show.nrows = 10, trunc.rows = 4))
expect_true(length(print_output)-1 == nrow(x),
            info = "print.xts() shows correct number of rows when show.nrows > trunc.rows")


y <- xts(cbind(1:11, 1:11), .Date(1:11))

show_nrows <- floor(nrow(y) / 2)
print_output <- utils::capture.output(print(y, show.nrows = show_nrows, trunc.rows = nrow(y)-2))
expect_true(length(print_output)-1 == 2*show_nrows+1,
            info = "print.xts() shows correct number of rows when show.nrows < trunc.rows / 2")

show_nrows <- ceiling(nrow(y) / 2)
print_output <- utils::capture.output(print(y, show.nrows = show_nrows, trunc.rows = nrow(y)-2))
expect_true(length(print_output)-1 == nrow(y),
            info = "print.xts() shows correct number of rows when show.nrows > trunc.rows / 2")

print_output <- utils::capture.output(p <- print(x))
expect_identical(p, x, info = "returns input invisibly")

z <- .xts(matrix(0, nrow = 200, ncol = 200), 1:200)
expect_silent(print(z), info = "print more columns than width doesn't error")

expect_silent(print(x, quote = TRUE),
              info = "print.xts() does not error when 'quote' argument is used")

expect_silent(print(x, right = TRUE),
              info = "print.xts() does not error when 'right' argument is used")
