library(xts)


x <- xts(cbind(1:10, 1:10), .Date(1:10))

# NOTE: expected value is (2 + 2) to account for
#       (1) column header: "             [,1]  [,2]"
#       (2) message: "[ reached getOption("max.print") -- omitted 6 rows ]"
print_output <- utils::capture.output(print(x, max = 4))
expect_true(length(print_output) == (2 + 2), info = "'max' argument is respected'")

print_output <- utils::capture.output(print(x, max = 4, show.nrows = 10))
expect_true(length(print_output) == (2 + 2),
            info = "'max' takes precedence over 'show.nrows'")
