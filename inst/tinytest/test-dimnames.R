### colnames(x) are not removed when 'x' and 'y' are shared and dimnames(y) <- NULL
orig_names <- c("a", "b")
x <- .xts(cbind(1:2, 1:2), 1:2, dimnames = list(NULL, orig_names))
y <- x

dimnames(y) <- NULL
expect_null(colnames(y), info = "dimnames(y) <- NULL removes dimnames from y")
expect_identical(orig_names, colnames(x),
                 info = "dimnames(y) <- NULL does not remove dimnames from x")


### colnames(x) are not changed when 'x' and 'y' are shared and dimnames(y) <- foo
new_names <- c("c", "d")
x <- .xts(cbind(1:2, 1:2), 1:2, dimnames = list(NULL, orig_names))
y <- x

dimnames(y) <- list(NULL, new_names)
expect_identical(new_names, colnames(y),
                 info = "dimnames(y) <- list(NULL, new_names) set correctly on y")

expect_identical(orig_names, colnames(x),
                 info = "dimnames(y) <- list(NULL, new_names) does not change dimnames on x")

new_names[1] <- "e"
expect_identical(c("c", "d"), colnames(y),
                 info = "colnames(y) not changed when new_names is changed")
