# make.index.unique

# test.make.index.unique_unsorted_result_warns <- function() {
#   x <- .xts(1:5, c(rep(0, 4), 2e-6))
#   orig <- options(warn = 2)
#   on.exit(options(warn = orig$warn))
#   checkException(y <- make.index.unique(x))
# }

test.make.index.unique_returns_sorted_index <- function() {
  x <- .xts(1:5, c(rep(1e-6, 4), 3e-6))
  y <- make.index.unique(x)
  checkEqualsNumeric(.index(y), cumsum(rep(1e-6, 5)))
}

test.make.index.unique_adds_eps_to_duplicates <- function() {
  epsilon <- c(1e-6, 1e-7, 1e-8)
  for (eps in epsilon) {
    x <- .xts(1:5, rep(eps, 5))
    y <- make.index.unique(x, eps = eps)
    checkEqualsNumeric(.index(y), cumsum(rep(eps, 5)))
  }
}
