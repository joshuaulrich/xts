dates <- c("2017-01-01", "2017-01-02", "2017-01-03", "2017-01-04")
d1 <- data.frame(x = seq_along(dates), row.names = dates)
d2 <- data.frame(d1, y = rev(seq_along(dates)))

# basic functionality on data.frame
test.first_xtsible_data.frame_pos_n <- function() {
  checkIdentical(first(d1, 1), head(d1, 1))
  checkIdentical(first(d2, 1), head(d2, 1))

  checkIdentical(first(d1, "1 day"), head(d1, 1))
  checkIdentical(first(d2, "1 day"), head(d2, 1))
}
test.first_xtsible_data.frame_neg_n <- function() {
  checkIdentical(first(d1, -1), tail(d1, -1))
  checkIdentical(first(d2, -1), tail(d2, -1))

  checkIdentical(first(d1, "-1 day"), tail(d1, -1))
  checkIdentical(first(d2, "-1 day"), tail(d2, -1))
}
test.last_xtsible_data.frame_pos_n <- function() {
  checkIdentical(last(d1, 1), tail(d1, 1))
  checkIdentical(last(d2, 1), tail(d2, 1))

  checkIdentical(last(d1, "1 day"), tail(d1, 1))
  checkIdentical(last(d2, "1 day"), tail(d2, 1))
}
test.last_xtsible_data.frame_neg_n <- function() {
  checkIdentical(last(d1, -1), head(d1, -1))
  checkIdentical(last(d2, -1), head(d2, -1))

  checkIdentical(last(d1, "-1 day"), head(d1, -1))
  checkIdentical(last(d2, "-1 day"), head(d2, -1))
}
test.first_nonxtsible_data.frame_pos_n <- function() {
  rownames(d1) <- rownames(d2) <- NULL
  checkIdentical(first(d1, 1), head(d1, 1))
  checkIdentical(first(d2, 1), head(d2, 1))
}
test.first_nonxtsible_data.frame_neg_n <- function() {
  rownames(d1) <- rownames(d2) <- NULL
  checkIdentical(first(d1, -1), tail(d1, -1))
  checkIdentical(first(d2, -1), tail(d2, -1))
}
test.last_nonxtsible_data.frame_pos_n <- function() {
  rownames(d1) <- rownames(d2) <- NULL
  checkIdentical(last(d1, 1), tail(d1, 1))
  checkIdentical(last(d2, 1), tail(d2, 1))
}
test.last_nonxtsible_data.frame_neg_n <- function() {
  rownames(d1) <- rownames(d2) <- NULL
  checkIdentical(last(d1, -1), head(d1, -1))
  checkIdentical(last(d2, -1), head(d2, -1))
}

# basic functionality on matrix
m1 <- as.matrix(d1)
m2 <- as.matrix(d2)

test.first_xtsible_matrix_pos_n <- function() {
  checkIdentical(first(m1, 1), head(m1, 1))
  checkIdentical(first(m2, 1), head(m2, 1))

  checkIdentical(first(m1, "1 day"), head(m1, 1))
  checkIdentical(first(m2, "1 day"), head(m2, 1))
}
test.first_xtsible_matrix_neg_n <- function() {
  checkIdentical(first(m1, -1), tail(m1, -1, addrownums = FALSE))
  checkIdentical(first(m2, -1), tail(m2, -1, addrownums = FALSE))

  checkIdentical(first(m1, "-1 day"), tail(m1, -1, addrownums = FALSE))
  checkIdentical(first(m2, "-1 day"), tail(m2, -1, addrownums = FALSE))
}
test.last_xtsible_matrix_pos_n <- function() {
  checkIdentical(last(m1, 1), tail(m1, 1, addrownums = FALSE))
  checkIdentical(last(m2, 1), tail(m2, 1, addrownums = FALSE))

  checkIdentical(last(m1, "1 day"), tail(m1, 1, addrownums = FALSE))
  checkIdentical(last(m2, "1 day"), tail(m2, 1, addrownums = FALSE))
}
test.last_xtsible_matrix_neg_n <- function() {
  checkIdentical(last(m1, -1), head(m1, -1))
  checkIdentical(last(m2, -1), head(m2, -1))
}
test.first_nonxtsible_matrix_pos_n <- function() {
  rownames(m1) <- rownames(m2) <- NULL
  checkIdentical(first(m1, 1), head(m1, 1))
  checkIdentical(first(m2, 1), head(m2, 1))
}
test.first_nonxtsible_matrix_neg_n <- function() {
  rownames(m1) <- rownames(m2) <- NULL
  checkIdentical(first(m1, -1), tail(m1, -1, addrownums = FALSE))
  checkIdentical(first(m2, -1), tail(m2, -1, addrownums = FALSE))
}
test.last_nonxtsible_matrix_pos_n <- function() {
  rownames(m1) <- rownames(m2) <- NULL
  checkIdentical(last(m1, 1), tail(m1, 1, addrownums = FALSE))
  checkIdentical(last(m2, 1), tail(m2, 1, addrownums = FALSE))
}
test.last_nonxtsible_matrix_neg_n <- function() {
  rownames(m1) <- rownames(m2) <- NULL
  checkIdentical(last(m1, -1), head(m1, -1))
  checkIdentical(last(m2, -1), head(m2, -1))
}

# basic functionality on vector
test.first_xtsible_vector <- function() {
  v1 <- setNames(d1$x, rownames(d1))
  checkIdentical(first(v1, 1), head(v1, 1))
  checkIdentical(first(v1,-1), tail(v1,-1))
  checkIdentical(first(v1, "1 day"), head(v1, 1))
  checkIdentical(first(v1,"-1 day"), tail(v1,-1))
  checkIdentical(first(v1, "2 days"), head(v1, 2))
  checkIdentical(first(v1,"-2 days"), tail(v1,-2))

  d <- .Date(3) + 1:21
  checkIdentical(first(d, "1 week"), head(d, 7))
  checkIdentical(first(d,"-1 week"), tail(d,-7))
  checkIdentical(first(d, "2 weeks"), head(d, 14))
  checkIdentical(first(d,"-2 weeks"), tail(d,-14))
}
test.last_xtsible_vector <- function() {
  v1 <- setNames(d1$x, rownames(d1))
  checkIdentical(last(v1, 1), tail(v1, 1))
  checkIdentical(last(v1,-1), head(v1,-1))
  checkIdentical(last(v1, "1 day"), tail(v1, 1))
  checkIdentical(last(v1,"-1 day"), head(v1,-1))

  d <- .Date(3) + 1:21
  checkIdentical(last(d, "1 week"), tail(d, 7))
  checkIdentical(last(d,"-1 week"), head(d,-7))
  checkIdentical(last(d, "2 weeks"), tail(d, 14))
  checkIdentical(last(d,"-2 weeks"), head(d,-14))
}
test.first_nonxtsible_vector <- function() {
  v1 <- d1$x
  checkIdentical(first(v1, 1), head(v1, 1))
  checkIdentical(first(v1,-1), tail(v1,-1))
}
test.last_nonxtsible_vector <- function() {
  v1 <- d1$x
  checkIdentical(last(v1, 1), tail(v1, 1))
  checkIdentical(last(v1,-1), head(v1,-1))
}
# zero-length vectors
test.zero_length_vector <- function() {
  types <- c("logical", "integer", "numeric", "complex", "character", "raw")

  for (type in types) {
    v <- vector(type, 0)
    checkIdentical(first(v, 1), v, paste("zero-length", type))
    checkIdentical(last(v, 1), v, paste("zero-length", type))
    # negative 'n'
    checkIdentical(first(v, -1), v, paste("zero-length", type))
    checkIdentical(last(v, -1), v, paste("zero-length", type))
  }
}
# zero-row matrix
test.zero_row_matrix <- function() {
  types <- c("logical", "integer", "numeric", "complex", "character", "raw")

  for (type in types) {
    m <- matrix(vector(type, 0), 0)
    checkIdentical(first(m, 1), m, paste("zero-row", type))
    checkIdentical(last(m, 1), m, paste("zero-row", type))
    # negative 'n'
    checkIdentical(first(m, -1), m, paste("zero-row", type))
    checkIdentical(last(m, -1), m, paste("zero-row", type))
  }
}


# tests for zoo
z1 <- zoo(seq_along(dates), as.Date(dates))
z2 <- merge(x = z1, y = rev(seq_along(dates)))

test.first_zoo_pos_n <- function() {
  checkIdentical(first(z1, 1), head(z1, 1))
  checkIdentical(first(z2, 1), head(z2, 1))

  checkIdentical(first(z1, "1 day"), head(z1, 1))
  checkIdentical(first(z2, "1 day"), head(z2, 1))
}
test.first_zoo_neg_n <- function() {
  checkIdentical(first(z1, -1), tail(z1, -1))
  checkIdentical(first(z2, -1), tail(z2, -1))

  checkIdentical(first(z1, "-1 day"), tail(z1, -1))
  checkIdentical(first(z2, "-1 day"), tail(z2, -1))
}
test.last_zoo_pos_n <- function() {
  checkIdentical(last(z1, 1), tail(z1, 1))
  checkIdentical(last(z2, 1), tail(z2, 1))

  checkIdentical(last(z1, "1 day"), tail(z1, 1))
  checkIdentical(last(z2, "1 day"), tail(z2, 1))
}
test.last_zoo_neg_n <- function() {
  checkIdentical(last(z1, -1), head(z1, -1))
  checkIdentical(last(z2, -1), head(z2, -1))

  checkIdentical(last(z1, "-1 day"), head(z1, -1))
  checkIdentical(last(z2, "-1 day"), head(z2, -1))
}
