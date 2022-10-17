dates <- c("2017-01-01", "2017-01-02", "2017-01-03", "2017-01-04")
d1 <- data.frame(x = seq_along(dates), row.names = dates)
d2 <- data.frame(d1, y = rev(seq_along(dates)))

### basic functionality on data.frame
info_msg <- "test.first_xtsible_data.frame_pos_n"
expect_identical(first(d1, 1), head(d1, 1), info = info_msg)
expect_identical(first(d2, 1), head(d2, 1), info = info_msg)

expect_identical(first(d1, "1 day"), head(d1, 1), info = info_msg)
expect_identical(first(d2, "1 day"), head(d2, 1), info = info_msg)

info_msg <- "test.first_xtsible_data.frame_neg_n"
expect_identical(first(d1, -1), tail(d1, -1), info = info_msg)
expect_identical(first(d2, -1), tail(d2, -1), info = info_msg)

expect_identical(first(d1, "-1 day"), tail(d1, -1), info = info_msg)
expect_identical(first(d2, "-1 day"), tail(d2, -1), info = info_msg)

info_msg <- "test.first_xtsible_data.frame_zero_n"
expect_identical(first(d1, 0), head(d1, 0), info = info_msg)
expect_identical(first(d2, 0), head(d2, 0), info = info_msg)

expect_identical(first(d1, "0 day"), head(d1, 0), info = info_msg)
expect_identical(first(d2, "0 day"), head(d2, 0), info = info_msg)

info_msg <- "test.last_xtsible_data.frame_pos_n"
expect_identical(last(d1, 1), tail(d1, 1), info = info_msg)
expect_identical(last(d2, 1), tail(d2, 1), info = info_msg)

expect_identical(last(d1, "1 day"), tail(d1, 1), info = info_msg)
expect_identical(last(d2, "1 day"), tail(d2, 1), info = info_msg)

info_msg <- "test.last_xtsible_data.frame_neg_n"
expect_identical(last(d1, -1), head(d1, -1), info = info_msg)
expect_identical(last(d2, -1), head(d2, -1), info = info_msg)

expect_identical(last(d1, "-1 day"), head(d1, -1), info = info_msg)
expect_identical(last(d2, "-1 day"), head(d2, -1), info = info_msg)

info_msg <- "test.last_xtsible_data.frame_zero_n"
expect_identical(last(d1, 0), head(d1, 0), info = info_msg)
expect_identical(last(d2, 0), head(d2, 0), info = info_msg)

expect_identical(last(d1, "0 day"), head(d1, 0), info = info_msg)
expect_identical(last(d2, "0 day"), head(d2, 0), info = info_msg)

### non-xtsible data.frames
d1 <- data.frame(x = seq_along(dates), row.names = dates)
d2 <- data.frame(d1, y = rev(seq_along(dates)))
rownames(d1) <- rownames(d2) <- NULL

info_msg <- "test.first_nonxtsible_data.frame_pos_n"
expect_identical(first(d1, 1), head(d1, 1), info = info_msg)
expect_identical(first(d2, 1), head(d2, 1), info = info_msg)

info_msg <- "test.first_nonxtsible_data.frame_neg_n"
rownames(d1) <- rownames(d2) <- NULL
expect_identical(first(d1, -1), tail(d1, -1), info = info_msg)
expect_identical(first(d2, -1), tail(d2, -1), info = info_msg)

info_msg <- "test.first_nonxtsible_data.frame_zero_n"
rownames(d1) <- rownames(d2) <- NULL
expect_identical(first(d1, 0), tail(d1, 0), info = info_msg)
expect_identical(first(d2, 0), tail(d2, 0), info = info_msg)

info_msg <- "test.last_nonxtsible_data.frame_pos_n"
rownames(d1) <- rownames(d2) <- NULL
expect_identical(last(d1, 1), tail(d1, 1), info = info_msg)
expect_identical(last(d2, 1), tail(d2, 1), info = info_msg)

info_msg <- "test.last_nonxtsible_data.frame_neg_n"
rownames(d1) <- rownames(d2) <- NULL
expect_identical(last(d1, -1), head(d1, -1), info = info_msg)
expect_identical(last(d2, -1), head(d2, -1), info = info_msg)

info_msg <- "test.last_nonxtsible_data.frame_zero_n"
rownames(d1) <- rownames(d2) <- NULL
expect_identical(last(d1, 0), head(d1, 0), info = info_msg)
expect_identical(last(d2, 0), head(d2, 0), info = info_msg)


### basic functionality on matrix
d1 <- data.frame(x = seq_along(dates), row.names = dates)
d2 <- data.frame(d1, y = rev(seq_along(dates)))
m1 <- as.matrix(d1)
m2 <- as.matrix(d2)

info_msg <- "test.first_xtsible_matrix_pos_n"
expect_identical(first(m1, 1), head(m1, 1), info = info_msg)
expect_identical(first(m2, 1), head(m2, 1), info = info_msg)

expect_identical(first(m1, "1 day"), head(m1, 1), info = info_msg)
expect_identical(first(m2, "1 day"), head(m2, 1), info = info_msg)

info_msg <- "test.first_xtsible_matrix_neg_n"
expect_identical(first(m1, -1), tail(m1, -1, keepnums = FALSE), info = info_msg)
expect_identical(first(m2, -1), tail(m2, -1, keepnums = FALSE), info = info_msg)

expect_identical(first(m1, "-1 day"), tail(m1, -1, keepnums = FALSE), info = info_msg)
expect_identical(first(m2, "-1 day"), tail(m2, -1, keepnums = FALSE), info = info_msg)

info_msg <- "test.first_xtsible_matrix_zero_n"
expect_identical(first(m1, 0), tail(m1, 0, keepnums = FALSE), info = info_msg)
expect_identical(first(m2, 0), tail(m2, 0, keepnums = FALSE), info = info_msg)

expect_identical(first(m1, "0 day"), tail(m1, 0, keepnums = FALSE), info = info_msg)
expect_identical(first(m2, "0 day"), tail(m2, 0, keepnums = FALSE), info = info_msg)

info_msg <- "test.last_xtsible_matrix_pos_n"
expect_identical(last(m1, 1), tail(m1, 1, keepnums = FALSE), info = info_msg)
expect_identical(last(m2, 1), tail(m2, 1, keepnums = FALSE), info = info_msg)

expect_identical(last(m1, "1 day"), tail(m1, 1, keepnums = FALSE), info = info_msg)
expect_identical(last(m2, "1 day"), tail(m2, 1, keepnums = FALSE), info = info_msg)

info_msg <- "test.last_xtsible_matrix_neg_n"
expect_identical(last(m1, -1), head(m1, -1), info = info_msg)
expect_identical(last(m2, -1), head(m2, -1), info = info_msg)

info_msg <- "test.last_xtsible_matrix_zero_n"
expect_identical(last(m1, 0), head(m1, 0), info = info_msg)
expect_identical(last(m2, 0), head(m2, 0), info = info_msg)

info_msg <- "test.first_nonxtsible_matrix_pos_n"
rownames(m1) <- rownames(m2) <- NULL
expect_identical(first(m1, 1), head(m1, 1), info = info_msg)
expect_identical(first(m2, 1), head(m2, 1), info = info_msg)

info_msg <- "test.first_nonxtsible_matrix_neg_n"
rownames(m1) <- rownames(m2) <- NULL
expect_identical(first(m1, -1), tail(m1, -1, keepnums = FALSE), info = info_msg)
expect_identical(first(m2, -1), tail(m2, -1, keepnums = FALSE), info = info_msg)

info_msg <- "test.first_nonxtsible_matrix_zero_n"
rownames(m1) <- rownames(m2) <- NULL
expect_identical(first(m1, 0), tail(m1, 0, keepnums = FALSE), info = info_msg)
expect_identical(first(m2, 0), tail(m2, 0, keepnums = FALSE), info = info_msg)

info_msg <- "test.last_nonxtsible_matrix_pos_n"
rownames(m1) <- rownames(m2) <- NULL
expect_identical(last(m1, 1), tail(m1, 1, keepnums = FALSE), info = info_msg)
expect_identical(last(m2, 1), tail(m2, 1, keepnums = FALSE), info = info_msg)

info_msg <- "test.last_nonxtsible_matrix_neg_n"
rownames(m1) <- rownames(m2) <- NULL
expect_identical(last(m1, -1), head(m1, -1), info = info_msg)
expect_identical(last(m2, -1), head(m2, -1), info = info_msg)

info_msg <- "test.last_nonxtsible_matrix_zero_n"
rownames(m1) <- rownames(m2) <- NULL
expect_identical(last(m1, 0), head(m1, 0), info = info_msg)
expect_identical(last(m2, 0), head(m2, 0), info = info_msg)


### basic functionality on vector
d1 <- data.frame(x = seq_along(dates), row.names = dates)
d2 <- data.frame(d1, y = rev(seq_along(dates)))

info_msg <- "test.first_xtsible_vector"
v1 <- setNames(d1$x, rownames(d1))
expect_identical(first(v1, 1), head(v1, 1), info = info_msg)
expect_identical(first(v1,-1), tail(v1,-1), info = info_msg)
expect_identical(first(v1, "1 day"), head(v1, 1), info = info_msg)
expect_identical(first(v1,"-1 day"), tail(v1,-1), info = info_msg)
expect_identical(first(v1, "2 days"), head(v1, 2), info = info_msg)
expect_identical(first(v1,"-2 days"), tail(v1,-2), info = info_msg)

d <- .Date(3) + 1:21
expect_identical(first(d, "1 week"), head(d, 7), info = info_msg)
expect_identical(first(d,"-1 week"), tail(d,-7), info = info_msg)
expect_identical(first(d, "2 weeks"), head(d, 14), info = info_msg)
expect_identical(first(d,"-2 weeks"), tail(d,-14), info = info_msg)

info_msg <- "test.last_xtsible_vector"
v1 <- setNames(d1$x, rownames(d1))
expect_identical(last(v1, 1), tail(v1, 1), info = info_msg)
expect_identical(last(v1,-1), head(v1,-1), info = info_msg)
expect_identical(last(v1, "1 day"), tail(v1, 1), info = info_msg)
expect_identical(last(v1,"-1 day"), head(v1,-1), info = info_msg)

d <- .Date(3) + 1:21
expect_identical(last(d, "1 week"), tail(d, 7), info = info_msg)
expect_identical(last(d,"-1 week"), head(d,-7), info = info_msg)
expect_identical(last(d, "2 weeks"), tail(d, 14), info = info_msg)
expect_identical(last(d,"-2 weeks"), head(d,-14), info = info_msg)

info_msg <- "test.first_nonxtsible_vector"
v1 <- d1$x
expect_identical(first(v1, 1), head(v1, 1), info = info_msg)
expect_identical(first(v1,-1), tail(v1,-1), info = info_msg)
expect_identical(first(v1,0), tail(v1,0), info = info_msg)

info_msg <- "test.last_nonxtsible_vector"
v1 <- d1$x
expect_identical(last(v1, 1), tail(v1, 1), info = info_msg)
expect_identical(last(v1,-1), head(v1,-1), info = info_msg)
expect_identical(last(v1,0), head(v1,0), info = info_msg)

### zero-length vectors
info_msg <- "test.zero_length_vector"
types <- c("logical", "integer", "numeric", "complex", "character", "raw")
for (type in types) {
  v <- vector(type, 0)
  expect_identical(first(v,  1), v, info = paste(info_msg, type, "- first, n = 1"))
  expect_identical( last(v,  1), v, info = paste(info_msg, type, "- last, n = 1"))
  # negative 'n'
  expect_identical(first(v, -1), v, info = paste(info_msg, type, "- first, n = -1"))
  expect_identical( last(v, -1), v, info = paste(info_msg, type, "- last, n = -1"))
  #zero 'n'
  expect_identical(first(v,  0), v, info = paste(info_msg, type, "- first, n = 0"))
  expect_identical( last(v,  0), v, info = paste(info_msg, type, "- last, n = 0"))
}

### zero-row matrix
info_msg <- "test.zero_row_matrix"
types <- c("logical", "integer", "numeric", "complex", "character", "raw")
for (type in types) {
  m <- matrix(vector(type, 0), 0)
  expect_identical(first(m,  1), m, info = paste(info_msg, type, "- first, n = 1"))
  expect_identical( last(m,  1), m, info = paste(info_msg, type, "- last, n = 1"))
  # negative 'n'
  expect_identical(first(m, -1), m, info = paste(info_msg, type, "- first, n = -1"))
  expect_identical( last(m, -1), m, info = paste(info_msg, type, "- last, n = -1"))
  #zero 'n'
  expect_identical(first(m,  0), m, info = paste(info_msg, type, "- first, n = 0"))
  expect_identical( last(m,  0), m, info = paste(info_msg, type, "- last, n = 0"))
}

### tests for zoo
z1 <- zoo(seq_along(dates), as.Date(dates))
z2 <- merge(x = z1, y = rev(seq_along(dates)))

info_msg <- "test.first_zoo_pos_n"
expect_identical(first(z1, 1), head(z1, 1), info = info_msg)
expect_identical(first(z2, 1), head(z2, 1), info = info_msg)

expect_identical(first(z1, "1 day"), head(z1, 1), info = info_msg)
expect_identical(first(z2, "1 day"), head(z2, 1), info = info_msg)

info_msg <- "test.first_zoo_neg_n"
expect_identical(first(z1, -1), tail(z1, -1), info = info_msg)
expect_identical(first(z2, -1), tail(z2, -1), info = info_msg)

expect_identical(first(z1, "-1 day"), tail(z1, -1), info = info_msg)
expect_identical(first(z2, "-1 day"), tail(z2, -1), info = info_msg)

info_msg <- "test.last_zoo_pos_n"
expect_identical(last(z1, 1), tail(z1, 1), info = info_msg)
expect_identical(last(z2, 1), tail(z2, 1), info = info_msg)

expect_identical(last(z1, "1 day"), tail(z1, 1), info = info_msg)
expect_identical(last(z2, "1 day"), tail(z2, 1), info = info_msg)

info_msg <- "test.last_zoo_neg_n"
expect_identical(last(z1, -1), head(z1, -1), info = info_msg)
expect_identical(last(z2, -1), head(z2, -1), info = info_msg)

expect_identical(last(z1, "-1 day"), head(z1, -1), info = info_msg)
expect_identical(last(z2, "-1 day"), head(z2, -1), info = info_msg)

