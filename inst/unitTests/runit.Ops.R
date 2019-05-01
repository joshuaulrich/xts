### 2-column objects
test.xts_minus_matrix2d_dimnames <- function() {
  m <- matrix(1:6, 3, 2, dimnames = list(format(.POSIXct(1:3)), c("x", "y")))
  x <- .xts(cbind(1:3, 4:6), 1:3, dimnames = list(NULL, c("x", "y")))
  y <- .xts(cbind(1:3, 4:6)*0L, 1:3, dimnames = list(NULL, c("x", "y")))
  checkIdentical(x-m, y)
}

test.xts_minus_matrix2d_only_colnames <- function() {
  m <- matrix(1:6, 3, 2, dimnames = list(NULL, c("x", "y")))
  x <- .xts(cbind(1:3, 4:6), 1:3, dimnames = list(NULL, c("x", "y")))
  y <- .xts(cbind(1:3, 4:6)*0L, 1:3, dimnames = list(NULL, c("x", "y")))
  checkIdentical(x-m, y)
}

test.xts_minus_matrix2d_only_rownames <- function() {
  m <- matrix(1:6, 3, 2, dimnames = list(format(.POSIXct(1:3)), NULL))
  x <- .xts(cbind(1:3, 4:6), 1:3)
  y <- .xts(cbind(1:3, 4:6)*0L, 1:3)
  checkIdentical(x-m, y)
}

test.xts_minus_matrix2d_no_dimnames <- function() {
  m <- matrix(1:6, 3, 2)
  x <- .xts(m, 1:3)
  y <- .xts(m*0L, 1:3)
  checkIdentical(x-m, y)
}

### 1-column objects
test.xts_minus_matrix1d_dimnames <- function() {
  m <- matrix(1:3, 3, 1, dimnames = list(format(.POSIXct(1:3)), "x"))
  x <- .xts(1:3, 1:3, dimnames = list(NULL, "x"))
  y <- .xts(1:3*0L, 1:3, dimnames = list(NULL, "x"))
  checkIdentical(x-m, y)
}

test.xts_minus_matrix1d_only_colnames <- function() {
  m <- matrix(1:3, 3, 1, dimnames = list(NULL, "x"))
  x <- .xts(m, 1:3)
  y <- .xts(m*0L, 1:3)
  checkIdentical(x-m, y)
}

test.xts_minus_matrix1d_only_rownames <- function() {
  m <- matrix(1:3, 3, 1, dimnames = list(format(.POSIXct(1:3)), NULL))
  x <- .xts(1:3, 1:3)
  y <- .xts(1:3*0L, 1:3)
  checkIdentical(x-m, y)
}

test.xts_minus_matrix1d_no_dimnames <- function() {
  m <- matrix(1:3, 3, 1)
  x <- .xts(m, 1:3)
  y <- .xts(m*0L, 1:3)
  checkIdentical(x-m, y)
}

### xts with dim, vector
test.xts2d_minus_vector_no_names <- function() {
  m <- cbind(1:3, 1:3)
  x <- .xts(m, 1:3)
  y <- .xts(m*0L, 1:3)
  checkIdentical(x-m[,1L], y)
  # add column names to xts objects
  colnames(x) <- colnames(y) <- c("x", "y")
  checkIdentical(x-m[,1L], y)
}

test.xts2d_minus_vector_names <- function() {
  m <- cbind(1:3, 1:3)
  x <- .xts(m, 1:3)
  y <- .xts(m*0L, 1:3)
  M <- setNames(m[,1], format(index(x)))
  checkIdentical(x-M, y)
  # add column names to xts objects
  colnames(x) <- colnames(y) <- c("x", "y")
  checkIdentical(x-M, y)
}

test.xts1d_minus_vector_no_names <- function() {
  m <- matrix(1:3, 3, 1)
  x <- .xts(m, 1:3)
  y <- .xts(m*0L, 1:3)
  checkIdentical(x-m[,1L], y)
  # add column names to xts objects
  colnames(x) <- colnames(y) <- "x"
  checkIdentical(x-m[,1L], y)
}

test.xts1d_minus_vector_names <- function() {
  m <- matrix(1:3, 3, 1)
  x <- .xts(m, 1:3)
  y <- .xts(m*0L, 1:3)
  M <- setNames(m[,1], format(index(x)))
  checkIdentical(x-M, y)
  # add column names to xts objects
  colnames(x) <- colnames(y) <- "x"
  checkIdentical(x-M, y)
}

### xts vector, matrix/vector
test.xts_vector_minus_matrix1d <- function() {
  rn <- format(.POSIXct(1:3))
  cn <- "x"
  x <- drop(.xts(1:3, 1:3))
  m <- matrix(1:3, 3, 1, dimnames = list(rn, cn))
  y <- .xts(1:3*0L, 1:3, dimnames = list(NULL, cn))

  # use checkEquals because attributes change order
  checkEquals(x-m, y)
  # test again with no rownames
  rownames(m) <- NULL
  checkEquals(x-m, y)
  # test again with no rownames or colnames
  colnames(m) <- colnames(y) <- NULL
  checkEquals(x-m, y)
  # test again with only colnames
  colnames(m) <- colnames(y) <- cn
  checkEquals(x-m, y)
}

test.xts_vector_minus_matrix2d <- function() {
  # FIXME:
  rn <- format(.POSIXct(1:3))
  cn <- c("x", "y")
  x <- drop(.xts(1:3, 1:3))
  m <- matrix(1:6, 3, 2, dimnames = list(rn, cn))
  y <- .xts(cbind(1:3*0L, 1:3-4:6), 1:3, dimnames = list(NULL, cn))

  # use checkEquals because attributes change order
  checkEquals(x-m, y)
  # test again with no rownames
  rownames(m) <- NULL
  checkEquals(x-m, y)
  # test again with no rownames or colnames
  colnames(m) <- colnames(y) <- NULL
  checkEquals(x-m, y)
  # test again with only colnames
  colnames(m) <- colnames(y) <- cn
  checkEquals(x-m, y)
}

test.xts_vector_minus_vector <- function() {
  # FIXME:
  m <- 1:3
  x <- drop(.xts(m, 1:3))
  y <- drop(.xts(m*0L, 1:3))

  checkIdentical(x-m, y)
  # add names to vector
  names(m) <- format(index(x))
  checkIdentical(x-m, y)
}
