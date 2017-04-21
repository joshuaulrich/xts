# i = missing, j = NA, object has column names
# See #181
test.i_missing_j_NA_has_colnames <- function() {
  iina <- .xts(matrix(NA_integer_, 5, 2), 1:5)
  idna <- .xts(matrix(NA_integer_, 5, 2), 1.0 * 1:5)
  dina <- .xts(matrix(NA_real_, 5, 2), 1:5)
  ddna <- .xts(matrix(NA_real_, 5, 2), 1.0 * 1:5)
  colnames(iina) <- colnames(idna) <-
  colnames(dina) <- colnames(ddna) <- rep(NA_character_, 2)

  # int data, int index
  ii <- .xts(matrix(1:10, 5, 2), 1:5)
  colnames(ii) <- c("a", "b")
  checkIdentical(ii[, NA], iina)
  checkIdentical(ii[, 1][, NA], iina[, 1])

  # int data, dbl index
  id <- .xts(matrix(1:10, 5, 2), 1.0 * 1:5)
  colnames(id) <- c("a", "b")
  checkIdentical(id[, NA], idna)
  checkIdentical(id[, 1][, NA], idna[, 1])

  # dbl data, int index
  di <- .xts(1.0 * matrix(1:10, 5, 2), 1:5)
  colnames(di) <- c("a", "b")
  checkIdentical(di[, NA], dina)
  checkIdentical(di[, 1][, NA], dina[, 1])

  # dbl data, dbl index
  dd <- .xts(1.0 * matrix(1:10, 5, 2), 1.0 * 1:5)
  colnames(dd) <- c("a", "b")
  checkIdentical(dd[, NA], ddna)
  checkIdentical(dd[, 1][, NA], ddna[, 1])
}

# i = missing, j = NA, object does not have column names
# See #97
test.i_missing_j_NA_no_colnames <- function() {
  iina <- .xts(matrix(NA_integer_, 5, 2), 1:5)
  idna <- .xts(matrix(NA_integer_, 5, 2), 1.0 * 1:5)
  dina <- .xts(matrix(NA_real_, 5, 2), 1:5)
  ddna <- .xts(matrix(NA_real_, 5, 2), 1.0 * 1:5)

  # int data, int index
  ii <- .xts(matrix(1:10, 5, 2), 1:5)
  checkIdentical(ii[, NA], iina)
  checkIdentical(ii[, 1][, NA], iina[, 1])

  # int data, dbl index
  id <- .xts(matrix(1:10, 5, 2), 1.0 * 1:5)
  checkIdentical(id[, NA], idna)
  checkIdentical(id[, 1][, NA], idna[, 1])

  # dbl data, int index
  di <- .xts(1.0 * matrix(1:10, 5, 2), 1:5)
  checkIdentical(di[, NA], dina)
  checkIdentical(di[, 1][, NA], dina[, 1])

  # dbl data, dbl index
  dd <- .xts(1.0 * matrix(1:10, 5, 2), 1.0 * 1:5)
  checkIdentical(dd[, NA], ddna)
  checkIdentical(dd[, 1][, NA], ddna[, 1])
}

# i = integer, j = NA, object has column names
# See #97
test.i_missing_j_NA_has_colnames <- function() {
  iina <- .xts(matrix(NA_integer_, 5, 2), 1:5)
  idna <- .xts(matrix(NA_integer_, 5, 2), 1.0 * 1:5)
  dina <- .xts(matrix(NA_real_, 5, 2), 1:5)
  ddna <- .xts(matrix(NA_real_, 5, 2), 1.0 * 1:5)
  colnames(iina) <- colnames(idna) <-
  colnames(dina) <- colnames(ddna) <- rep(NA_character_, 2)

  i <- 1:3

  # int data, int index
  ii <- .xts(matrix(1:10, 5, 2), 1:5)
  colnames(ii) <- c("a", "b")
  checkIdentical(ii[i, NA], iina[i,])
  checkIdentical(ii[i, 1][, NA], iina[i, 1])

  # int data, dbl index
  id <- .xts(matrix(1:10, 5, 2), 1.0 * 1:5)
  colnames(id) <- c("a", "b")
  checkIdentical(id[i, NA], idna[i,])
  checkIdentical(id[i, 1][, NA], idna[i, 1])

  # dbl data, int index
  di <- .xts(1.0 * matrix(1:10, 5, 2), 1:5)
  colnames(di) <- c("a", "b")
  checkIdentical(di[i, NA], dina[i,])
  checkIdentical(di[i, 1][, NA], dina[i, 1])

  # dbl data, dbl index
  dd <- .xts(1.0 * matrix(1:10, 5, 2), 1.0 * 1:5)
  colnames(dd) <- c("a", "b")
  checkIdentical(dd[i, NA], ddna[i,])
  checkIdentical(dd[i, 1][, NA], ddna[i, 1])
}

# i = integer, j = NA, object does not have column names
# See #97
test.i_integer_j_NA_no_colnames <- function() {
  iina <- .xts(matrix(NA_integer_, 5, 2), 1:5)
  idna <- .xts(matrix(NA_integer_, 5, 2), 1.0 * 1:5)
  dina <- .xts(matrix(NA_real_, 5, 2), 1:5)
  ddna <- .xts(matrix(NA_real_, 5, 2), 1.0 * 1:5)

  i <- 1:3

  # int data, int index
  ii <- .xts(matrix(1:10, 5, 2), 1:5)
  checkIdentical(ii[i, NA], iina[i,])
  checkIdentical(ii[i, 1][, NA], iina[i, 1])

  # int data, dbl index
  id <- .xts(matrix(1:10, 5, 2), 1.0 * 1:5)
  checkIdentical(id[i, NA], idna[i,])
  checkIdentical(id[i, 1][, NA], idna[i, 1])

  # dbl data, int index
  di <- .xts(1.0 * matrix(1:10, 5, 2), 1:5)
  checkIdentical(di[i, NA], dina[i,])
  checkIdentical(di[i, 1][, NA], dina[i, 1])

  # dbl data, dbl index
  dd <- .xts(1.0 * matrix(1:10, 5, 2), 1.0 * 1:5)
  checkIdentical(dd[i, NA], ddna[i,])
  checkIdentical(dd[i, 1][, NA], ddna[i, 1])
}
