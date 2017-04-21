# i = missing, j = NA, object has column names
# See #181
test.i_missing_j_NA_colnames <- function() {
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
