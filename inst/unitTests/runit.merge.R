test.merge_empty_xts_with_2_scalars <- function() {
  m1 <- merge(xts(), 1, 1)
  m2 <- merge(merge(xts(), 1), 1)
  checkIdentical(m1, m2)
}

