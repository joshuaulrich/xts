test.attr_on_object_equal_to_attr_on_index <- function() {
  # ensure xts objects with index attributes attached are equal to
  # xts objects with index attributes on the index only
  attrOnObj <-
    structure(1:3, index = structure(1:3, tzone = "UTC", tclass = "Date"), 
              class = c("xts", "zoo"), dim = c(3L, 1L),
              .indexCLASS = "Date", .indexTZ = "UTC",
              tclass = "Date", tzone = "UTC",
              dimnames = list(NULL, "x"))
  attrOnIndex <-
    structure(1:3, index = structure(1:3, tzone = "UTC", tclass = "Date"), 
              class = c("xts", "zoo"), dim = c(3L, 1L),
              dimnames = list(NULL, "x"))
  checkTrue(all.equal(attrOnIndex, attrOnObj))
}
