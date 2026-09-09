#' As-of Join for xts objects
#'
#' @param x an xts object
#' @param y an xts object
#' @param ... additional arguments
#' @param join type of join (left, right, full)
#' @param return_side which side to return
#'
#' @export
as_of_join <- function(x, y, ..., join = c("full", "left", "right"), return_side = c(TRUE, TRUE)) {
  join <- match.arg(join)
  
  # The highly optimized C path for left joins
  if (join == "left") {
    idx <- .Call(C_as_of_indices, .index(x), .index(y))
    y_mat <- coredata(y)
    
    # Subset coredata of y based on matched indices. NA indices return NA rows.
    y_mapped <- y_mat[idx, , drop = FALSE]
    
    # Assign the column names of y
    colnames(y_mapped) <- colnames(y)
    
    # Create the mapped y with the index of x
    y_xts <- xts(y_mapped, index(x))
    
    # Combine them using merge.xts
    out <- merge.xts(x, y_xts, join = "inner", retside = return_side)
    return(out)
  } else if (join == "right") {
    # Right join is just a left join with arguments swapped
    idx <- .Call(C_as_of_indices, .index(y), .index(x))
    x_mat <- coredata(x)
    x_mapped <- x_mat[idx, , drop = FALSE]
    colnames(x_mapped) <- colnames(x)
    x_xts <- xts(x_mapped, index(y))
    out <- merge.xts(x_xts, y, join = "inner", retside = return_side)
    return(out)
  }
  
  # Fallback to crude implementation for full join
  out <- merge.xts(x, y, fill = na.locf, retside = return_side)
  out <- switch(join,
                full = out,
                left = out[index(x)],
                right = out[index(y)])
  return(out)
}
