`isOrdered` <- function(x, increasing=TRUE, strictly=TRUE) {
  # x must be of type double or integer.  Checked in the C code.
  if(is.character(x))
    stop('character ordering unsupported')

  if(!is.numeric(x))
    x = as.numeric(x)

  .Call('do_is_ordered', 
        x = x,
        increasing = as.logical(increasing),
        strictly   = as.logical(strictly), PACKAGE='xts')
}
