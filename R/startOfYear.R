`startOfYear` <- function(from=1900,
                            to=2200,
                         origin=1970)
{
  .C('do_startofyear',
     from = as.integer(from),
     to   = as.integer(to),
     fromto=as.integer(from:to),
     origin=as.integer(origin), PACKAGE='xts'
    )$fromto
}
