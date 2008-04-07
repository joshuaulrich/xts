`nseconds` <-
function(x) {
  length(endpoints(x,on='seconds'))-1
}
`nminutes` <-
function(x) {
  length(endpoints(x,on='minutes'))-1
}
`nhours` <-
function(x) {
  length(endpoints(x,on='hours'))-1
}
`ndays` <-
function(x) {
  length(endpoints(x,on='days'))-1
}
`nweeks` <-
function(x) {
  length(endpoints(x,on='weeks'))-1
}
`nmonths` <-
function(x) {
  length(endpoints(x,on='months'))-1
}
`nquarters` <-
function(x) {
  length(endpoints(x,on='quarters'))-1
}
`nyears` <-
function(x) {
  length(endpoints(x,on='years'))-1
}
