#' @export
d_range=function(x)UseMethod('d_range')
#' @export
d_range.factor=function(x){
  levels(x)
}
#' @export
d_range.character=function(x){
  sort(unique(x))
}
#' @export
d_range.character=function(x){
  sort(unique(x))
}
#' @export
d_range.logical=function(x){
 as.logical( range(x))
}



#' @export
d_range.default=function(x){
  range(x)
}
