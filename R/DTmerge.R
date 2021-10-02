#' Merge two data.tables by like columns
#'
#' @param x a data.table
#' @param y a data.table
#'
#' @return a merged data.table
DTmerge<-function(x,y,all=F,all.x=F,all.y=F){
  assert_data_table(x)
  assert_data_table(y)
  likeCols<-names(x)[names(x)%in%names(y)]

  if(all==T){
    merge(x,y,by=likeCols,all=T)
  } else  if(all.x==T){
    merge(x,y,by=likeCols,all.x=T)
  }else if(all.y==T) {
    merge(x,y,by=likeCols,all.y=T)
  }else {
    merge(x,y,by=likeCols)
  }
}
