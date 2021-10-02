#' @export
nrow=function(x) UseMethod('nrow')
#' @export
nrow.default=function(x)base::nrow(x)
#' @export
nrow.DataTable=function(x) nrow(x$data)
#' @export
ncol=function(x) UseMethod('ncol')
#' @export
ncol.default=function(x)  base::ncol(x)
#' @export
ncol.DataTable=function(x) ncol(x$data)
#' @export
`[.DataTable` <- function(self,i,j,by,...,on=NULL) {

      I.J.BY<-enexprs(i,j,by)
      i_expr=I.J.BY[[1]]


      if(!is_missing( i_expr)&&
         !is_call_logical_test(i_expr)&&
         !expr_has_named_bracket(i_expr,name='.D',grep=TRUE,skip_first = F)&&
         (is_DataTableList(i)|is_DataTable(i)))
        return(self$join(i,on=on))
      if(!is_missing( i_expr)&&expr_has_named_bracket(i_expr,name='.D',grep=TRUE,skip_first = F)){
        snames=names(self)
       dlist<- expr_extract_named_bracket(i_expr,name='.D',grep=TRUE,skip_first = F,fully_chained = T)
       if(is(dlist,'call'))
         dlist<-list(dlist)
       joinList<-lapply(dlist,function(x){
         eval(expr_find_replace(expr(.D),expr(self),in_expr = x))

         })
       self<-self$join(joinList,on=on)
       if(is_call_logical_test(i_expr)){
       replacement<-lapply(joinList,function(x)sym(names(x)[length(names(x))]))
       I.J.BY[[1]]<-expr_find_replace_all(find=  dlist,replacement =replacement,i_expr)
       }
       if(is_call(i_expr,"[")){
         newnames<-names(self)
         fm=newnames%nin%snames
         fm=fm[ fm%starts_with%'filter_var_']

         if(nlen0(fm)){
           self$delete(fm)
         }
         I.J.BY[[1]]<-missing_arg()
       }
      }

      expr_eval(self$DT(!!!I.J.BY,...))
}
#' @export
`[[.DataTable` <- function(self,i) {
  self$data[[i]]
}
#' @export
print=base::print
#' @export
print.DataTable <- function(x) {
  x$print
  invisible(x)
}
#' @export
print.dynDataTable<-function(x){
  x$print_data
  invisible(x)
}


#' @export
names.DataTable<-function(x){
  x$names
}

