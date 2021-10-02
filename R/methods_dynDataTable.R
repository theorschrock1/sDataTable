names.dynDataTable <-function(x){
  x$variable_names
}



#' @export
`[.dynDataTable` <- function(self,...) {
  dots_list<-enexprs(...)

  if(names(dots_list)[2]!=""){
    J=deparse(dots_list[[2]])
    name=names(dots_list)[2]
    replace_old=F
    if(name%in%names(self))replace_old=T
    I=dots_list[[1]]
    if(missing(I)){
      I=NULL
    }else{
      I<-deparse(I)
    }
    by=dots_list$by
    if(is_call(by)){
      by=as.list(by)[-1]
    }
    by<-as.character(by)

    exclude=dots_list$exclude_LOD
    if(is_call(exclude)){
      exclude=as.list(exclude)[-1]
    }

    if(is_integerish(exclude[[1]]))  exclude<-as.numeric(exclude)
    if(is_character(exclude[[1]]))  exclude<-as.character(exclude)
    if(is_symbol(exclude[[1]]))  exclude<-as.character(exclude)

    include=dots_list$include_LOD
    if(is_call(include)){
      include=as.list(include)[-1]
    }
    include<-as.character(include)

    # print(exclude)
    self$new_variable(name=name,
                      I=I,
                      J=J,
                      by=(by%or%NULL),
                      exclude_LOD=(exclude%or%NULL),
                      include_LOD=(include%or%NULL),
                      from_source = F,
                      replace_old=replace_old)


  }else{
    dots_list$exclude_LOD<-NULL
    dots_list$include_LOD<-NULL
    self$query2(...)
  }

}
#
# `[.dynDataTable ` <- function(x,...) {
#   dots_list= enexprs(...)
#   if(names(dots_list)[2]!=""){
#     J=deparse(dots_list[[2]])
#     name=names(dots_list)[2]
#     replace_old=F
#     if(name%in%names(x))replace_old=T
#     I=dots_list[[1]]
#     if(missing(I)){
#       I=NULL
#     }else{
#       I<-deparse(I)
#     }
#     by=dots_list$by
#     if(is_call(by)){
#       by=as.list(by)[-1]
#     }
#     by<-as.character(by)
#
#     exclude=dots_list$exclude_LOD
#     if(is_call(exclude)){
#       exclude=as.list(exclude)[-1]
#     }
#
#     if(is_integerish(exclude[[1]]))  exclude<-as.numeric(exclude)
#     if(is_character(exclude[[1]]))  exclude<-as.charater(exclude)
#     if(is_symbol(exclude[[1]]))  exclude<-as.character(exclude)
#
#     include=dots_list$include_LOD
#     if(is_call(include)){
#       include=as.list(include)[-1]
#     }
#     include<-as.character(include)
#
#     # print(exclude)
#     x$new_variable(name=name,
#                    I=I,
#                    J=J,
#                    by=(by%or%NULL),
#                    exclude_LOD=(exclude%or%NULL),
#                    include_LOD=(include%or%NULL),
#                    from_source = F,
#                    replace_old=replace_old)
#
#
#   }else{
#     dots_list$exclude_LOD<-NULL
#     dots_list$include_LOD<-NULL
#     x$query2(...)
#   }
#
# }
