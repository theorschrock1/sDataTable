#' @export
data_attr=function(x,...)UseMethod("data_attr")
#' @export
data_attr.default=function(x){
  out<-list(dtype=data_type(x,tail=T))
  class(out)<-"data_attributes"
  out
}
#' @export
data_attr.units=function(x){
  out<-list(dtype=data_type(x,tail=T),unit=get_units(x))
  class(out)<-"data_attributes"
  out
}
#' @export
data_attr.factor=function(x){
  out<-list(dtype=data_type(x,tail=T),levels=unique(x))
  class(out)<-"data_attributes"
  out
}
#' @export
data_attr.data.table=function(x,names){
  out<-lapply(x,attr,'vars')
  out<-out[!sapply(out,is.null)]
  out
}
#' @export
`data_attr<-`=function(x,value)UseMethod("data_attr<-")
#' @export
`data_attr<-.default`=function(x,value)g_stop("cannot assign data_attributes to object of class '{class(x)%sep%','}'")
#' @export
`data_attr<-.data.table`<-function(x,value){
  vnames=names(value)
  if(len0(value))
    return(invisible(x))
  if(!isTRUE(check_subset(vnames,names(x)))){
    nomatch<-vnames%NIN%names(x)%sep%","
    g_stop("names in data_attributes list ['{nomatch}'] do not match names in data.table")
  }
  x[,c(vnames):=map2(.SD,value,setattr,name="vars"),.SDcols=vnames]

}
#' @export
group_data_attrs=function(data_attrs,names_only=T){

  att<-unlist(lapply(data_attrs,function(x)x$dtype))
  if(names_only){
    out=list(
      date=names(data_attrs[att%in%c("date","time")]),
      numeric=names(data_attrs[att%in%c("float","integer","units")]),
      factor=names(data_attrs[att%in%c("ordered","factor")]),
      character=names(data_attrs[att%in%c("character")])
    )}else{
      out=list(
        date=data_attrs[att%in%c("date","time")],
        numeric=data_attrs[att%in%c("float","integer","units")],
        factor=data_attrs[att%in%c("ordered","factor")],
        character=data_attrs[att%in%c("character")]
      )
    }
  out[!sapply(out,len0)]

}
#' @export
.flatten=function(dt,melt_cols,id.vars=NULL,vname=NULL){

dt<-copy(dt)
 if(is.list(melt_cols))melt_cols<-unlist(melt_cols,use.names = F)
 if(any(duplicated(melt_cols)))g_stop("Duplicate variable names found [{unique(duplicated[duplicated(melt_cols)])%sep%','}]")
.SDcols=melt_cols
if(is.null(id.vars)){
id.vars=names(df)%NIN%.SDcols
}
data_attrs=lapply(dt[,.SD,.SDcols=.SDcols],data_attr)
groups<-group_data_attrs(data_attrs)
groups_attr<-group_data_attrs(data_attrs,names_only = FALSE)


names(groups_attr)<-paste(names(groups_attr),"names")
unit=NULL
if(!is.null(groups_attr$`numeric variable`)){
unit_check=groups_attr$`numeric variable`%>% stack_lists()
if(all_equal(unit_check$dtype)&&unique(unit_check$dtype)=="units"&&
   all_units_compatible(unit_check$unit)){
  unit=unit_check$unit[1]
}
}
dt[,c(.SDcols):=lapply(.SD,set_melt_var,unit=unit),.SDcols=.SDcols]

vnamest <- data.table(
  variable.name= paste(vname%or%names(groups), "value"),
  value.name=  paste(vname%or%names(groups), "names")
) %>%
  t() %>%
  as.data.table() %>%
  as.list()
value.name= paste(vname%or%names(groups), "value")
variable.name=  paste(vname%or%names(groups), "names")
dt[,rowid:=1:nrow(dt)]
.flatten2=function(measures,vnames,dt,id.vars){
  out<-expr_eval(
    melt(
      dt,
      id.vars = !!id.vars,
      measure.vars = !!measures,
      value.name = !!vnames[1],
      variable.name = !!vnames[2]
    ))
  expr_eval(out[,`:=`(variable,as.numeric(!!sym(vnames[2])))])
  out
}

out<-reduce(map2(groups,vnamest,.flatten2,dt=dt,id.vars=c(id.vars,'rowid')),DTmerge,all=T)
names(groups_attr)<-variable.name
out[,c(names(groups_attr)):=map2(.SD,groups_attr,function(x,attr)setattr(x,"vars",attr)),.SDcols=names(groups_attr)]
if(!is.null(groups_attr$factor)){
  fweights<-lapply(groups_attr$factor,function(x)data.table(factor_value=levels(x$levels),numeric_value=seq_along(levels(x$levels))))
  names(fweights)

  fweigths=map2(fweights,names(fweights),function(x,y)x[,variable:=y]) %>% rbindlist()
exprs_eval(expr_glue('out[,{syms(value.name)}:=setattr({syms(value.name)},"class",c("combined_factor",class({syms(value.name)})))]'))
exprs_eval(expr_glue("out[,{syms(value.name)}:=setattr({syms(value.name)},'factor_wiegths',  fweights)]"))
}
if(!is.null(unit)&is.null(vname))
  setnames(out,
           c("numeric variable", "numeric value"),
           c(paste(unit, 'variable'),paste(unit, 'value')))
out
}
#' @export
flatten=function(dt,melt_cols,id.vars=NULL,mixed_units=TRUE,other.measures=NULL){
  if(is.list(melt_cols)){
    assert_names(names(melt_cols))
    if(is.null(id.vars))id.vars<-names(dt)%NIN%unlist(melt_cols)
    out=map2(melt_cols,names(melt_cols),.flatten,dt=dt,id.vars=id.vars)%>%
    reduce(DTmerge,all=T)

    if(!is.null(other.measures)){
      om= dt[,..other.measures]
      om[,variable:=1]
      om[,rowid:=1:nrow(dt)]
      setkey(om,variable,rowid)
      setkey(out,variable,rowid)
     out=om[out]
    }
    setnames(out,"variable","variable.names")
    DTnull(out,c("rowid"))
    return(out)

  }
  if(mixed_units){
    out<-.flatten(dt,melt_cols,id.vars)
    set_names(out,"variable","variable.names")
    setcolorder(out,neworder = "rowid")
    return(out)
    }
  tmp<-dt[,.SD,.SDcols=melt_cols]
  compatible_units=get_compatible_unit_vars(tmp)
  compatible_units$other=melt_cols%NIN%unlist(compatible_units)%or%NULL
  if(is.null(id.vars)){
    id.vars=names(dt)%NIN%unlist(compatible_units)
  }
  out=lapply(compatible_units,.flatten,dt=dt,id.vars=id.vars) %>% reduce(DTmerge,all=T)
  setnames(out,"variable","variable.names")
  DTnull(out,c("rowid"))
   out
}

#' @export
set_melt_var=function(x,...)UseMethod("set_melt_var")
#' @export
set_melt_var.default=function(x,...)x
#' @export
set_melt_var.units=function(x,unit=NULL){
  if(is.null(unit))
  return(as.numeric(x))
expr_eval(set_units(x,!!unit))
}
#' @export
set_melt_var.Date=function(x,...)as_datetime(x)
#' @export
set_melt_var.POSIXct=function(x,...)as_datetime(x)
#' @export
set_melt_var.integer=function(x,...)as.double(x)

