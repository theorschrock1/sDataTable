get_measure_names=function(data){
  dt%dtype_nms%("continuous"&"!date")
}
get_identity_names=function(data){
  dt%dtype_nms%("identity"|"date")
}
make_melt_names=function(group_name,melt_expr){

  var_name=paste0(group_name," names")
  val_name=paste0(group_name)
if(val_name=="variable")val_name="values"
  args<-exprs_make_names(call_args(melt_expr))
  names(args)<-glue("[{var_name}:{names(args)}] {val_name}")
  tmp<-expr(MELT(group_name=.(!!!args)))
  names(tmp)[2]<-group_name
  tmp
}
expr_make_melt_names=function(expr){

  melt_exprs=expr_extract_call(expr,'MELT')
  if(len(melt_exprs)==1&&is.null(melt_exprs[[1]]))return(expr)
names(melt_exprs)=NULL
melt_exprs_inner<-lapply(melt_exprs,function(x){
  tmp=call_args(x)
  if(l(tmp)>1)g_stop("MELT expression args should contain a single list expr. Example: MELT(variable=.(x,y,z))")
  names(tmp)[names(tmp)==""]<-"variable"
  tmp
  }) %>% unlist(recursive = F)
melt_exprs_inner<-exprs_make_names(melt_exprs_inner)


  replace=map2(names(melt_exprs_inner),melt_exprs_inner,make_melt_names)
  replace<-lapply(replace,exprs)
  #unlist( replace)
  expr_find_replace_all(find=melt_exprs,replacement=replace,in_expr=expr,match.first=TRUE)
  #J=expr_find_replace_all(find=melt_exprs,replacement=unlist( replace),in_expr=expr,match.first=TRUE)
}
fgroup=function(f,by,env=caller_env(),totals=FALSE){
  func<-f_lhs_nms(f)
  var<-f_rhs_nms(f)
  fn=expr(f(value))
  fn[[1]]=sym( func)
  varsyms=syms(var)
  by<-exprs_make_names(call_args(enexpr(by)))

  byparent<-by%nget%'variable'
  names(varsyms)<-var
  if(totals){
    expr_eval({
      tmp=.SD[,.(!!!varsyms,!!!byparent)]
      mtmp=melt(tmp,measure.vars= !!var)
      out=expr_eval(nested_totals(
        mtmp,,j=!!fn,
        by = .(!!!by)
      ))

    },env=env)
    return(out[[2]])
  }
  expr_eval({
    tmp=.SD[,.(!!!varsyms,!!!byparent)]
    mtmp=melt(tmp,measure.vars= !!var)
    out<-mtmp[,!!fn,by=.(!!!by)]
    out[[2]]
  },env=env)

}
set_measures=function(data){

}
setflat=function(dt,measure.names=NULL){
  setattr(dt,'class',c('flat',class(dt)))
  if(!is.null(measure.names))setattr(dt,'measures',measure.names)
}
setcasted=function(dt){
  setattr(dt,'class',c('casted',class(dt)))
}

build_group_fn=function(x){
  look<-expr_extract_call(x,'GROUP',skip_first = F)

  if(len(look)==0|(len(look)==1&&is.null(look[[1]])))return(x)
  if(is.null(names(look))){
    names(look)=rep('variable',l(look))
  }
  names(look)[names(look)==""]<-'variable'
  replace=map2( look,names(look),function(x,name){
    tmp=expr(GROUP(variable=.(!!!call_args(x))))
    names(tmp)[2]<-name
    tmp
    })
  expr_find_replace_all( look,replace,x)
}
#' @export
melt_cast=function(data,measure.vars,id.vars,value.name){

  mdt=melt.data.table( data,measure.vars =measure.vars,id.vars=id.vars,value.name = value.name)
  mdt[,`variable (#)`:=as.numeric(variable)]
  vnames=str_extract_all( mdt$variable[1],any_inside("\\[","\\:"))[[1]]
  mdt[,c( vnames):=tstr_extract(variable,any_inside("\\:","\\]"))]

  if('variable'%nin%vnames)mdt[,variable:=NULL]

  setkeyv(mdt,c(id.vars, 'variable (#)'))
  mdt
}
#' @export
melt.casted=function(data){
  #id_groups=get_casted_identity_groups(data)
  dtmp=copy(data)

  set_casted_names( dtmp)
  measures=get_casted_measure_groups( dtmp)
  id.vars<-get_uncasted_vars(dtmp)
  vnames<-names(measures)

  out<-joinDTlist(map2( measures, vnames,melt_cast,data=dtmp,id.vars=  id.vars),all=T)
  setcolorder(out,c('variable (#)',names(out)%NIN%c(vnames,'variable (#)'),vnames))

  cf<-attr(data,"casted_factors")
  if(!len0(cf)){
    out[,c(names(cf)):=map2(.SD,cf,function(x,y)factor(x,levels = levels(y),ordered=is.ordered(y))),.SDcols=names(cf)]
  }
  setflat(out, names(measures))
  setkeyv(out,names(out)%NIN%c(vnames,'variable (#)'))
  out[,`variable (#)`:=NULL]
  out[]
}
get_casted_measures=function(data){
 names(data)[sapply(data,function(x)is(x,"casted"))]
}
get_uncasted_vars=function(data){
  names(data)[sapply(data,function(x)!is(x,"casted"))]
}
set_casted_names=function(data){
  cast_names= sapply(data,attr,"casted_name") %>% unlist()
  old<-names(cast_names)
  names(cast_names)<-NULL
  setnames(data,old,cast_names)
}
get_casted_measure_groups=function(data){
 dtmp= data%get%get_casted_measures(data)
  #out2= sapply( dtmp,attr,"measure") %>% unlist()
  cast_name= sapply( dtmp,attr,"casted_name") %>% unlist()
  measure<-str_extract(  cast_name,any_inside("\\] ","$"))
  mnames<- unique( measure)
  out<-lapply(mnames,function(x)cast_name[ measure==x])
  names(out)=mnames
  out
}
permute=function(data,formula){
  assert(
    check_class(dt,'casted'),
    check_class(dt,'flattened'))

  assert_formula(formula)
  if(is(data,"casted")){
    dt<-melt(data)

  }
  measures= attr(dt,'measures')
  agg_funcs=syms(rep("na_fill",l(measures)))
  tmp = expr(dcast(dt, !!formula, value.var = !!measures,sep='<<>>',drop = FALSE,fun.aggregate=!!agg_funcs)) %>% eval()
  rename_casted_vars(dt=tmp,by_names = expr_find_names(formula),j_names=measures,from_data=data)
}

rename_casted_vars=function(dt,by_names,j_names,from_data,formula){

  oldn=names(dt)%NIN%by_names
  interaction_vars=by_names%NIN%names(dt)

  if(l(j_names)==1){

    setnames(dt,oldn,glue("{j_names}<<>>{oldn}"))
    oldn= glue("{j_names}<<>>{oldn}")
    #setnames(dt,oldn,glue('[{interaction_vars}:{oldn}] {j_names}'))
  }
  newn<-str_split(oldn,pattern = '\\<\\<\\>\\>')  %>%
    sapply(function(x){
      interaction_values<-seq(from=(l(x)+1-l( interaction_vars)),to=l(x),by=1)
      glue_collapse(c(paste0("[",  interaction_vars,":",x[interaction_values],"]")," ",x[1]),sep="")
    }

    )

  newn=str_remove(newn,'^zero_fill // ') %>% str_remove('^na_fill // ')


  setnames(dt,oldn,newn)

  measure<-str_extract(newn,any_inside("\\] ","$"))
  variable<-str_extract_all(newn,any_inside("\\[","\\]"))

  variable<-lapply(variable,function(x){
    x<-str_split(x,":")
    x2=lapply( x,function(x)x[2])
    x1=lapply( x,function(x)x[1]) %>% unlist()
    names(x2)<-x1
    x2
  })

  dt[,c(newn):=map2(.SD,measure,setattr,name="measure"),.SDcols=newn]
  dt[,c(newn):=map2(.SD,newn,setattr,name="casted_name"),.SDcols=newn]
  dt[,c(newn):=map2(.SD,lapply(.SD,function(x)c("casted",class(x))),setattr,name="class"),.SDcols=newn]
  dt[,c(newn):=map2(.SD,variable,setattr,name="identity"),.SDcols=newn]

  setattr(dt,'class',c("casted",class(dt)))
  lhs=expr_find_names(f_lhs(formula))
  rhs=expr_find_names(f_rhs(formula))
  setattr(dt,'formula',list(lhs=lhs,rhs=rhs))

  interaction_vars=interaction_vars%IN%names(from_data)
  if(nlen0(interaction_vars)){
  factorvars=interaction_vars[from_data[,sapply(.SD,is.factor),.SDcols=interaction_vars]]
  if(!len0(factorvars)){
    setattr(dt,"casted_factors",lapply(from_data[,.SD,.SDcols=factorvars],unique))
  }
  }
  dt
}
get_casted_identity_groups=function(data){
  dtmp= data%get%get_casted_measures(data)
  out2= lapply(dtmp,attr,"identity")
  names(out2)<-NULL
  out2=out2[!sapply(out2,is.null)]
  out2=unique(as.data.table(stack_lists(out2)))
  out2[,variable:=factor(1:nrow(out2))]
  out2
}
nested_totals=function(data,i,j,by){
  make_factors=function(x){
    if(is.factor(x))return(x)
    factor(x,ordered = TRUE)
  }
  data<-copy(data)
  by<-enexpr(by)
  i<-enexpr(i)
  j<-enexpr(j)
  if(!is_missing(i))
    data=expr_eval(data[!!i])

  f=by
  if(is.character(by)){
    f=.~.
    f_lhs_nms( f)<-by
  }
  if(is_call(by,'c')){
    f=.~.
    f_lhs_nms( f)<-eval(by)
  }
  if(is_list_call(by)|is_call(by,'+')){
    f=.~.
    f_lhs_nms(f)=expr_find_names(by)
  }
  if(!is_formula(f)){
    g_stop("Invalid by statement: {expr_to_str(by)}).  'by' must be a formula, list of names, or a character vector of names")
  }
  fnms<-f_nms(f)
  fsyms<-syms(fnms)
  data[,c(fnms):=lapply(.SD,make_factors),.SDcols=fnms]
  dt=expr_eval(groupingsets(data, j = !!j, by = fnms,sets = build_sets(f), id=TRUE))
  expr_glue('dt[is.na({fsyms}),{fsyms}:="Total"]') %>% exprs_eval()
  dt[,grouping:=NULL]
  setkeyv(dt,fnms)
  dt
}
build_sets=function(f){
  look<-expand.grid(accumulate(c(NA_character_,f_lhs_nms(f)),c),accumulate(c(NA_character_,f_rhs_nms(f)),c))
  map2(look$Var1,look$Var2,function(x,y)c(x,y)[!is.na(c(x,y))])
}
td_col=function(x,y){

  if(grepl("\\<td",x[1])){
    paste0(ROUND(x),td(ROUND(y)))
  }else{
    paste0(td(ROUND(x)),td(ROUND(y)))
    }
  }

tbody<-function(inner='',...){
  na<-list(...)
  extra<-glue_collapse(glue('{names(na)}:{na};'),' ')%x%''

  glue('<tbody style="{extra}">
{inner}
</tbody>')
}
table_html=function(x){
  glue("<table>{x}</table>")
}
ROUND<-function(x,...){
  ex <- eval_tidy(enquo(x))
  UseMethod('ROUND',ex)
}
ROUND.default<-function(x,...){
  assert_dtype(x,dtype=c("continuous","identity"),.vname=enexpr(x),.fname='ROUND')
  round(x,...)
}
ROUND.character<-function(x,...){
x
}
ROUND.factor<-function(x,...){
as.character(x)
}
table_cell=function(x,...)UseMethod("table_cell")
table_cell.default=function(x,...){
  td(x,class='default',...)
}
table_cell.row_names=function(x,...){
  td(x,class='rownames',...)
}
table_cell.row_groups=function(x,...){
length(x)
  # c(td(x,class='rowgroup',rowspan=length(x),...)[1],
  #   rep("",length(x)-1))
    }





