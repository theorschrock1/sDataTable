replace_fixed_names_for_error=function(x){
  if(!is.call(x))return(x)
  nb<-expr_extract_named_bracket(x,'._SDcol',skip_first = F,grep=TRUE)
  if(!len0( nb)){
    replacement<-lapply(nb,function(x) as.character(x[[2]])%>% str_remove_all("\\._SDcol") %>% str_remove_all("\\._") %>% sym())
    for(i in 1:length(nb)){
      x=expr_find_replace(nb[[i]],replacement =replacement[[i]],x )
    }
  }
  call_modify(x, na.rm = zap())
}




.opsManager =  R6::R6Class(
  'opsManager',
  public = list(
    ops=list(),
    proto=NULL,
    initialize = function(proto) {
      self$proto=proto
      self$ops[["+"]]<-list()
      self$ops[["-"]]<-list()
      self$ops[["*"]]<-list()
      self$ops[["/"]]<-list()
      self$ops[["%%"]]<-list()
      self$ops[["^"]]<-list()
      self$ops[["&"]]<-list()
      self$ops[["|"]]<-list()
      self$ops[["%/%"]]<-list()
      self$ops[['==']]=list()
      self$ops[['!=']]=list()
      self$ops[['<']]=list()
      self$ops[['<=']]=list()
      self$ops[['>=']]=list()
      self$ops[['>']]=list()
    },
    new_old_op=function(op,e1,e2,out,exclude=NULL,bi_dir=TRUE){
      assert_subset(str_remove(e1,"\\$"),choices=names(self$proto))
      assert_subset(str_remove(e2,"\\$"),choices=names(self$proto))
      assert_subset(out,choices=names(self$proto))
      assert_subset(op,choices=c(names(self$ops),'all',"compare"))
      if('all'%in%op)op=names(self$ops)
      if("compare"%in%op)op=c(op[op!='compare'],c('==','!=','<','<=','>=','>'))
      if(!is.null(exclude))
        op=op[op%nin%exclude]
      de1= data_type(self$proto[[str_remove(e1,"\\$")]])
      de2=data_type(self$proto[[str_remove(e2,"\\$")]])
      out=expr(self$proto[[!!out]])
      comp=c('==','!=','<','<=','>=','>')
      for(i in op){
        ret=out
        if(i %in% comp)ret<-expr(self$proto$logical)

        new_op=expr(if(is_data_type(x,!!e1)&is_data_type(y,!!e2))return(!! ret))

        self$ops[[i]][[paste0(str_remove(e1,"\\$"),'_',str_remove(e2,"\\$"))]]<-  new_op
        if(bi_dir){
          new_op=expr(if(is_data_type(x,!!e2)&is_data_type(y,!!e1))return(!! ret))

          self$ops[[i]][[paste0(str_remove(e2,"\\$"),'_',str_remove(e1,"\\$"))]]<-  new_op
        }
      }
      invisible(self)
    },
    new_op2=function(op,e1,e2,out='default',exclude=NULL,bi_dir=TRUE){
      assert_subset(str_remove(e1,"\\$"),choices=names(self$proto))
      assert_subset(str_remove(e2,"\\$"),choices=names(self$proto))
      #assert_subset(out,choices=names(self$proto))
      assert_subset(op,choices=c(names(self$ops),'all',"compare"))
      if('all'%in%op)op=names(self$ops)[1:15]
      if("compare"%in%op)op=c(op[op!='compare'],c('==','!=','<','<=','>=','>'))
      if(!is.null(exclude))
        op=op[op%nin%exclude]
      de1= data_type(self$proto[[str_remove(e1,"\\$")]])
      de2=data_type(self$proto[[str_remove(e2,"\\$")]])

      comp=c('==','!=','<','<=','>=','>')
      for(i in op){
        ret=out


        new_op=expr(if(is_data_type(x,!!e1)&is_data_type(y,!!e2))return(!! ret))

        self$ops[[i]][[paste0(str_remove(e1,"\\$"),'_',str_remove(e2,"\\$"))]]<-  new_op
        if(bi_dir){
          new_op=expr(if(is_data_type(x,!!e2)&is_data_type(y,!!e1))return(!! ret))

          self$ops[[i]][[paste0(str_remove(e2,"\\$"),'_',str_remove(e1,"\\$"))]]<-  new_op
        }
      }
      invisible(self)
    },
    new_op1=function(op,...,class_name='default',exclude=NULL){
       dots<-list(...)
       args<-c(...)

      names(dots)<-paste0("v",1:l(dots))
      assert_subset(str_remove(args,"\\$"),choices=names(self$proto))

      #assert_subset(out,choices=names(self$proto))
      assert_subset(op,choices=c(names(self$ops),'all',"compare"))
      if('all'%in%op)op=names(self$ops)
      if("compare"%in%op)op=c(op[op!='compare'],c('==','!=','<','<=','>=','>'))
      if(!is.null(exclude))
        op=op[op%nin%exclude]
      ##de1= data_type(self$proto[[str_remove(e1,"\\$")]])
      ##de2=data_type(self$proto[[str_remove(e2,"\\$")]])
      dnames<-syms(names(dots))
      if_statement<-map2(dnames,dots,function(xname,class)expr(is_data_type(!!xname,!!class)))
      if_statement<-expr_reduce(if_statement,`&`)
      comp=c('==','!=','<','<=','>=','>')
      for(i in op){
        ret=class_name


        new_op=expr(if(!!if_statement)return(!! ret))

        self$ops[[i]][[paste(str_remove(args,"\\$"),collapse="_")]]<-  new_op

      }
      invisible(self)
    },
    new_op=function(op,...,class_name,exclude=NULL,bi_dir=TRUE){
      if(all(op%in%c(names(self$ops)[1:15],"all","compare"))){
        self$new_op2(op,...,out=class_name,exclude=exclude,bi_dir=bi_dir)
      }else{
        self$new_op1(op,...,class_name=class_name,exclude=exclude)
      }

    },
    build_fn=function(op){

      start=expr_extract_lines({
        e1=enexpr(e1)
        e2=enexpr(e2)
        x=eval(e1,envir=env)
        y=eval(e2,envir=env)
        })
      error_part=expr_extract_lines({
      e1=replace_fixed_names_for_error(e1)
      e2=replace_fixed_names_for_error(e2)
      typex<-data_type(x,tail=TRUE)
      typey<-data_type(y,tail=TRUE)
      if(typex=="units")
        typex=get_units(x)
      if(typey=="units")
        typey=get_units(y)

      })
      body=expr({!!!c(start,self$ops[[op]],error_part,
                      expr(g_stop("Incompatible operation:[ {rebuild_and_deparse(e1)} {op} {rebuild_and_deparse(e2)} ]
                                  '{typex}'{op}'{typey}' not supported.",silent=TRUE)))})
      args=pairlist2(e1=,e2=,env=caller_env())
      new_function(args,body)
    },
    build_fn2=function(op){

      start<-expr_extract_lines({
        vars<-enexprs(...)
        names1=paste0("x",1:l(vars))
        names2=paste0("v",1:l(vars))
        exprs_eval(expr_glue('eval(expr(c({names1%sep%","})%<-%vars))'))
        exprs_eval(expr_glue("{names2}=eval({names1},envir=env)"))
        vars<-lapply(vars,replace_fixed_names_for_error)
        stopOp<-glue('{op}({exprs_deparse(vars)%sep%","})')
      })
      error_part=expr_extract_lines({
        ufunc=function(x){
          typex=data_type(x,tail=TRUE)
          if(typex=="units")typex=get_units(x)
          typex
          }
        exprs_eval(expr_glue("vtypes<-unlist(lapply(list({names2%sep%','}),ufunc))%sep%','"))

      })
      body=expr({!!!c(start,self$ops[[op]],error_part,
                      expr(g_stop("Incompatible operation:[ {stopOp} ]
                              {op}('{vtypes}') not supported.",silent=TRUE)))})
      fn=function(...,env=caller_env()){}
      new_function(fn_fmls(fn),body)
    },
    .test_output=function(call,env=caller_env()){
      call=enexpr(call)
      assert_call(call)
      fn=self$build_fn(as_string(call[[1]]))
      x=call[[2]]

      y=call[[3]]
      out<-expr_eval(fn(!!x,!!y))
      if(is_error(out))return(out)
      data_type(expr_eval(fn(!!x,!!y)))
    },
    test_output=function(call,env=caller_env()){
      call=enexpr(call)
      assert_call(call)
      fn=self$build_fn(as_string(call[[1]]))
      x=call[[2]]

      y=call[[3]]
      out<-expr_eval(fn(!!x,!!y))
      out
    }
  ),
  private = list(
    .ops=c('+','-','*','/','%%','^','%/%','==','!=','<','<=','>=','>'),
    .logical_ops=c('&','|'),
    .compare=c('==','!=','<','<=','>=','>'),
    .agg=c('N',
           'N_missing',
           'Nth',
           'N_distinct',
           'N_duplicated',
           'first',
           'last',
           'IQR',
           'mad',
           'mean',
           'median',
           'max',
           'min',
           'sum',
           'sd',
           'var',
           'cor',
           'kurtosis',
           'skewness',
           'moment'),
    .math=c('abs','sign','sqrt',
            'floor', 'ceiling', 'trunc',
            'round', 'signif','exp', 'log', 'expm1', 'log1p',
            'cos', 'sin', 'tan',
            'cospi', 'sinpi', 'tanpi',
            'acos', 'asin', 'atan',
            'cosh', 'sinh', 'tanh',
            'acosh', 'asinh', 'atanh'),
    .table=c('cumsum', 'cumprod', 'cummax', 'cummin','diff')
  ),
  active = list(
    agg=function(value){
      if(missing(value)){

        return(private$.agg)

      }
      stop("agg is read only")
    },
    ops_names=function(value){
      if(missing(value)){

        return(private$.ops)

      }
      stop("ops is read only")
    },
    math=function(value){
      if(missing(value)){

        return(private$.math)

      }
      stop("math is read only")
    },
    table=function(value){
    if(missing(value)){
      return(private$.table)
    }
    stop("table is read only")
    },
    logical_ops=function(value){
  if(missing(value)){

    return(private$.logical_ops)

  }
  stop("logical_ops is read only")
},
    all_fn=function(value){
      if(missing(value)){

        return(c(self$table,self$math))

      }
      stop("all_fn is read only")
    }
  )
)

# self<-.opsManager$new(proto = proto)
# #self$new_op(op=c("diff"),x=c("date"),class_name='diff_date')
#
# self$new_op(op=c("+","-"),"duration",'date_time',class_name='duration_date_time')
# self$new_op(c("+","-"),"duration",'date$',class_name='duration_date_time')
# self$new_op(c("-","compare"),"date_time",'date_time',class_name='datetime')
# self$new_op(c("-","compare"),"date$",'date_time',class_name='date_datetime')
# self$new_op(c("-","compare"),"date$",'date$',class_name='date')
# self$new_op('all',"units",'units',class_name='units',exclude = '^')
# self$new_op(c("/","*","^"),"units",'integer',class_name='default',bi_dir=FALSE)
# self$new_op(c("/","*"),"integer",'units',class_name='default',bi_dir=FALSE)
# self$new_op(c("/","*"),"float$",'units',class_name='default')
# self$new_op('all',"float$",'integer',class_name='default')
# self$new_op('all',"integer",'integer',class_name='default')
# self$new_op(c("+","-"),"float$",'date$',class_name='float_date')
# self$new_op(c("+","-"),"integer",'date$',class_name='integer_date')
# self$new_op(c("+","-"),"float$",'date_time',class_name='float_datetime')
# self$new_op(c("+","-"),"integer",'date_time',class_name='integer_datetime')
#
# self$new_op('all',"float$",'float$',class_name = "default")
# self$new_op('compare',"ordered","ordered",class_name = "default")
# self$new_op(c("==","!="),"factor$","factor$",class_name = "default")
# self$new_op(c("==","!="),"logical","logical",class_name = "default")
# self$new_op(c("==","!="),"factor","character",class_name = "default")
# self$new_op(c('compare'),"character","character",class_name = "default")
# self$new_op(c('+'),"character","character",class_name="character")

assert_same_aggregation=function(x,y,.xname,.yname,.fname,silent=FALSE){

  aggtest<-is(x,"aggregate")==is(y,"aggregate")
  if(isTRUE(aggtest))return(invisible(aggtest))
  if(isFALSE(aggtest)){
    mess<-expr(ops(!!.xname,!!.yname,op=!!.fname)) %>%
      expr_replace_call_names("ops","rebuild_op") %>%
      eval()%>%
      expr_deparse()%sep%"\n"

    g_stop("Incompatible operation: '{mess}'\n Cannot combine aggregated and unaggregated measures",silent=silent)}
}
#' @export
arthi_op=function(op,x,y,...){
  UseMethod("arthi_op",op)
}
#' @export
arthi_op.default=function(op,x,y){
  v_collect()
  assert_same_aggregation(x,y,.xname=.x,.yname=.y,.fname=op)
  out=expr(.f(x,y))
  out[[1]]=sym(op)
  eval(out)
}
#' @export
arthi_op.duration_date_time=function(op,x,y){
  v_collect()
  assert_same_aggregation(x,y,.xname=.x,.yname=.y,.fname=op)
  if(is_data_type(x,"duration")){
    x<-set_units(x,'s') %>% drop_units()
    y<-as_datetime(y)
    }
  if(is_data_type(y,"duration")){
    y<-set_units(y,'s') %>% drop_units()
    x<-as_datetime(x)
    }
  as_datetime(arthi_op.default(op,x,y))
}
#' @export
arthi_op.units=function(op,x,y){

  v_collect()
  assert_same_aggregation(x,y,.xname=.x,.yname=.y,.fname=op)
  assert_unit_compatibility(x,y,.xname=.x,.yname=.y,.fname = op)

  arthi_op.default(op,x,y)
}
#' @export
arthi_op.datetime=function(op,x,y){

  v_collect()
  assert_same_aggregation(x,y,.xname=.x,.yname=.y,.fname=op)
  out=arthi_op.default(op,as_datetime(x),as_datetime(y))
  if(op=="-")out=as_units(out)
  out
}
#' @export
arthi_op.date=function(op,x,y){

  v_collect()
  assert_same_aggregation(x,y,.xname=.x,.yname=.y,.fname=op)
  out=arthi_op.default(op,x,y)
  if(op=="-")out=as_units(out)
  out
}
#' @export
arthi_op.date_datetime=function(op,x,y){
  v_collect()
  assert_same_aggregation(x,y,.xname=.x,.yname=.y,.fname=op)
  x<-as_datetime(x)
  y<-as_datetime(y)

  out=arthi_op.default(op,x,y)
  if(op=="-")out=as_units(out)
  out
}
#' @export
arthi_op.character=function(op,x,y){
  v_collect()
  assert_same_aggregation(x,y,.xname=.x,.yname=.y,.fname=op)
  switch(op,
         "+"=paste0(x,y),
         arthi_op.default(op,x,y)
  )
}
#' @export
arthi_op.float_date=function(op,x,y){
  v_collect()
  assert_same_aggregation(x,y,.xname=.x,.yname=.y,.fname=op)
  if(is(x,"Date")){
    x<-as_datetime(x)
    y<-set_units(y,"days")
  }
  if(is(y,"Date")){
    y<-as_datetime(y)
    x<-set_units(x,"days")
  }
  arthi_op.duration_date_time(op,x,y)

}
#' @export
arthi_op.integer_date=function(op,x,y){
  v_collect()
  assert_same_aggregation(x,y,.xname=.x,.yname=.y,.fname=op)
  if(is(x,"Date")){
    x<-as_datetime(x)
    y<-set_units(y,"days")
  }
  if(is(y,"Date")){
    y<-as_datetime(y)
    x<-set_units(x,"days")
  }

  arthi_op.duration_date_time(op,x,y)

}
#' @export
arthi_op.float_datetime=function(op,x,y){
    v_collect()
  assert_same_aggregation(x,y,.xname=.x,.yname=.y,.fname=op)
  if(is(x,"POSIXct")){
    y<-set_units(y,"days")
    x<-as_datetime(x)
  }
  if(is(y,"POSIXct")){
    x<-set_units(x,"days")
    y<-as_datetime(y)
  }
  arthi_op.duration_date_time(op,x,y)

}
#' @export
arthi_op.integer_datetime=function(op,x,y){
  v_collect()
  assert_same_aggregation(x,y,.xname=.x,.yname=.y,.fname=op)
  if(is(x,"POSIXct")){
    y<-set_units(y,"days")
    x<-as_datetime(x)
  }
  if(is(y,"POSIXct")){
    x<-set_units(x,"days")
    y<-as_datetime(y)
  }
  arthi_op.duration_date_time(op,x,y)
}
#' @export
ops=function(op,...){
  #vnames<-enexprs(...)
  if(op%in%c('+','-','*','/','%%','^','&','|','%/%','==','!=','<','<=','>=','>')){
    fn <- opsR6$build_fn(op)
  }else{
    fn<-opsR6$build_fn2(op)
  }
  pclass<-fn(...,env = caller_env())
  if(is_error(pclass))g_stop(pclass)
  class(op)<- pclass

  arthi_op(op,...)

}
#' @export
rebuild_op=function(x,y,op){
  x=enexpr(x)
  y=enexpr(y)
  if(expr_has_call_name(x,'rebuild_op',skip_first = FALSE)){
    xtmp=expr_extract_call(x,'rebuild_op',skip_first = FALSE)
    xreplace=eval(xtmp[[1]])
    x<-expr_find_replace(xtmp[[1]],xreplace,x)
    }
  if(expr_has_call_name(y,'rebuild_op',skip_first = FALSE)){
      ytmp=expr_extract_call(y,'rebuild_op',skip_first = FALSE)
      yreplace=eval(ytmp[[1]])
      y<-expr_find_replace(ytmp[[1]],yreplace,y)
      }



  out=expr(f(!!x,!!y))
  out[[1]]<-sym(op)
  out
}
#' @export
as_aggregate=function(x){
  class(x)<-c("aggregate",class(x))
  x
}

rebuild_and_deparse=function(x){
  out<-expr_replace_call_names(x,"ops","rebuild_op")
  if(is_call_type(out,"rebuild_op"))out<-eval(out)
  expr_deparse(out)%sep%"\n"
}
init_ops=function(proto){
  self<-.opsManager$new(proto = proto)
  #self$new_op(op=c("diff"),x=c("date"),class_name='diff_date')

  self$new_op(op=c("+","-"),"duration",'date_time',class_name='duration_date_time')
  self$new_op(c("+","-"),"duration",'date$',class_name='duration_date_time')
  self$new_op(c("-","compare"),"date_time",'date_time',class_name='datetime')
  self$new_op(c("-","compare"),"date$",'date_time',class_name='date_datetime')
  self$new_op(c("-","compare"),"date$",'date$',class_name='date')
  self$new_op('all',"units",'units',class_name='units',exclude = '^')
  self$new_op(c("/","*","^"),"units",'integer',class_name='default',bi_dir=FALSE)
  self$new_op(c("/","*"),"integer",'units',class_name='default',bi_dir=FALSE)
  self$new_op(c("/","*"),"float$",'units',class_name='default')
  self$new_op('all',"float$",'integer',class_name='default')
  self$new_op('all',"integer",'integer',class_name='default')
  self$new_op(c("+","-"),"float$",'date$',class_name='float_date')
  self$new_op(c("+","-"),"integer",'date$',class_name='integer_date')
  self$new_op(c("+","-"),"float$",'date_time',class_name='float_datetime')
  self$new_op(c("+","-"),"integer",'date_time',class_name='integer_datetime')

  self$new_op('all',"float$",'float$',class_name = "default")
  self$new_op('compare',"ordered","ordered",class_name = "default")
  self$new_op(c("==","!="),"factor$","factor$",class_name = "default")
  self$new_op(c("==","!="),"logical","logical",class_name = "default")
  self$new_op(c("==","!="),"factor","character",class_name = "default")
  self$new_op(c('compare'),"character","character",class_name = "default")
  self$new_op(c('+'),"character","character",class_name="character")
  self
}

