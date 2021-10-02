
DataTable = R6::R6Class(
  'DataTable',

  public = list(
    initialize = function(data) {
      if(is(data,"DataTable")){
        private$.data<-data$data
        private$.D<-data$data

  }else{
        assert(
          check_data_table(data),
          check_data_frame(data),
          check_matrix(data),
          check_tibble(data))
      if(!is(data,'data.table')){
      data=data.table(data)
      }
        private$.data<-data
        private$.D<-data
    }
      },
    DT=function(i,j,by,...,setnames,setunits,calculate_totals=FALSE){
      #tic("intro")



      I<-enexpr(i)
      J<-enexpr(j)
      tmp_data=NULL
      melt=FALSE
      reshape=FALSE
      by_names=NULL
      remove_by_names=NULL
      remove_melt_by_names=NULL
      delete_melt_names=NULL

      if(!is_missing(J)&&is_assignment(J)&&is_call(J[[2]],"units")){
        setunits<-list()

        setunits$names<-lapply(call_args(J[[2]]),as.character) %>% unlist()
        setunits$units=eval(J[[3]])
        if(is.null( setunits$units))
          setunits$drop=TRUE
        J=NULL
        j=missing_arg()
        print(setunits)
      }

      if(!missing(setnames)){
        setnames(private$.data,new=setnames$new,old = setnames$old,skip_absent= setnames$skip_absent)
      }
      if(!missing(setunits)){
        #validate setunits is a list

        assert(
        check_names(names(setunits),must.include = "names",subset.of = c("names","units","drop")),
        check_list(setunits,min.len = 1,max.len = 3),combine = "and")
        expr_eval(self$set_units(!!!setunits))

      }
      if(!is_missing(J)&&is_assignment(J)&&is_symbol(J[[2]])){
        J[[2]]<-as_name(J[[2]])
      }


    # if(!missing(key)){
      #   setkeyv(private$.data,key)
      # }
      if(!missing(j)){
        is_SD=is_call(J)&&J[[2]]==".SD"
      if(is_SD){
        SDJ=J
        J=SDJ[[4]]
      }
        J<-expr_apply_fn_formulas(J)
     has_stack= expr_has_call_name(J,"STACK",skip_first=FALSE)
     melt= expr_has_call_name(J,"MELT",skip_first=FALSE)
     if(has_stack){
     stack_expr<-expr_extract_call(J,"STACK",skip_first=FALSE)
     stack_args<-  stack_expr%>% lapply(call_args)
     names(  stack_args)<-NULL
     stack_args=unlist(  stack_args,recursive = F)

     if(!is_named(   stack_args))g_stop("stack expression must be named")
     remove_by_names=paste0(names(  stack_args)," names")
     }
     if(melt){
       if(is_call(J,'MELT')){
         J=expr(.(!!J))
       }
       if(is_call(J,'[')&&!is_missing(J[[4]])&&is_call(J[[4]],'MELT')){
         J[[4]]=expr(.(!!J[[4]]))
       }
       J= expr_make_melt_names(J)
       jtmp= expr_extract_call(J,"MELT",skip_first = FALSE)
       mtmp=lapply(jtmp,function(x){call_args(x)}) %>% unlist(recursive = T)
       #if(any(sapply(mtmp,is_list_call)==F))g_stop("MELT(...) must contain lists of expression")
       mtmp=lapply( mtmp,expr_unlist)

       mnames=names( mtmp)
       vcats=map2(mtmp,mnames,function(x,y)rep(y,l(x))) %>% unlist(use.names = F)
       names(mtmp)<-NULL
       mj_vars<-exprs_make_names(unlist(mtmp,recursive = F))
       j_names_before_melt=names(mj_vars)
       melt_cols=data.table(vars=names(mj_vars),cat=vcats) %>%
         split(by='cat',keep.by=F) %>%
         lapply(unlist,use.names=F)
       names(jtmp)=NULL
       c(new_by_names,new_j_names)%<-%list(paste(names(melt_cols),"names"),names(melt_cols))
       remove_melt_by_names=new_by_names
      # mtmp2=lapply(mtmp,function(x) expr(c(!!!x)))
       J=expr_find_replace_all(jtmp, mtmp,J)

       }

     }

      if(missing(i)){
        I=missing_arg()
      }else{
        I<-enexpr(i)
      }

      by<-enexpr(by)
      if(missing(by)||is.null(by)||(is_list_call(by)&&expr_list_len(by)==0)){
        by=missing_arg()

        delete_melt_names=remove_melt_by_names

      }else{

        assert(
          is_symbol(by),
          is_list_call(by),
          is_formula(by),
          is_plus_call(by)
        )
        reshape=is_formula(by)
        if(reshape==F&& is_plus_call(by)){
          by=expr(.(!!!expr_formula_args_to_list(by)))
        }
        if(reshape){
          shape_out=as.formula(expr_simplify_formula(by))

          by=expr(.(!!!expr_formula_args_to_list(by)))
        }
        if(is_symbol(by))by=expr(.(!!by))
        if(is_list_call(by)&&expr_list_len(by)>0){
          bytmp<-self$make_expr_names(by)
          delete_melt_names=remove_melt_by_names%NIN%  names( bytmp)
          bytmp<-bytmp[names(bytmp)%NIN%remove_melt_by_names]
          by_names=names(bytmp)
          bytmp2<-bytmp[names(bytmp)%nin%remove_by_names]
          if(!len0( bytmp2)){

          expr(self$data[,`:=`(!!!bytmp2)]) %>% eval()

          setkeyv(  self$data, names(bytmp2))
          }
          by=expr(.(!!!syms(by_names)))
          if(len0( bytmp))  by=missing_arg()
        }
      }
      if(missing(j)){
        J=missing_arg()
      }else{
        if(has_stack){

          melt_vars=stack_args %>% lapply(expr_find_names)
          names(melt_vars)<-make_names_unique(names(
            melt_vars))

          replace=syms(names(
            melt_vars))
          #joinTable=c('variable.names',paste(names( melt_vars),'variable'))
          #joinVariables<-self$data[,..joinTable]
          J=expr_find_replace_all(stack_expr,replace,J,match.first = T)
          vnams=paste(names( melt_vars),'names')
           vsyms=syms(paste(names( melt_vars),'names'))
          J_id.vars=expr_find_names( J)%NIN%names( melt_vars)%NIN%c(".D",".SD")
          J_id.vars=   J_id.vars%NIN%remove_by_names%or%NULL
          id.vars=c(by_names%NIN%remove_by_names,J_id.vars)
          other.measures=NULL
         if(len0(remove_by_names%IN%by_names)){
           other.measures=J_id.vars
           id.vars=c(by_names%NIN%remove_by_names)
         }

           tmp_data<-flatten(self$data,melt_cols=melt_vars,id.vars = id.vars,other.measures = other.measures%or%NULL)
           old_names=paste0(names(melt_vars)," value")
           new_names=names(melt_vars)
           data.table::setnames(tmp_data,old_names,new_names,skip_absent = T)

        }
        if(is_list_call(J)){
          jtmp<-expr_unlist(J)
          if(reshape){
            jtmp=exprs_make_names(expr_set_agg_func(    jtmp))
            zfillfn=c('N',
                      'N_missing',
                      'N_distinct',
                      'N_duplicated',
                      'sum')
            agg_funcs=expr_extract_agg_func( jtmp) %>% unlist()
            agg_funcs=ifelse(agg_funcs%in%c(zfillfn,toupper(zfillfn)),"zero_fill","na_fill")
            if(melt){
              agg_funcs=c(agg_funcs[names(jtmp)%nin%j_names_before_melt],rep("na_fill",l(new_j_names)))
            }
          }else{
            jtmp=exprs_make_names(jtmp)
          }
          j_names=names(jtmp)
          J=expr(.(!!!jtmp))
          J=expr_modify_fn_args(J,list(na.rm=self$na.rm),modify_if_present = FALSE)
        }
        if(is_SD){
          SDJ[[4]]=J
          J<-SDJ
        }
      }
      #by<-enexpr(by)
      #toc()
      #DTexprs<-enexprs(...)
#
      if(!missing(i)|!missing(j)|!missing(by)){

        .D= tmp_data%or%self$data
        prenames=names(.D)
        tmp0<- expr(data[!!maybe_missing(I),!!maybe_missing(J),by=!!maybe_missing(by)])

       if(expr_has_named_bracket(tmp0,name='.D',grep=TRUE)){
      #  tic("eval_JN")
        data_expr_tmp2<-self$eval_JN(tmp0,.D=.D,env=current_env())
       # toc()
        #tic("eval_fixed2")
        tmp0<-self$eval_fixed2(data_expr_tmp2,data=.D,env=current_env())
        #toc()
       }


         #tic('Query')
        tmp0[[2]]<-expr(.D)
        tmp0[[4]]<-replace_ops_in_expr(tmp0[[4]])
       # $tmpData<-data.table(.D)
        #tic("eval")
        #print(tmp0)
        if (calculate_totals) {
          i.n = tmp0[[3]]
          j.n = tmp0[[4]]
          by.n = tmp0[[5]]
          if (reshape)
            by.n = shape_out
          tmp = expr_eval(nested_totals(
            .D,i=!!maybe_missing(i.n),j=!!maybe_missing(j.n),
            by = !!maybe_missing(by.n)
          ))

        } else{
          tmp <- eval(tmp0)
        }
        #toc()
        if(melt){
          mcols=unlist(melt_cols,use.names = FALSE)
          tmp[,c(mcols):=map2(.SD,mcols,setattr,name="casted_name"),.SDcols=mcols]
          tmp[,c(mcols):=map2(.SD,lapply(.SD,function(x)c("casted",class(x))),setattr,name="class"),.SDcols=mcols]
          setattr(tmp,'class',c("casted",class(tmp)))
          tmp2<-melt(data=tmp)
          dattrs<-data_attr(tmp2)
          j_names<-c(j_names[ j_names%nin%j_names_before_melt],new_j_names)
          # if(is.null(by_names)&)
          #   delete_melt_names=remove_melt_by_names
          if(!is.null(delete_melt_names)&&!len0(delete_melt_names))
            tmp2[,c(delete_melt_names):=NULL]
          tmp=tmp2
        }


         if(reshape){
           if(melt){
          lhs=expr_find_names(f_lhs(shape_out))
          rhs=expr_find_names( f_rhs(shape_out))
          shape_out<-new_formula(expr_reduce(syms(lhs),`+`),expr_reduce(syms(rhs),`+`))
          by_names=unique(c(new_by_names,by_names))
          }
           agg_funcs=syms(agg_funcs)
           names(agg_funcs)=NULL

           tmp_casted = expr(dcast(tmp, !!shape_out, value.var = !!as.list(j_names),sep='<<>>',drop =TRUE,fun.aggregate=!!agg_funcs)) %>% eval()
            tmp=rename_casted_vars(dt=tmp_casted,by_names=by_names,j_names=j_names,from_data=.D,formula=shape_out)
            attributes(tmp)
            if(melt) data_attr(tmp)<-dattrs
           data_attr(tmp)<-data_attr(self$data%get%f_lhs_nms(shape_out))
           }
        #toc()
        #tic("expr_has_call_name")
        test<-!expr_has_call_name(tmp0, ':=')
        #toc()
        if(test){

          if(is(tmp,'data.table'))
            return(DataTable$new(tmp))
          return(tmp)
        }
      }
      invisible(self)
    },
    eval_fixed=function(d_expr,by=NULL,data,env=caller_env()){
      if(expr_has_named_bracket(d_expr,'.D',grep=TRUE)==F)
        return(self$CJ(x_expr=d_expr,y=by,.D=data,env=env))
      extract<-expr_extract_named_bracket(d_expr,'.D',grep=TRUE)
      by<-d_expr[names(d_expr)=='by'][[1]]
      if(missing(by)||is.null(by)||is_list_call(by)&&expr_list_len(by)==0){
        out<-lapply(extract,self$eval_fixed,by=NULL,env=env,data=data)
      }else{
        out<-lapply(extract,self$eval_fixed,by=by,env=env,data=data)
      }
      for(i in 1:length(out)){
        d_expr=expr_find_replace(extract[[i]],out[[i]],d_expr)
      }

      return(d_expr)
    },
    eval_fixed2=function(d_expr,by=NULL,data,env=caller_env()){
      while(expr_has_named_bracket(d_expr,name='.D',grep=TRUE)){
        d_expr<-  self$eval_fixed(d_expr,by=by,data=data,env=env)
      }
      d_expr
    },
    set_units=function(names,units=NULL,drop=FALSE){

      #validate eith units or drop is not null
      if(is.null(units)&is.null(drop))
        stop("either units or drop must be not null")
      #validate setunit names are present in data
      assert_subset(names,names(self$data))
      #validate setunit drop is logical or NULL
      assert_logical(drop)
      #validate setunit units is length 1, length of names, or NULL

        expr_eval(self$data[, `:=`(!!names,
                           map_units(.SD,!!units,!!drop)),.SDcols = !!names])
    },
    make_expr_names=function(e_list){
      dl<-as.list(e_list)[-1]
      if(is.null(names(dl))){
        maken<-lapply(dl,function(x)as.character(x)) %>% unlist()
        names(dl)=maken
      }else{
        maken<-lapply(dl[names(dl)==''],function(x)as.character(x)) %>% unlist()
        names(dl)[names(dl)=='']=maken
      }
      dl
      #expr(.(!!!dl))
    },
    CJ=function(x_expr,y,.D,env=caller_env()){
      x_by<- get_by(x_expr,env=env)
      x_expr=replace_ops_in_expr(x_expr)
      if(is.null(y)){
        out=eval(x_expr,envir=env)
        name=names(x_expr[[4]])[2]%or%last(names(out))

        dname<-last(make_names_unique(c(names(.D),name)))

        list2env(list2(!!dname:=out),envir = env)
        out<-expr(tmp[,!!sym(name)])
        out[[2]]<-sym(dname)
        return(out)
      }

      if(is.null(x_by)||is_list_call(x_by)&&expr_list_len(x_by)==0){
        out=eval(x_expr,envir=env)
        name=names(x_expr[[4]])[2]%or%last(names(out))

        dname<-last(make_names_unique(c(names(.D),name)))
        list2env(list2(!!dname:=out),envir = env)
        out<-expr(tmp[,!!sym(name)])
        out[[2]]<-sym(dname)
        return(out)
      }

      yn<-self$make_expr_names(e_list=y)
      x_by=self$make_expr_names(e_list=x_by)
      y_names=names( yn)
      by_names=c(x_by,yn)
      x_expr_eval=x_expr
      if(is_call(x_expr)&&grepl('\\.D_[[:alnum:]]{8}_\\.D',as.character(x_expr[[2]]))){
        x_expr_eval=x_expr[[2]]
      }
      oFqYbljWgsh9vfA_x=eval( x_expr_eval,envir=env)
      joinTable<-expr(unique(.D[,.(!!!by_names[!duplicated(by_names)])]))
      oFqYbljWgsh9vfA_joinTable<-eval(joinTable,envir=env)
      key<-names(oFqYbljWgsh9vfA_x)%IN%names(oFqYbljWgsh9vfA_joinTable)
      setkeyv(oFqYbljWgsh9vfA_x,key)
      setkeyv(oFqYbljWgsh9vfA_joinTable,key)
      x_join<-oFqYbljWgsh9vfA_joinTable[oFqYbljWgsh9vfA_x]
      expr(setkeyv(x_join,!! y_names)) %>% eval()
      name=names(x_expr[[4]])[2]%or%last(names(x_join))

      x_joinR6<-subsetDT$new(x_join,SDcol=name,by=y_names)
      x_join_SD<-x_joinR6$SDcol
      x_join_groups<-x_joinR6$grps
      SDname<-paste0("._",name,"._SDcol")
      GRPname<-paste0("._",name,"._GRP")
      list2env(list2(!!name:=x_join,!!SDname:=x_join_SD,!!GRPname:=x_join_groups),envir = env)
      out<-expr(SDcol[])
      out2<- expr(grps[[.GRP]])
      out[[2]]<-sym(SDname)
      out2[[2]]<-sym( GRPname)
      out[[3]]<- out2
      out
    },
    JN=function(s_expr,.D,env=caller_env()){

      .f=s_expr[[1]]
      x_expr=s_expr[[2]]
      y_expr=s_expr[[3]]
      #print(s_expr)
      if(expr_has_call_name(x_expr,c('+','-','/','*'),skip_first = FALSE)){
        tmp<-expr_extract_call( x_expr,c('+','-','/','*'),skip_first = FALSE)
        if(!is(tmp,'list'))tmp<-list(tmp)
        out<-lapply(tmp,self$JN,.D=.D,env=env)
        for(i in 1:length(out)){
          if(!is.null(out[[i]]))
            x_expr=expr_find_replace(tmp[[i]],out[[i]],x_expr)
        }
      }
      if(expr_has_call_name(y_expr,c('+','-','/','*'),skip_first = FALSE)){
        tmp<-expr_extract_call(y_expr,c('+','-','/','*'),skip_first = FALSE)
        if(!is(tmp,'list'))tmp<-list(tmp)
        out<-lapply(tmp,self$JN,.D=.D,env=env)
        for(i in 1:length(out)){
          if(!is.null(out[[i]]))
            y_expr=expr_find_replace(tmp[[i]],out[[i]],y_expr)
        }
      }
      if(is_fixed_arg(y_expr)) y_expr<-remove_parenthesis(y_expr)
      if(is_fixed_arg(x_expr)) x_expr<-remove_parenthesis(x_expr)
      s_expr[[2]]=x_expr
      s_expr[[3]]=y_expr
      # print(s_expr)
      # print(names(env))
      if(!has_fixed_arg(s_expr))
        return(s_expr)

      x_by=get_by(x_expr,env=env)
      y_by=get_by(y_expr,env=env)
      # print( x_by)
      # print(y_by)
      if(is.null(x_by)&is.null(y_by)){
        return(s_expr)
      }

      if(is.null(y_by)){
        if(is_call(y_expr)&&
           as_string(y_expr[[1]])=="["&&
           is_symbol(y_expr[[2]])&&
           grepl('.D',as_string(y_expr[[2]]),fixed=T))
          y_expr=expr(!!eval(y_expr,envir = env)[[1]])
        y_has_name=expr_has_name(y_expr,ignore.first=FALSE)
        if(y_has_name){
          #print("Y_has_name")
          x_by<-self$make_expr_names(e_list=x_by)
          key=names(x_by)
          x_expr_eval=x_expr
          if(is_call(x_expr)&&grepl('\\.D_[[:alnum:]]{8}_\\.D',as.character(x_expr[[2]]))){
            x_expr_eval=x_expr[[2]]
          }
          oFqYbljWgsh9vfA_x=eval(x_expr_eval,envir=env)
          setkeyv(oFqYbljWgsh9vfA_x,key)
          setkeyv(.D,key)
          out<-.D[oFqYbljWgsh9vfA_x]
          dname<-create_unique_id(8)
          SDname<-paste0(".D_",dname,"_.D")

          t1<-expr_unlist(x_expr[[4]]) %>% names() %>% sym()
          tmp<-expr(.f(!!t1,!!y_expr))
          tmp[[1]]<-.f
          #ltmp<-list2(!!dname:=tmp)
          #out2<-expr(out[,.(!!!c(x_by,ltmp))]) %>% eval()

          list2env(list2(.D:= out),envir = env)
          # v_name<-list2(!!dname:= sym(dname))
          # out<-expr(SDcol[,.(!!!v_name)])
          # out[[2]]<-sym(SDname)
          return(  tmp)
        }
        t1<-x_expr[[4]][[2]]
        tmp<-expr(.f(!!t1,!!y_expr))
        tmp[[1]]<-.f
        x_expr[[4]][[2]]<-tmp

        return(x_expr)
      }
      if(is.null(x_by)){
        if(is_call(x_expr)&&as_string(x_expr[[1]])=="["&&is_symbol(x_expr[[2]])&&grepl('.D',as_string(x_expr[[2]]),fixed=T))
          x_expr=expr(!!eval(x_expr,envir = env)[[1]])
        x_has_name=expr_has_name(x_expr,ignore.first=FALSE)
        if(x_has_name){
          y_by<-self$make_expr_names(e_list=y_by)
          key=names(y_by)
          y_expr_eval=y_expr
          if(is_call(y_expr)&&grepl('\\.D_[[:alnum:]]{8}_\\.D',as.character(y_expr[[2]]))){
            y_expr_eval=y_expr[[2]]
          }
          oFqYbljWgsh9vfA_y=eval( y_expr_eval,envir=env)
          setkeyv(oFqYbljWgsh9vfA_y,key)
          setkeyv(.D,key)
          out<-.D[oFqYbljWgsh9vfA_y]

          # dname<-create_unique_id(8)
          # SDname<-paste0(".D_",dname,"_.D")
          t1<-expr_unlist(y_expr[[4]]) %>% names() %>% sym()
          tmp<-expr(.f(!!x_expr,!!t1))
          tmp[[1]]<-.f
          #ltmp<-list2(!!dname:=tmp)
          #out2<-expr(out[,.(!!!c(y_by,ltmp))]) %>% eval()

          list2env(list2(.D:=  out),envir = env)
          # v_name<-list2(!!dname:= sym(dname))
          # out<-expr(SDcol[,.(!!!v_name)])
          # out[[2]]<-sym(SDname)
          return(tmp)
        }
        t1<-y_expr[[4]][[2]]
        tmp<-expr(.f(!!x_expr,!!t1))
        tmp[[1]]<-.f
        y_expr[[4]][[2]]<-tmp
        return(y_expr)
      }

      y_by<-self$make_expr_names(e_list=y_by)
      x_by<-self$make_expr_names(e_list=x_by)
      #y_names=names( yn)
      by_names=c(x_by,  y_by)
      x_expr_eval=x_expr
      if(is_call(x_expr)&&grepl('\\.D_[[:alnum:]]{8}_\\.D',as.character(x_expr[[2]]))){
        x_expr_eval=x_expr[[2]]
      }
      y_expr_eval=y_expr
      if(is_call(y_expr)&&grepl('\\.D_[[:alnum:]]{8}_\\.D',as.character(y_expr[[2]]))){
        y_expr_eval=y_expr[[2]]
      }

      oFqYbljWgsh9vfA_x=eval(  x_expr_eval,envir=env)
      oFqYbljWgsh9vfA_y=eval( y_expr_eval,envir=env)
      joinTable<-expr(unique(.D[,.(!!!by_names[!duplicated(by_names)])]))
      oFqYbljWgsh9vfA_joinTable<-eval(joinTable)

      #x_join
      key<-names(oFqYbljWgsh9vfA_x)%IN%names(oFqYbljWgsh9vfA_joinTable)
      setkeyv(oFqYbljWgsh9vfA_x,key)
      setkeyv(oFqYbljWgsh9vfA_joinTable,key)
      x_join<-oFqYbljWgsh9vfA_joinTable[oFqYbljWgsh9vfA_x]
      #y_join
      key<-names(oFqYbljWgsh9vfA_y)%IN%names(oFqYbljWgsh9vfA_joinTable)
      setkeyv(oFqYbljWgsh9vfA_y,key)
      setkeyv(oFqYbljWgsh9vfA_joinTable,key)
      y_join<-oFqYbljWgsh9vfA_joinTable[oFqYbljWgsh9vfA_y]

      key=names(oFqYbljWgsh9vfA_joinTable)
      setkeyv(x_join,key)
      setkeyv(y_join,key)
      out<-x_join[y_join]
      x_name=names(x_expr[[4]])[2]
      y_name=names(y_expr[[4]])[2]

      tmp<-expr(.f(!!sym(x_name),!!sym(y_name)))
      tmp[[1]]<-.f
      dname<-create_unique_id(8)

      ltmp<-list2(!!dname:=tmp)
      out2<-expr(out[,.(!!!c(by_names,ltmp))]) %>% eval()

      SDname<-paste0(".D_",dname,"_.D")

      list2env(list2(!!SDname:= out2),envir = env)
      v_name<-list2(!!dname:= sym(dname))
      out<-expr(SDcol[,.(!!!v_name)])
      out[[2]]<-sym(SDname)

      out
    },
    eval_JN=function(d_expr,.D,env=caller_env()){
      tmp<-expr_extract_call(d_expr,c('+','-','/','*'),skip_first = TRUE)
      if(is.null(tmp))return(d_expr)
      if(!is(tmp,'list'))tmp<-list(tmp)
      out<-lapply(tmp,self$JN,.D=.D,env=env)
      for(i in 1:length(out)){
        if(!is.null(out[[i]]))
          d_expr=expr_find_replace(tmp[[i]],out[[i]],d_expr)
      }
      d_expr
    },
    rename_casted_vars=function(dt,by_names,j_names){

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
    dt[,c(newn):=map2(.SD,variable,setattr,name="identity"),.SDcols=newn]

    setattr(dt,'class',c("casted",class(dt)))
    factorvars=interaction_vars[self$data[,sapply(.SD,is.factor),.SDcols=interaction_vars]]
    if(!len0(factorvars)){
  setattr(dt,"casted_factors",lapply(self$data[,.SD,.SDcols=factorvars],unique))
    }
    dt
    },
    join=function(dt,on=NULL){
      out=self$data
       on_init=on
      if(!is.list(dt))dt<-list(dt)
      for(i in 1:length(dt)){
      if(is.null( on_init)){
          on=names(self$data)[names(self$data)%in%names(dt[[i]]$data)]
      }
      dt2=dt[[i]]$data

      setkeyv(out,on)
      setkeyv(dt2,on)
      dt2<-out[dt2]
      }

      DataTable$new(   dt2)
      },
    delete=function(vars){
       DTnull(private$.data,vars)
     }
  ),
  private = list(
    .data=NULL,
    .D= NULL,
    .na.rm=TRUE
  ),
  active = list(

    print=function(value){
      if(missing(value)){
        print(private$.data)
      }
    },
    names=function(value){
      if(missing(value)){
        names(private$.data)
      }
    },
    data=function(value){
      if(missing(value)){
        return(private$.data)
      }
      private$.data=assert_data_table(value)

    },
    na.rm=function(value){
      if(missing(value)){
        return(private$.na.rm)
      }
      assert_logical(value)
      private$.na.rm<-value
    }
  )
)
