dynDataTable <-  R6::R6Class(
  'dynDataTable',
  public = list(
    func=NULL,
    vars=NULL,
    tmp_expr=NULL,
    tmp_formula=NULL,
    folders=NULL,
    initialize=function(data=NULL,vars=NULL,var_names_fn=NULL,name=NULL){
      #require(zeallot)
      self$folders[['measures']]<-VariableFolder$new('measures','measure','measures')
      self$folders[['dimensions']]<-VariableFolder$new('dimensions','dimension','dimensions')

      self$prv$.dataset_id<-paste0('data_',create_unique_id(5))
      self$prv$.dataset_name=name
      self$func=Variable
      if(!is.null(data)){
          self$set_data(data)

          if(!is.null(var_names_fn)){
            lapply(var_names_fn$var_id,self$add_variable_from_source)
            setname=function(id,name_fn){
              self$vars[[id]]$name<-name_fn
            }

            ids<-self$get_variable_id(var_names_fn$var_id)
            tmp=map2(ids,var_names_fn$name_fn,setname)
            datanames<-lapply(ids,function(id)expr(self$vars[[!!id]]$source_name))
            tmp<-map2(var_names_fn$var_id,datanames,self$data_setnames)


        }else{
          lapply(names(self$data),self$add_variable_from_source)
        }
}

        #if(is.function(data))  self$set_data(data)



    },
    new_variable=function(name,J,by=NULL,I=NULL,from_source=F,include_LOD=NULL,exclude_LOD=NULL,replace_old=F){


      # c(formula,by,I,key)%<-%self$parse_formula_parts(formula,by=by,I=I,key=key)
      if(is_empty(name))
        stop_quiet('Missing variable name')

      c(formula,output_type) %<-%
        check_if_valid(self$parse_formula_parts(
          name=name,
          J = J,
          by = by,
          I = I,
          exclude_LOD=exclude_LOD,
          include_LOD = include_LOD,
          from_source=from_source
        ))
      if(is_error(formula))return(formula)

      check_if_valid(
        self$check_for_self_referencing(
          name = name,
          J = J,
          by = by,
          I = I,
          from_source = from_source,
          include_LOD = include_LOD,
          exclude_LOD = exclude_LOD
        )
      )


      if(replace_old==F){
        var_id<-check_if_valid(self$generate_variable_id(name))

        self$vars[[var_id]]<-env(name=name,
                                 var_id=var_id)
      }else{
        var_id=self$get_variable_id(name,stop_if_not_found = F)
        if(is.null(var_id)){
          var_id<-self$generate_variable_id(name)

          self$vars[[var_id]]<-env(name=name,
                                   var_id=var_id)
        }
      }
      base_formula=self$replace_symbols_with_name_pointers(formula)
      #if(all(is.null(by)&is.null(exclude_LOD))){
      hidden_formula=self$replace_symbols_with_formula_pointers(formula)
      # }else{
      # hidden_formula=self$replace_symbols_with_pointer_type(formula,pointer = "hidden_J_for_fixed")
      # }

      has_fixed_calculation=l(c(by, include_LOD, exclude_LOD))!= 0

      test_formula=ifelse(from_source,function()formula,hidden_formula)
      #print("jer")

      data_results<-check_if_valid(self$test_formula(test_formula,
                                                     from_source,
                                                     has_fixed_calculation,
                                                     output_type))

      # #print("jer2")
      if(is_error(data_results)){
        if(replace_old==F) self$vars[[var_id]]<-NULL
        return(data_results)
      }

      # self$vars[[var_id]] = self$func$new(
      #   var_id = var_id,
      #   name = name,
      #   base_formula =  base_formula,
      #   hidden_formula = hidden_formula,
      #   is_source_variable = from_source,
      #   creation_id = self$var_count,
      #   class = 'numeric',
      #   is_aggregation = FALSE
      # )
      self$vars[[var_id]] = self$func$new(
        var_id = var_id,
        name = name,
        base_formula =  base_formula,
        hidden_formula = hidden_formula,
        is_source_variable = from_source,
        creation_id = self$var_count,
        dataset_id= self$dataset_id,
        class = data_results$data_class,
        is_aggregation = data_results$is_aggregation
      )
      vtype=self$vars[[var_id]]$variable_type
      if(vtype=='measure')
        self$vars[[var_id]]$folder_group=self$folders$measures
      if(vtype=='dimension')
        self$vars[[var_id]]$folder_group=self$folders$dimensions

      self$vars[[var_id]]$prototype= data_results$proto
      self$build_formulas(var_id)
      self$build_exclude_formula(var_id,exclude_LOD = exclude_LOD )
      self$vars[[var_id]]$requisite_variables<-
        self$gen_requisites_variable_fn(var_id)

      self$vars[[var_id]]$data<-function(){
        self$data
      }
      self$vars[[var_id]]$current_query_groups <-
        self$current_query_groups

      if(self$vars[[var_id]]$is_variable_stack){
       tmp=self$vars[[var_id]]
       var_id2=self[,tmp:='No {self$name} values']

        self$vars[[var_id2]]$name=function(){
          paste0( tmp$name," names") }
      }
      class(var_id)<-c('variable_id',class(var_id))
      return(var_id)
    },
    duplicate_variable=function(var_id){
      var<-self$assert_var_exists(var_id)
      name<-var$name
      variable_names=self$variable_names
      while(name%in%variable_names){
        name<-paste(name,"(copy)")
      }
      if(var$is_source_variable){
        out<-self$new_variable(name=name,J=var$name)
        return(out)
      }
      tmp<-var$clone()
      tmp$name=name
      new_var_id<-self$generate_variable_id(name)
      tmp$var_id<-new_var_id
      self$vars[[new_var_id]]<- tmp
      return(new_var_id)
    },
    new_variable_folder=function(name,type){

      if(name%in%self$folder_names)
        stop_quiet('Folder variable exists')
      tmp<- VariableFolder$new(name,type)
      self$folders[[tmp$folder_id]]<-tmp
      folder_id=tmp$folder_id
      class( folder_id)<-c(' folder_id',class( folder_id))
      return(  folder_id)
    },
    add_variable_to_folder=function(variable_id,folder_id){

      vtype=self$vars[[variable_id]]$variable_type
      ftype=self$folders[[folder_id]]$type
      if(is.null( ftype))
        g_stop("Folder id `{folder_id}` not found")
      if(is.null( vtype))
        g_stop("Variable id `{variable_id}` not found")
      if( ftype!=vtype)
        stop_quiet("Cannot add a {vtype} variable to a {ftype} folder")

      self$vars[[variable_id]]$folder_group<-self$folders[[folder_id]]


      return(invisible(NULL))
    },
    add_variable_to_new_folder=function(var_id){

      var<-self$assert_var_exists(var_id)
      name<-"Folder1"
      type<-var$variable_type

      folder_names=self$folder_names
      i<-1
      while(name%in%folder_names){
        i<-i+1
        name<-str_replace(name,"\\d+",as.character(i))
      }

      folder_id<-self$new_variable_folder(name,type)
      self$add_variable_to_folder(var_id,folder_id)
      return(invisible(folder_id))
    },
    check_for_self_referencing = function(name, J, by=NULL, I=NULL, from_source=F,include_LOD=NULL,exclude_LOD=NULL) {
      if(from_source&!is.null(c(by,include_LOD,exclude_LOD)))
        stop_quiet("source variable can't have fixed or filter expression")
      if(from_source&!is_empty(I))
        stop_quiet("source variable can't have fixed or filter expression")
      if (any(c(by,include_LOD,exclude_LOD) %in% name))
        stop_quiet('variable name `{name}` self referenced fixed expression')
      Jparsed<-parse_expr(J)
      name_is_in_J <-
        any(expr_find_names( Jparsed) %in% name)
      name_is_in_I =F
      if(!is_empty(I))
        name_is_in_I <-
        any(expr_find_names(parse_expr(I)) %in% name)
      if (name_is_in_I)
        stop_quiet('variable name `{name}` self referenced in fitler expression')
      J_is_not_type_symbol <-
        !is_symbol(parse_expr(J))

      if (name_is_in_J &
          J_is_not_type_symbol)
        stop_quiet('variable name `{name}` self referenced')
      if (name_is_in_J &
          from_source == F)
        stop_quiet('variable name `{name}` self referenced')
      return(TRUE)
    },
    parse_formula_parts=function(name,J,by,I,exclude_LOD=NULL,include_LOD=NULL,from_source){
      if(is.null(J)||str_trim(J)=="")
        stop_quiet("Formula can't be empty")

      J_parsed = try_parsing(J, error_message = "Formula has errors")

      if(is_error(J_parsed)){
        j_check<-check_valid_code(J)
        if(is_error(j_check))
          return(j_check)
        return(as_error_message(str_replace_all(str_trim(J_parsed),"\n",":")))
      }
      if(is_error(J_parsed))return(J_parsed)
      include_LOD_parsed=missing_arg()
      if(!is.null(include_LOD)){
        # J_parsed=self$parse_formula_parts(J,by=include_LOD,I=I,from_source=F)$formula
        # if(is_error(J_parsed))return(J_parsed)
        # J_parsed=expr(J(!!J_parsed))
        # I=NULL
        include_LOD=syms(include_LOD)
        include_LOD_parsed=expr(.(!!!include_LOD))

      }
      j_test=NULL
      if(from_source==F){
        j_test<-
          self$test_J_formula(J_parsed=J_parsed,include_LOD = NULL)
        if(is_error( j_test))return( j_test)
      }
      I_parsed=missing_arg()
      if(!is_empty(I)){
        I_parsed=try_parsing(I,error_message="Filter formula has errors")
        if(is_error(I_parsed)){
          check<-check_valid_code(I)
          if(is_error(check))
            return(check)
          return(I_parsed)
        }
      }
      if(!is.null(by)|!is.null(exclude_LOD)){
        by_parsed=missing_arg()
        if(!is.null(by)){
          by=syms(by)
          by_parsed=expr(.(!!!by))
        }
        if(is.null(by)){
          by_parsed=expr(.())
        }
        tmp<-list2(!!name:=J_parsed)
        J_parsed=expr(.(!!!tmp))
        return(list(formula=expr(.D[!!maybe_missing(I_parsed),!!J_parsed,by=!!maybe_missing(by_parsed)]),is_aggregation=j_test))

      }
      #list(formula=formula,by=by,I=I,key=key)

      list(formula=expr(.SD[!!maybe_missing(I_parsed),!!J_parsed,by=!!maybe_missing(include_LOD_parsed)]),is_aggregation=j_test)

    },
    test_J_formula=function(J_parsed,include_LOD=NULL){
      agg_test<-J_parsed
      if(!is.null(include_LOD))
        agg_test<-J_parsed[[2]][[4]]
      #IsAgg_formula=self$replace_symbols_with_pointer_type( agg_test,"calculation_type")()
      # tmp=self$eval_pointers(IsAgg_formula) %>%
      #   replace_calls_in_expr()
      #
      # try(eval(tmp),silent=T)
      vname=expr_find_names(  agg_test)

      ids= unlist( drop_nulls(sapply(vname,self$get_variable_id,stop_if_not_found = F)))
      valid_vars=names(ids)

      check_if_valid(check_code_usage(
        agg_test,
        valid_vars = valid_vars,
        valid_functions = getSDTfns()
      ))
      exprs_eval(expr_glue(
        {syms(names(ids))}  <-self$vars[['{ids}']]$prototype
      ))
      out<-try(eval(replace_ops_in_expr( agg_test)),silent=T)
      if(is_error(out))
        return(as_error_message(out))
      # if (is_error(out)) {
      #   var_names = vname %NIN% expr_find_assign(agg_test)
      #   missing = var_names[!sapply(var_names, function(x, env = current_env()) {
      #     getvar <- try(get(x, envir = env), silent = TRUE)
      #     if (is_error(getvar))
      #       return(FALSE)
      #     is_atomic(get(x, envir = env))
      #   })]
      #   if (len0(missing))
      #     return(as_error_message(out))
      #   out = glue('Variable/s `{missing%sep%","}` not found') %>%
      #     as_error_message()
      #   return(out)
      # }
      return(function(){
        eval(replace_ops_in_expr( agg_test))
      })

    },
    new_source_variable_placeholder=function(name,data_type_fn){
      self$data[,c(name):=data_type_fn(NA)]
      self$new_variable(
        name,
        J = name,
        by = NULL,
        I = NULL,
        from_source = T,
        include_LOD = NULL,
        exclude_LOD = NULL
      )
    },
    add_variable_from_source=function(name){

      J =sym(name)
      output_type='..source..'
      formula=expr(.SD[,!!J, by = , on = ])


      var_id<-self$generate_variable_id(name)

      self$vars[[var_id]]<-env(name=name,
                               var_id=var_id)

      base_formula=self$replace_symbols_with_name_pointers(formula)
      hidden_formula=self$replace_symbols_with_formula_pointers(formula)
      data_class=function(){data_type(self$data[[name]])}
      protofn=function(){
                   self$data[[name]][1]
                 }

      self$vars[[var_id]] = self$func$new(
        var_id = var_id,
        name = name,
        base_formula =  base_formula,
        hidden_formula = hidden_formula,
        is_source_variable = TRUE,
        creation_id = self$var_count,
        class =data_class,
        dataset_id=self$dataset_id,
        is_aggregation =F
      )
      if(self$is_data_function==FALSE){
      vtype=self$vars[[var_id]]$variable_type
      if(vtype=='measure')
        self$vars[[var_id]]$folder_group=self$folders$measures
      if(vtype=='dimension')
        self$vars[[var_id]]$folder_group=self$folders$dimensions
      }
      self$vars[[var_id]]$prototype=protofn
      self$build_formulas(var_id)
      self$vars[[var_id]]$requisite_variables<-
        self$gen_requisites_variable_fn(var_id)

      self$vars[[var_id]]$current_query_groups <-
        self$current_query_groups
      return(var_id)
    },
    build_formulas=function(var_id){
      inputs<-expand.grid(slot=c('I','J','by'),fn_type=c('query','formula'),stringsAsFactors = F)
      map2(inputs$slot,inputs$fn_type, self$generate_fn,var_id=var_id) %>%
        invisible()
    },
    build_exclude_formula=function(var_id,exclude_LOD){
      self$vars[[var_id]]$exclude=NULL
      if(!is.null(exclude_LOD)){
        if(!is.character(exclude_LOD)&!is_integerish(exclude_LOD))
          stop('exclude_LOD must by a name in character format or a integer')
        if(is.character(exclude_LOD)){
          excludeL<-expr(.(!!!syms(exclude_LOD)))
          exclude_syms_fn=self$replace_symbols_with_name_pointers( excludeL)
          self$vars[[var_id]]$exclude=function(){
            tmp<-exclude_syms_fn()
            out=self$eval_pointers(tmp)
            out
          }
        }
        if(is_integerish(exclude_LOD)){

          self$vars[[var_id]]$exclude=function(){
            exclude_LOD
          }
        }
      }
    },
    generate_fn=function(slot=c('I','J','by'),fn_type=c('query','formula'),var_id){

      slot=match.arg(slot,c('I','J','by','include'))
      index=chr_approx(c('I','J','by','include'),list(3,4,5,c(4,2,5)))(slot)[[1]]
      fn_type=match.arg(fn_type,c('query','formula'))

      sd_fn_type=chr_approx(c('query','formula'),
                                c("SD_formula","SD_base_formula")
      )( fn_type)
      fn_slot=slot %>% paste0(chr_approx(c('query','formula'),
                                             c("","_formula"))(fn_type))
      #tmp=self$vars[[var_id]]$hidden_formula[[4]]


      # if(self$vars[[var_id]]$is_source_variable&
      #    slot=="J") {
      #   self$vars[[var_id]][[fn_slot]] = function(x) {
      #     out
      #   }
      # } else{
      #


      tmp_expr=get_node(self$vars[[var_id]][[sd_fn_type]],index)

      if(missing(tmp_expr)){
        self$vars[[var_id]][[fn_slot]]=NULL
        return()
      }
      if(is_error(tmp_expr)){
        self$vars[[var_id]][[fn_slot]]=NULL
        return()
      }
      self$vars[[var_id]][[fn_slot]]<-
        function(){


          out= self$eval_pointers(tmp_expr)
          out
        }


      return()



    },
    next_level=function(exp_in,index){

      tmp=expr(ex[[!!index]])
      tmp[[2]]<-exp_in
      tmp
    },
    generate_variable_id=function(variable_name,id=create_unique_id(length =10)){
      if(variable_name%in%self$variable_names){
        return(g_stop("Variable name `{variable_name}` already exists", silent = TRUE))

        }
      return(paste0("var_",id))
    },
    gen_requisites_variable_fn=function(var_id){
      get_requisite_variable_names<-function(x){
        x[[3]]<-expr(name)
        name<-eval(x)
        x[[3]]<-expr(is_fixed)
        fixed<-eval(x)
        x[[3]]<-expr(is_aggregation)
        is_aggregation<-eval(x)
        data.table(variable_name=name,is_fixed=fixed,is_aggregation=is_aggregation)
      }
      id=var_id
      function(){
        tmp_id=create_unique_id(length = 5)
        self$tmp_expr[[tmp_id]]<-self$vars[[id]]$SD_formula
        out=self$eval_variable_pointers(
          x = self$tmp_expr[[tmp_id]],
          index = NULL,
          self_expr = expr(self$tmp_expr[[!!tmp_id]])
        )

        fd<-unlist(out)%>% unique()
        look=  lapply(fd,get_requisite_variable_names) %>% rbindlist() %>% unique()

        look
      }
    },
    test_formula=function(test_formula,from_source=F,has_fixed_calculation,output_type){

      if(has_fixed_calculation){
        require_true(is(output_type(),"aggregate"),
                     "LOD calculations can only be used with aggregated measure")
        out_type=output_type
        output_type=function(){
          out=out_type()
          cout=class(out)
          class(out)<-c('fixed',cout[cout!="aggregate"])
          out
        }
      }
      ag=is(output_type(),"grouped aggregates")

      self$current_query_groups<-NULL
      query= test_formula()
      if(!from_source){
        if(ag)query[[4]][[1]]<-expr(MELT)
        query<-expr_replace_call_names(query,"GROUP","STACK")
        query= self$eval_pointers( query)
      }
      #if is a fixed variable
      # if(length(query[[5]])>1){
      #   query[[2]]<-expr(.D)
      #   query[[6]]<-NULL
      #   #self$find_query_prerequite_vars(query,eval_pointers = F)
      #   #J<-eval(query)
      #   query<-expr(data[,!!query,])
      # }
      qtest=expr(data[,!!query,by=])
      qtest[[2]]<-self$data_expr

      out <-
        check_if_valid(
          try(
            eval(qtest),
            silent=T
          ))

      #query<-self$remove_I_and_BY_from_DT_query(query)
      # dtmp=self$data
      #
      #
      # # if(len(dtmp)<10){
      # #   dtmp<-rbindlist(rep(list(dtmp$data),10))
      # #   dtmp=DataTable$new(dtmp)
      # # }
      # query[[2]]<-expr(dtmp)
      #
      # out<-eval(query)
      if(is(out,'DataTable'))out=out[[ncol(out)]]

      is_aggregation=is(output_type(),"aggregate")
      data_class=data_type(out)
      list(is_aggregation=is_aggregation,data_class=data_class,proto=output_type)
    },
    replace_symbol=function(symbol,self_expr,replace_with='hidden_J'){
      #replace_type= match.arg(replace_with,c('formula','name'))
      if(!is_symbol(symbol))stop('symbol must be class symbol')
      name=as.character(symbol)
      id=self$get_variable_id(name,stop_if_not_found=F)
      if(name%in%c("(",")"))return()
      if(is.null(id))return()
      tmp<-expr(self$vars[[!!id]]$replace_with)
      tmp[[3]]=sym(replace_with)
      # tmp= switch(replace_type,
      #             formula=expr(self$vars[[!!id]]$hidden_J),
      #             name=expr(self$vars[[!!id]]$symbol))

      eval(expr(!!self_expr<-expr(!!tmp)))
    },
    replace_symbols_with_formula_pointers=function(formula){

      if(is_call(formula,"[")&&expr_type(formula[[4]])=="constant"){
            formula[[4]]<-expr(as_constant(!!formula[[4]]))
          }

      look<-expr_extract_call(formula,'GROUP',skip_first = F)
      self$tmp_formula<- build_group_fn(formula)
      self$find_symbols(replace_with='hidden_J')
      tmp_form<- self$tmp_formula
      tmp_form
      function(){ expr(!!tmp_form) }
    },
    replace_symbols_with_name_pointers=function(formula){
      self$tmp_formula<-formula
      self$find_symbols(replace_with='symbol')
      tmp_form<- self$tmp_formula
      function(){ expr(!!tmp_form) }
    },
    replace_symbols_with_pointer_type=function(formula,pointer){
      self$tmp_formula<-formula
      self$find_symbols(replace_with=pointer)
      tmp_form<- self$tmp_formula
      function(){ expr(!!tmp_form) }
    },
    eval_pointers=function(x){
      tmp_id=create_unique_id(length=5)
      self$tmp_expr[[tmp_id]]<-x
      #print(self$tmp_expr[[tmp_id]])
      self$eval_variable_pointers(
        x = self$tmp_expr[[tmp_id]],
        index = NULL,
        self_expr = expr(self$tmp_expr[[!!tmp_id]])
      )
      out=self$tmp_expr[[tmp_id]]
      self$tmp_expr[[tmp_id]]<-NULL
      return(out)

    },
    eval_variable_pointers = function(x,index=NULL, self_expr) {


      if(!is.null(index)){
        #print(index)
        self_expr<- self$next_level(self_expr,index)
        #print(deparse(self_expr))
      }
      flat_map <- function(.x, .f, ...) {
        map2(.x=.x,.y=seq_along(.x),.f=.f,...)
      }
      constant_f=function(x)return()
      #x=as.list(self$expr)[[index]]
      # self$expr[[3]]
      #print(expr_type(x))
      switch_expr(x,
                  # Base cases
                  constant =     character(),
                  symbol = character(),

                  # Recursive cases
                  pairlist = flat_map(.x=as.list(x),.f=self$eval_variable_pointers,self_expr=self_expr),
                  call = {
                    if(is_call(x, "$")){

                      replacement=eval(x)
                      #print(expr(!!self_expr<-!!x))
                      eval(expr(!!self_expr<-!!x))
                      if(!missing(replacement)){
                        out <- self$eval_variable_pointers(
                          replacement,
                          index = NULL,
                          self_expr)
                        return(c(list(x),out))
                      }
                      return(c(list(x),list(NA)))
                    } else {
                      flat_map(
                        .x = as.list(x),
                        .f = self$eval_variable_pointers,
                        self_expr = self_expr
                      )
                    }
                  }
      )
    },
    find_symbols = function(x=self$tmp_formula,index=NULL, self_expr=expr(self$tmp_formula),replace_with='hidden_J') {
      #replace_with=match.arg(replace_with,
      #if(is.null(index))print("NULL")
      if(!is.null(index)){

        self_expr<- self$next_level(self_expr,index)
        #print(deparse(self_expr))
      }
      flat_map <- function(.x, .f,index_start=1L, ...) {
        to=length(.x)+1
        if(length(.x)<index_start)to=index_start
        map2(.x=.x,.y=seq(from=index_start,to=to,by=1),.f=.f,...)
      }
      #  x=self$tmp_formula
      #x=call_args(x)[[1]]
      # self_expr= expr(x[[2]])
      constant_f=function(x)return()
      #print(expr_type(x))
      switch_expr(x,
                  # Base cases
                  constant = constant_f(x),
                  symbol = self$replace_symbol(symbol=x,self_expr=self_expr,replace_with = replace_with),

                  # Recursive cases
                  pairlist = flat_map(.x=as.list(x),.f=self$find_symbols,self_expr=self_expr,replace_with = replace_with),
                  call = flat_map(.x=call_args(x),.f=self$find_symbols,self_expr=self_expr,index_start = 2,replace_with = replace_with)

      )
    },

    rename_variable=function(old_name,new_name){
      if(old_name%nin%self$variable_names)stop("Variable name not found")
      if(new_name%in% self$variable_names)stop("Replacement variable name already exists")
      variable_id=self$get_variable_id(old_name)

      self$vars[[variable_id]]$name=new_name
      if(self$vars[[variable_id]]$is_source_variable)
        self$data_setnames(old_name,new_name)
    },
    rename_variable_by_id=function(var_id,new_name){

      var=self$assert_var_exists(var_id)
      old_name<-var$name

      if(new_name==old_name)
        return()
      if(new_name%in% self$variable_names)
        stop("Replacement variable name already exists")
      var$name=new_name
      if(var$is_source_variable)
        self$data_setnames(old_name,new_name)
    },
    rename_folder_by_id=function(id,new_name){
      folder=self$assert_folder_exists(id)
      old_name<-folder$name

      if(new_name==old_name)
        return()

      if(new_name%in% self$folder_names)
        stop("Replacement folder name already exists")
      folder$name=new_name
    },
    rename_object_by_id=function(id,new_name){
      if(id%starts_with%'folder_'){
        self$rename_folder_by_id(id,new_name)
      }
      if(id%starts_with%'parameter_'){
       # self$rename_parameter_by_id(id,new_name)
      }
      if(id%starts_with%'var_'){
       self$rename_variable_by_id(id,new_name)
      }
    },

    calculated_field=function(name,J,by=NULL,I=NULL){
      var_id<-self$new_variable(name,J,by,I)

    },
    data_setnames=function(old_name,new_name){

      self$set_names_expr<-expr(data[setnames=list(old=!!old_name,new=!!new_name,skip_absent = TRUE)])

    },

    build_I_expr=function(I){
      hidden_formula=self$replace_symbols_with_formula_pointers(parse_expr(I))
      self$eval_pointers(hidden_formula())

    },
    remove_I_and_BY_from_DT_query=function( query){

      query[[5]]<-missing_arg()
      query[[3]]<-missing_arg()

      if(typeof(query[[4]])=='language'){
        if(query[[4]][[2]]==expr(.SD)){
          query[[4]][[3]]<-missing_arg()
        }
      }
      query
    },
    build_query_expr=function(J,BY=NULL,I=NULL,pre_filter=NULL,post_filter=NULL){

      #J<-lapply(J,self$eval_pointers)
      #BY<-lapply(BY,self$eval_pointers)
      if(is.null(pre_filter)){
        pre<-expr(data)
      }else{
        pre<-expr(data[!!pre_filter])
      }
      if(is.null(I)){
        query<-expr(data[ ,.(!!!J),by=.(!!!BY)])
      }else{
        query<-expr(data[!!I,.(!!!J),by=.(!!!BY)])
      }
      query[[2]]<-pre
      if(nnull(post_filter)){
      dat=expr(data[!!post_filter])
      dat[[2]]<-query
      query=dat
      }
      self$eval_pointers(query)

      #query
    },
    query=function(vars,var_groups=NULL,filter=NULL){
      build_query_expr<-self$build_query_expr
      all_vars=c(vars,var_groups)
      ids<-self$get_variable_id(all_vars)
      self$current_query_groups<-by
      J<-self$get_attrs('formula',vars= self$get_variable_id(vars))$formula
      BY=rev(syms(by))
      data_fn=function(){
        data<-build_query_expr(J=J,BY=BY,filter=filter)
        setnames(data,all_vars,ids)
        data
      }
      var_names_fn<-self$get_attrs(attrs='name_fn',vars=ids)
      dynDataTable$new(data_fn,var_names_fn =var_names_fn )
    },
    query2=function(I=NULL,J,by=NULL){
      #  tic('total')
      build_query_expr<-self$build_query_expr
      self$current_query_groups<-NULL
      BY=NULL
      J<-enexpr(J)
      I<-enexpr(I)
      by<-enexpr(by)
      if(!is.null(by)){

        by=as.list(by)[-1]
        self$current_query_groups<-exprs_deparse(by)

        BY=self$parse_DT_list(rev(by))
        #  toc()
      }
      if(!is.null(I))#I<-expr_d(I)
      #tic('find_query_prerequite_vars')
      #prereqs=self$find_query_prerequite_vars(J)
      #toc()
      J=as.list(J)[-1]
      #tic('parse_DT_list(J)')
      J_in=self$parse_DT_list(J)
      #toc()
      #tic('get_name_fns')
      var_names_fn<-self$get_name_fns(c(by,J))
      #toc()
      all_vars=var_names_fn$name
      ids=var_names_fn$var_id


      #tmpself<-self
      #data_expr<-self$data_expr
      data_fn=function(rows=NULL){
        #print(J_in)
        rows=rows
        self$current_query_groups<-(exprs_deparse(by)%or%NULL)
        #self$current_query_prereqs<-prereqs
        tmp<-build_query_expr(J=J_in,BY=BY,filter=I)
        if(is.null(rows)){
          tmp[[2]]<-self$data_expr
        }
        if(!is.null(rows)){

          row_q=expr(data[!!rows])
            #row_q[[3]]<-rows
          row_q[[2]]<-self$data_expr
          tmp[[2]]<-  row_q
        }
        query<-expr(data[setnames=list(old=!!all_vars,new=!!ids,skip_absent=F)])
        query[[2]]<-tmp

        eval_tidy(query)

      }


      #is_quosure(data_q)
      #eval_tidy(data_q)
      #list(data= data_fn,var_names_fn =var_names_fn)

      out=dynDataTable$new(data= data_fn,var_names_fn =var_names_fn )
      # out
    },
    transform=function(I=NULL,J,by=NULL){
      #  tic('total')
      if(!is_formula(by))
        stop("'by' must be a formula in the transform function")
      build_query_expr<-self$build_query_expr
      self$current_query_groups<-NULL
      BY=NULL
      d_formula<-enexpr(d_formula)

      if(!is.null(I))I<-as.character(I)
      #tic('find_query_prerequite_vars')
      #prereqs=self$find_query_prerequite_vars(J)
      #toc()
      J=as.list(J)[-1]
      #tic('parse_DT_list(J)')
      J_in=self$parse_DT_list(J)
      #toc()
      #tic('get_name_fns')
      var_names_fn<-self$get_name_fns(c(by,J))
      #toc()
      all_vars=var_names_fn$name
      ids=var_names_fn$var_id


      #tmpself<-self
      #data_expr<-self$data_expr
      data_fn=function(rows=NULL){
        #print(J_in)
        rows=rows
        self$current_query_groups<-(as.character(by)%or%NULL)
        #self$current_query_prereqs<-prereqs
        tmp<-build_query_expr(J=J_in,BY=BY,filter=I)
        if(is.null(rows)){
          tmp[[2]]<-self$data_expr
        }
        if(!is.null(rows)){

          row_q=expr(data[!!rows])
          #row_q[[3]]<-rows
          row_q[[2]]<-self$data_expr
          tmp[[2]]<-  row_q
        }
        query<-expr(data[setnames=list(old=!!all_vars,new=!!ids,skip_absent=F)])
        query[[2]]<-tmp

        eval_tidy(query)

      }


      #is_quosure(data_q)
      #eval_tidy(data_q)
      #list(data= data_fn,var_names_fn =var_names_fn)

      out=dynDataTable$new(data= data_fn,var_names_fn =var_names_fn )
      # out
    },
    find_query_prerequite_vars=function(J,eval_pointers=T){
      jvars1=NULL
      if(eval_pointers){
        jvars1=self$get_variable_id( expr_find_names(J))
        J=self$replace_symbols_with_formula_pointers(J)() %>% self$eval_pointers()
      }
      jvars2=self$get_variable_id( expr_find_names(J),stop_if_not_found = F,return_matched = T)
      current_by_list<-self$get_attrs(attrs =c("by_list"),c(jvars1,jvars2) ,.if=has_by==T)$by_list %>% names()
      by_vars_list=self$get_attrs(attrs=c('formula','creation_id'),self$get_variable_id(current_by_list))
      out=by_vars_list$formula[order(by_vars_list$creation_id)]

      self$current_query_prereqs<-  (out%or%NULL)
      invisible(out)
    },
    parse_DT_list=function(x){
      parse_list=function(x){
        switch_expr(x,
                    # Base cases
                    constant =x,
                    symbol ={
                      tmp<-self$replace_symbols_with_pointer_type(x,'query_formula')()
                      tmp<-list(tmp)
                      names(tmp)<-deparse(x)
                      tmp
                    },
                    pairlist = {
                      tmp<- self$replace_symbols_with_pointer_type(x,'query_formula')()
                      tmp<-list(tmp)
                      names(tmp)<-deparse(x)
                      tmp
                    },
                    call = {
                      tmp<-self$replace_symbols_with_pointer_type(x,'query_formula')()
                      tmp<-list(tmp)

                      names(tmp)<-deparse(x)
                      #if(x[[1]]=='sum')names(tmp)<-deparse(x[[2]])
                      tmp
                    })
      }
      sapply(x,parse_list)
    },

    set_data=function(data){
      if(!is(data,"DataTable")&!is.function(data))
        data=DataTable$new(data)
      private$.data<-data
    },
    format_pill_data=function(pill_data){
      ids<-as.data.table(pill_data[names(pill_data)%NIN%c('data','folder')])
      ids<-lapply(split(ids,f=pill_data$folder),as.list)
      data_attrs<-as.data.table(pill_data$data)
      data_attrs=lapply(split(data_attrs,f=pill_data$folder),as.list)
      list(ids=ids,data=data_attrs)
    },

    get_name_fns=function(x){
      parse_name_list=function(x){
        switch_expr(x,
                    # Base cases
                    constant =NULL,
                    symbol ={
                      if(as.character(x)%nin%c(".D",".SD")){
                        id<-self$get_variable_id(as.character(x))
                        var_names_fn<-self$get_attrs(attrs='name_fn',vars=id)
                      }
                    },
                    pairlist = {
                      tmp<- self$replace_symbols_with_name_pointers(x)
                      evalp=self$eval_pointers
                      id=paste0("var_",create_unique_id(10))
                      name_fn<-list(
                        function(){
                          deparse(evalp(tmp()))
                        })
                      names(name_fn)<-id
                      list(var_id=id,
                           name=deparse(x),
                           name_fn=name_fn)

                    },
                    call = {
                      if(x[[1]]=='sum')x<-x[[2]]
                      tmp<- self$replace_symbols_with_name_pointers(x)
                      evalp=self$eval_pointers
                      id=paste0("var_",create_unique_id(10))
                      name_fn<-list(
                        function(){
                          deparse(evalp(tmp()))
                        })
                      names(name_fn)<-id
                      name_out=deparse(x)

                      list(var_id=id,
                           name=name_out,
                           name_fn=name_fn)

                    })
      }
      lapply(x,parse_name_list) %>%
        stack_lists()
    },
    get_formula_edit=function(var_id){
      var=self$vars[[var_id]]
      if(is.null(var))
        g_stop("variable id `{var_id}` not found")
      list(name=var$name,
           formula=expr_deparse(var$J_formula))
    },
    get_fixed_variables=function(vars=NULL){
      self$get_attrs(attr =c("creation_id","fixed_formula"),.if=is_fixed==T,vars=vars)

    },
    get_variable_range=function(var_id,conversion=NULL){
     self$data_expr

     J<-self$vars[[var_id]]$query_formula
     if(nnull(conversion)){
       conv<-sym(conversion)
       J<-expr(f(!!J))
       J[[1]]<-sym(conversion)
     }
       query<-expr(data[ ,!!J,by=.()])
       query<-self$eval_pointers(query)
     query[[2]]<- self$data_expr
     d_range(eval(query))

    },

    get_attrs=function(attrs=NULL,vars=NULL,.if=NULL,.in=NULL){
      attrs=unique(c('var_id','name',attrs))
      condition=enexpr(.if)
      #getter<-function(var) as.data.table(mget(attr,var,ifnotfound = NA))
      tmp=DT(matrix(ncol=l(attrs))) %>% na.omit()
      setnames(tmp,attrs)
      if(is.null(vars))vars<-names(self$vars)
      if(l(vars)==0){

        return(tmp)
      }
      if(!is.null(condition)){
        condition_attrs=expr_find_names(condition)
        out= lapply(self$vars[vars],mget_R6_attrs,c('var_id',condition_attrs)) %>%
          rbindlist(fill=T)
        vars<-expr(out[!!condition]$var_id) %>% eval()
        if(l(vars)==0)return(tmp)
      }

      out= lapply(self$vars[vars],mget_R6_attrs,attrs,.in=.in) %>%
        stack_lists()
      out
    },
    get_folder_attrs=function(attrs=NULL,ids=NULL,.if=NULL,.in=NULL){
      attrs=unique(c('folder_id','name',attrs))
      condition=enexpr(.if)
      #getter<-function(var) as.data.table(mget(attr,var,ifnotfound = NA))
      tmp=DT(matrix(ncol=l(attrs))) %>% na.omit()
      setnames(tmp,attrs)
      if(is.null(ids))ids<-names(self$folders)
      if(l(ids)==0){

        return(tmp)
      }
      if(!is.null(condition)){
        condition_attrs=expr_find_names(condition)
        out= lapply(self$folders[ids],mget_R6_attrs,c('folder_id',condition_attrs)) %>%
          rbindlist(fill=T)
        ids<-expr(out[!!condition]$folder_id) %>% eval()
        if(l(ids)==0)return(tmp)
      }

      out= lapply(self$folders[ids],mget_R6_attrs,attrs,.in=.in) %>%
        stack_lists()
      out
    },
    get_folder_id=function(folder_name,stop_if_not_found=T,return_matched=F){
      folder_names<-self$folder_names
      if(!folder_name%all_in%folder_names){
        missing_vars<-paste(folder_name%NIN%folder_names,collapse = ",")

        if(stop_if_not_found)stop(glue("Folders/s '{missing_vars}' not found"))
        if(!return_matched) return()
      }
      folder_name=folder_name[folder_name%in%folder_names]
      names_and_ids<-self$get_attrs()
      folder_names<-names_and_ids$name
      folder_ids<-names_and_ids$var_id
      folder_ids=folder_ids[folder_names%in%folder_name]

      folder_ids[match_order(folder_names[folder_names%in%folder_name],folder_name)]

    },
    get_variable_id=function(var_name,stop_if_not_found=T,return_matched=F){
      variable_names<-self$variable_names
      if(!var_name%all_in%variable_names){
        missing_vars<-paste(var_name%NIN%variable_names,collapse = ",")

        if(stop_if_not_found)stop(glue("Variable/s '{missing_vars}' not found"))
        if(!return_matched) return()
      }
      var_name=var_name[var_name%in%variable_names]
      names_and_ids<-self$get_attrs()
      variable_names<-names_and_ids$name
      variable_ids<-names_and_ids$var_id
      variable_ids=variable_ids[variable_names%in%var_name]

      variable_ids[match_order(variable_names[variable_names%in%var_name],var_name)]

    },
    get_folder_variable_ids=function(folder_id){
      self$assert_folder_exists(folder_id)
      out<-as.data.table(self$get_attrs(c('folder_id','folder_name','variable_type')))
      setkey(out,folder_id)

      if(folder_id%in%c('measures','dimensions')){
        out[,variable_type:=paste0(variable_type,'s')]
        setkey(out,variable_type)
       tmp<-out[folder_id]
       tmp[folder_name!=folder_id,c('var_id','name'):=list(folder_id,folder_name)]
       setkey(tmp,var_id)

       out<-unique(tmp)

      return(out$var_id[order(out$name)])
      }
      out<-out[folder_id]
      out$var_id[order(out$name)]
    },
    session_sort_folder=function(folder_id){
      data_ids<-self$get_folder_variable_ids(folder_id)
      #print(self$get_attrs(vars=data_ids))

      updateSortableDiv(inputId=folder_id,order=data_ids)
    },
    get_pill_html=function(id){
      if(id%starts_with%'folder_'){
        vars<-self$get_folder_variable_ids(id)
        data<-self$get_attrs('pill_data',vars=vars)$pill_data
        base_folder=glue('{self$folders[[id]]$type}s')
       c(folder_id,folder_name)%<-%str_split(data$folder,":::")[[1]]
        out<-pill_item(id=data$id,label=data$label,type=data$type,data=data$data,icon=data$icon)
       out<-list(pill_folder(folder_id,folder_name,out,sortable_group = glue("nav_{base_folder}")))
        names(out)=base_folder
        return(out)
      }
      if(id%starts_with%'var_'){
        var<-self$assert_var_exists(id)
        data<-var$pill_data

          out<-pill_item(id=data$id,label=data$label,type=data$type,data=data$data,icon=data$icon,is_hierachy=data$is_hierachy)
        names(out)<-var$folder_id
        return(out)
      }
      g_stop("invalid id: `{id}` in get_pill_html")
    },
    get_pill_insert=function(var_id){
      out<-self$get_pill_html(var_id)
      folder_id=names(out)
      vars_in_folder<-self$get_folder_variable_ids(folder_id)
      index<-which(vars_in_folder==var_id)-1
      if(index==0){
        return(list(id=folder_id,where='start',data_id=NULL,html=out[[1]]))
      }
      list(id=folder_id,where='after',data_id= vars_in_folder[index],html=out[[1]])
    },
    insert_new_pill=function(id,session = NULL){
      if(nnull(session)){
      c(id,where,data_id,html)%<-%self$get_pill_insert(id)

        sortable_insert(inputId=id, where = where, data_id = data_id,html,session=session)
        return(invisible(NULL))
      }
      self$get_pill_insert(id)
    },
    get_variable_name=function(var_id){
      var=self$vars[[var_id]]
      if(is.null(var))
        g_stop("variable id `{var_id}` not found")
      var$name
    },
    assert_var_exists=function(var_id){
      var=self$vars[[var_id]]
      if(is.null(var))
        g_stop("variable id `{var_id}` not found")
      return(invisible(var))
    },
    assert_folder_exists=function(id){
      var=self$folders[[id]]
      if(is.null(var))
        g_stop("folder id `{id}` not found")
      return(invisible(var))
    },
    add_filter_to_query_expr=function(q_expr,filter){
      self$eval_pointers(filter)
      out=expr(tmp[!!filter])
      out[[2]]<-q_expr
      out
    },
    eval_data_expr=function(x){

      eval(x)
    }

  ),
  private = list(
    .id_table=data.table::data.table(variable_name="init",id=sUtils::create_unique_id())[0],
    .var_count=0,
    .base_data_expr=rlang::expr(self$data),
    .data=NULL,
    .set_names=NULL,
    .current_query_groups=NULL,
    .current_query_prereqs=NULL,
    .current_filter=NULL,
    .current_J_filter=NULL,
    .current_agg_filter=NULL,
    .current_J=NULL,
    .current_by=NULL,
    .dataset_id=NULL,
    .dataset_name=NULL,
    .agg_filter_ranges=NULL,
    .pre_filter_ranges=NULL
  ),
  active = list(
    dataset_name = function(value) {
        if (missing(value)) {
            return(private$.dataset_name)
        }
          private$.dataset_name=assert_string(value)

    },
    dataset_id= function(value) {
      if (missing(value)) {
        return(private$.dataset_id)
      }
      stop('dataset_id is read only')

    },
    pre_filter=function(value){
      if (missing(value)) {
        if(is_null(private$.current_filter)){
          return(missing_arg())
        }
      return(private$.current_filter)
      }
      tmp=lapply(value,function(x)parse_expr(x$var))
      forms=self$parse_DT_list( tmp)
      value=map2(value,forms,function(x,i){x$formula=i
      return(x)})
      build_query_expr<-self$build_query_expr
      private$.pre_filter_ranges=function(){
        jnames=list(names( forms))
        names(jnames)<-jnames
        query<-build_query_expr(J=forms)
        query[[2]]<-self$pre_data
        data=eval(query)
        lapply(jnames,function(x)d_range(data[[x]]))
      }
      ranges=private$.pre_filter_ranges()
      value=map2(value,ranges,function(x,i){
        if(is.null(x$range))
              x$range=i
      return(x)
      })

      private$.current_filter=lapply(value,build_filter) %>% reduce(function(x,y)expr(!!x&!!y))

    },
    I=function(value){
      if (missing(value)) {
        if(is_null(self$J_filter))
          return(missing_arg())
        pre=self$pre_filter
        J=self$J_filter
        by=self$by
        build_query_expr =self$build_query_expr

     vnames<-as.list(names(self$J_filter))
       names(vnames)<-names(self$J_filter)
       private$.agg_filter_ranges <- function(name) {
         query <- build_query_expr(J = J,
                                   BY = by,
                                   pre_filter = pre)

         query[[2]] <- self$pre_data
         data = eval_tidy(query)
         lapply(vnames, function(x)
           d_range(data[[x]]))
       }
       if(is_missing(self$agg_filter))
         return(NULL)
       tmp<-self$build_query_expr(J=self$J_filter,BY=self$by,post_filter = self$agg_filter)
       insert_data_expr(tmp,pre=sym('.D'))
       return(insert_data_expr(tmp,pre=sym('.D')))

      }
      stop('I is read only')
    },
    agg_filter=function(value){
      if (missing(value)) {
        if(is_null(private$.current_agg_filter)){
          return(missing_arg())
        }
        out=private$.current_agg_filter  %>% reduce(function(x,y)expr(!!x&!!y))
        return(out)
      }
      self$J_filter<-lapply(value,function(x)parse_expr(x$var))
      private$.current_agg_filter<-
        drop_nulls(lapply(value,build_filter,use_formula=FALSE)) %or%NULL
      invisible(self$I)
    },
    current_query_dimensions=function(value){
      if(missing(value)){
        return(
          function(){
            if(is.null(private$.current_query_groups))
              return(NULL)
            private$.current_query_groups
          }
        )
      }
      private$.current_query_groups<-NULL
      if(!is.null(value)){
        private$.current_query_groups<-lapply(value,self$eval_pointers)
      }
    },
    current_query_groups=function(value){
      if(missing(value)){
        return(
          function(){
            if(is.null(private$.current_query_groups))
              return(NULL)
            self$get_attrs('formula',
                           vars= private$.current_query_groups
            )$formula
          }
        )
      }
      private$.current_query_groups<-NULL
      if(!is.null(value)){
        private$.current_query_groups<-
          self$get_variable_id(value)
      }
    },
    current_query_prereqs=function(value){
      if(missing(value)){
        return(
          private$.current_query_prereqs
        )
      }
      private$.current_query_prereqs<-NULL
      if(!is.null(value)){
        private$.current_query_prereqs<- value
      }
    },
    var_count=function(value){
      if(missing(value)){
        private$.var_count=private$.var_count+1L
        return(private$.var_count)
      }
      stop("var_count can't be set")

    },
    id_table=function(value){
      if(missing(value)){
        return(private$.id_table)
      }
      stop("id_table is read only")
    },
    variable_names=function(value){
      if(missing(value)){
        return(self$get_attrs()$name)
      }
      stop("variable_names is read only")
    },
    data=function(value){
      if(missing(value)){
        if(is.function(private$.data))
          return(private$.data())
        if(is(private$.data,'DataTable'))
          return(private$.data)
      }
    },
    data.table=function(value){
      if(missing(value)){
        tmp<-eval(self$set_names_expr[[1]])
        return(tmp$data)
      }else{
        stop("print_data is read only")
      }
    },
    print_data=function(value){
      if(missing(value)){
        tmp<-eval(self$set_names_expr[[1]])
        print(tmp$data)
      }else{
        stop("print_data is read only")
      }
    },
    set_names_expr=function(value){
      if(missing(value)){
        if(is.null(private$.set_names))
          return(list(private$.base_data_expr))
        return(list(private$.set_names))
      }
      if(is.null(private$.set_names)){
        value[[2]]=private$.base_data_expr

        private$.set_names= value

      } else {
        value[[2]]<-private$.set_names
        private$.set_names=value
      }
    },
    pre_data=function(value){
      self$set_names_expr[[1]]
    },
    data_expr=function(value){
      if(missing(value)){
        ##out<-self$get_attrs(attr =c("creation_id","fixed_formula"),.if=is_fixed==T)
        insert_fixed_variable=function(x,y){
          if(is.list(x)&is.list(y)){
            y[[1]][[2]]<-x[[1]]
            return(y)
          }
          y[[2]]<-x
          y
        }
        #fixed_formulas<-self$get_fixed_variables()$fixed_formula
        groups<- c(self$current_query_groups(), self$current_query_prereqs)
        groups<-names(groups[!duplicated(groups)])
        if(is.null(groups))return(self$set_names_expr[[1]])
        by_vars_list=self$get_attrs(attrs=c('formula','creation_id'),self$get_variable_id( groups),.if=is_source_variable==F)
        if(len0(by_vars_list))return(self$set_names_expr[[1]])
        groups <-by_vars_list$formula[order(by_vars_list$creation_id)]
        fixed_formulas<-list()
        for(i in 1:l(groups)){
          fixed_formulas[[i]]<- expr(data[,`:=`(!!sym(names(groups[i])),!!groups[[i]]) ])

        }
        reduce(c(self$set_names_expr,fixed_formulas),insert_fixed_variable)

      }
    },
    fixed_variable_name=function(value){
      if(missing(value)){
        return(self$get_fixed_variable_exprs()$name)
      }
      stop('fixed variables is read only')
    },
    identity_vars=function(value){
      if(missing(value)){
        return(self$get_attrs('pill_data',.if=is_class_identity==TRUE))
      }
      stop('identity_vars is read only')
    },
    measure_vars=function(value){
      if(missing(value)){
        return(self$get_attrs('pill_data',.if=is_class_continuous==TRUE&is_class_date==FALSE))
      }
      stop('continuous_vars is read only')
    },
    date_vars=function(value){
      if(missing(value)){
        return( self$get_attrs('pill_data',.if=is_class_date==TRUE))
      }
      stop('date_vars is read only')
    },
    dimension_vars=function(value){
       if(missing(value)){
         return( self$get_attrs('pill_data',.if=is_class_date==TRUE|is_class_identity))
       }
       stop(' dimension_vars is read only')
    },
    measure_pills=function(value){
      if(missing(value)){
  return( self$format_pill_data(self$measure_vars$pill_data))
      }
      stop(' dimension_vars is read only')
    },
    dimension_pills=function(value){
      if(missing(value)){
        return( self$format_pill_data( self$dimension_vars$pill_data))
      }
      stop(' dimension_vars is read only')
      },
    folder_names=function(value){
      if(missing(value)){
        return(self$get_folder_attrs()$name)
      }
      stop("folders_names is read only")
    },
    naming_functions = function(value) {
    if (missing(value)) {
        return(self$get_name_fns(unique(c(private$.current_by,private$.current_J,private$.current_J_filter))))
    }
    stop("naming_functions is read only")

},
    J = function(value) {
    if (missing(value)) {
        return(self$parse_DT_list(private$.current_J))
    }
      private$.current_J<-assert_exprs(value)
    },
    J_filter = function(value) {
      if (missing(value)) {

        return(self$parse_DT_list(private$.current_J_filter))
      }
      private$.current_J_filter<-assert_exprs(value)
    },
    by = function(value) {
      if (missing(value)) {
        by=private$.current_by
        return( self$parse_DT_list(rev(by)))
      }
      private$.current_by<-assert_exprs(value)

    },
    pre_filter_ranges = function(value) {
    if (missing(value)) {
      if(is.null(private$.pre_filter_ranges))
        return(function(){NULL})
      return(private$.pre_filter_ranges)
    }
    stop("pre_filter_ranges  is read only")

    },
    agg_filter_ranges = function(value) {
  if (missing(value)) {
    if(is.null(private$.agg_filter_ranges))
      return(function(){NULL})
    return(private$.agg_filter_ranges)
  }
  stop("agg_filter_ranges is read only")

},
    current_query=function(value){
       if(missing(value)){
         parent=self
        build_query_expr<-parent$build_query_expr

        J_in=parent$J
        J_Filter=parent$J_filter
        var_names_fn<-parent$naming_functions
        all_vars=var_names_fn$name
        ids=var_names_fn$var_id
        BY=parent$by
        pre_filter=parent$pre_filter
        I=parent$I
        rows=NULL

        data_fn=function(rows=NULL){

          tmp<-build_query_expr(I=parent$I,J=J_in,BY=BY,pre_filter= parent$pre_filter)

          tmp[[2]]<-parent$pre_data


          query<-expr(data[setnames=list(old=!!all_vars,new=!!ids,skip_absent=F)])
          query[[2]]<-tmp

          eval_tidy(query)

        }


        #is_quosure(data_q)
        #eval_tidy(data_q)
        #list(data= data_fn,var_names_fn =var_names_fn)

        out=dynDataTable$new(data=    data_fn,var_names_fn =var_names_fn )
        #out$print_data
        return(invisible(out))
       }
      stop("current query is read-only")
    },
    is_data_function = function(value) {
    if (missing(value)) {
        return(is.function(private$.data))
    }
    stop("is_data_query is read only")

},
    prv = function(value) {
  if (missing(value)) {
    return(private)
  }
  private<-value

}
  )
)
build_filter=function(filter,pointer='query_formula',use_formula=TRUE){
  if(is.null(filter$range ))
     return( NULL)
  if(use_formula==TRUE){

  assert_named_list(filter,
                    structure = list(
                      var = string(),
                      range = atomic(),
                      type = choice(choices = c('range', 'exact')),
                      exclude = TF(),
                      formula=call()))
    tmp=filter$formula
  }else{
    assert_named_list(filter,
                      structure = list(
                        var = string(),
                        range = atomic(),
                        type = choice(choices = c('range', 'exact')),
                        exclude = TF()))
    tmp=sym(filter$var)
  }



  if(filter$type=='range'){

    out<-expr(!!tmp%inrange%list(!!min(filter$range),!!max(filter$range)))
    out
  }
  if(filter$type=='exact'){
    out<-expr(!!tmp%in%!!parse_expr(expr_text(filter$range)))
  }
  if(filter$exclude)
    out<-expr(!(!!out))
  out
}


