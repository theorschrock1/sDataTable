unitFunctions=R6::R6Class(
  'unitFunctions',
  public = list(
    initialize = function() {
    },
    unit_fn_name=function(unit){
      if(is.null(unit))return(NULL)
     tmp<-data.table(name=c("d",'s','min','h','yr','week'),alias='DAYS',"SECONDS","MINUTES","HOURS","WEEKS")
     tmp[name=='unit']$alias

    }
  ),
  private = list(

  ),
  active = list(
    unaggregated_continuous_date = function(value) {
    if (missing(value)) {
        return(c("YEAR","QUARTER","MONTH","WEEK","DAY"))
    }
    stop("date is read only")

},
    unaggregated_continuous_float_duration_units = function(value) {
    if (missing(value)) {
        return(c("YEARS","WEEKS","DAYS","HOURS","MINUTES","SECONDS"))
    }
    stop("duration is read only")

    },
    unaggregated_continuous_time = function(value) {
    if (missing(value)) {
        return(c("HOUR","MINUTE","SECOND"))
    }
    stop("time is read only")

    },
    unaggregated_continuous_date_time = function(value) {
    if (missing(value)) {
        return(c(self$unaggregated_continuous_date ,self$unaggregated_continuous_time))
    }
    stop("date_time is read only")

    },
    unaggregated_continuous_float_distance_units = function(value) {
    if (missing(value)) {
        return()
    }
    stop("distance is read only")

}
  )
)

variableClass = R6::R6Class(
  'variableClass',
  public = list(
    initialize = function(class) {
      private$.class=class
    }

  ),
  private = list(
    .class=NULL
  ),
  active = list(
    class=function(value){
      if(missing(value)){
        if(is.function(private$.class))
          return(private$.class())
        return(private$.class)
      }
    }
  )
)
variableClass = sUtils::add_R6_active_logicals(
  variableClass,
  .f = is_class,
  test_variable = "class",
  options = c(
    "continuous",
    "identity",
    "date",
    "factor",
    "character",
    "logical",
    "time",
    "ordered",
    "integer",
    "numeric",
    'duration'
  )
)


Variable= R6::R6Class(
  'Variable',
  inherit=variableClass,
  public = list(

    initialize = function(var_id,name,base_formula,hidden_formula,is_source_variable=F,creation_id,is_aggregation,dataset_id,...) {
      self$super_init(...)
      if(!missing(hidden_formula))  self$SD_formula=hidden_formula
      if(!missing(base_formula))    self$SD_base_formula=base_formula
      private$.is_source_variable=is_source_variable
      private$.dataset_id=dataset_id
      self$name=name
      self$var_id=var_id
      self$creation_id=creation_id
      self$is_aggregation=is_aggregation
      private$.unitsFns=unitFunctions$new()
    },
    super_init=sUtils::import_fn(sUtils::super_init)
  ),
  private = list(
    .init=FALSE,
    .unitsFns=NULL,
    .is_source_variable=NULL,
    .SD_formula=NULL,
    .SD_base_formula=NULL,
    .J=NULL,
    .J_formula=NULL,
    .name=NULL,
    .by_formula=NULL,
    .by=NULL,
    .I_formula=NULL,
    .I=NULL,
    .requisite_variables=NULL,
    .var_id=NULL,
    .creation_id=NULL,
    .is_aggregation=NULL,
    .current_query_groups=NULL,
    .exclude_LOD_groups=NULL,
    .include=NULL,
    .include_formula=NULL,
    .exclude=NULL,
    .name_fn=NULL,
    .dataset_id=NULL,
    .exclude_formula=NULL,
    .include_LOD_groups_hidden=NULL,
    .data=NULL,
    .has_been_evaluated=FALSE,
    .units=NULL,
    .prototype=NULL,
    .default_aggregation="SUM",
    .folder_group=NULL,
    .default_converstion=NULL
  ),
  active = list(
    current_query_groups=function(value){
      if(missing(value)){
        if(!is_function(private$.current_query_groups))
          return(NULL)
        return(private$.current_query_groups())
      }
      if(!is_function(value))
        stop("current_query_groups can only be a function")
      private$.current_query_groups<-value
    },
    is_aggregation=function(value){
      if(missing(value)){
        if(is(self$prototype,"grouped aggregates"))
          return(TRUE)
        if(!self$is_class_continuous)
          return(as.logical(NA))
        if(self$is_class_date)
          return(as.logical(NA))
        if(self$is_class_time)
          return(as.logical(NA))
        if(self$is_class_continuous)
          return(private$.is_aggregation)
      }
      private$.is_aggregation=value
    },
    default_aggregation=function(value){
      if(missing(value)){
        if(is.na(self$is_aggregation))
          return(as.logical(NA))
        if(self$is_aggregation)
          return("AGG")
        return(private$.default_aggregation)
      }
      private$.default_aggregation=value
    },
    default_conversion=function(value){
      if(missing(value)){
       if(self$is_class_date)return("YEAR")
       if(self$is_class_duration){
         private$.unitsFns$unit_fn_name(get_units(self$prototype))
       }
        return(NULL)
      }
      stop("default conversion is read only")
    },
    conversion_opts = function(value) {
    if (missing(value)) {
        out<-private$.unitsFns[[self$class]]
        if(nnull(out))return(jsonlite::toJSON(out))
        return(NULL)
    }
    stop("conversion_opts is read only")

},
    has_aggregation=function(value){
      if(missing(value)){
        reqs<-private$.requisite_variables()
        reqs[,has_aggregation:=is_fixed|is_aggregation]
        return(any(c(self$is_aggregation,self$is_fixed,self$has_include,reqs$has_aggregation)))
      }
      stop("has_agregation is read only")
    },
    calculation_type=function(value){
      if(missing(value)){
        if(!self$is_class_continuous){
          return('..cat..')
        }
        if( self$is_aggregation){
          return('..agg..')
        }
        return('..measure..')
      }
    },
    prototype=function(value){
      if(missing(value)){
        if(is.null(private$.prototype))g_stop("Null prototype in var {self$name}")
        out<-private$.prototype()
        if(self$is_source_variable&&!is.null(self$units))
          return(expr_eval(set_units(out,!!self$units)))
        return(out)
      }
       assert_function(value)
       private$.prototype<-value
    },
    units=function(value){
      if(missing(value)){
        return(private$.units)
      }
      assert_character(value,len = 1)
      private$.units=value
    },
    is_hierachy = function(value) {
    if (missing(value)) {
        return(self$is_class_date)
    }
    stop("is_hierachy is read only")

},
    hierachy_n = function(value) {
      if (missing(value)) {
       if(self$is_hierachy){
         return(0)
       }else{
         return(NA)
       }
      }
      stop("is_hierachy is read only")

    },
    SD_base_formula=function(value){
      if(missing(value)){
        if(is.null(private$.SD_base_formula))return(private$.SD_base_formula)
        return(private$.SD_base_formula())
      }
      private$.SD_base_formula=value
    },
    requisite_variables=function(value){
      if(missing(value)){
        return(private$.requisite_variables())
      }
      if(!is_function(value)&!is.null(value))stop("requisite_variables must be a function")
      private$.requisite_variables<-value
    },
    SD_formula=function(value){
      if(missing(value)){
        if(is.null(private$.SD_formula))return(private$.SD_formula)
        return(private$.SD_formula())
      }
      private$.SD_formula=value
    },
    hidden_J_formula=function(value){
      if(missing(value)){

        if(is.null(private$.SD_base_formula))return(private$.SD_base_formula)
        if(private$.is_source_variable)return(expr(self$vars[[!!self$var_id]]$symbol))
        return(private$.SD_base_formula()[[4]])
      }
      private$.SD_base_formula=value
    },
    hidden_J=function(value){
      if(missing(value)){
        if(is.null(private$.SD_formula))return(private$.SD_formula)
        if(private$.is_source_variable){
          return(expr(self$vars[[!!self$var_id]]$symbol))
        }
        if(self$has_include){
          tmp=self$SD_formula
          tmp[[5]]<-expr(self$vars[[!!self$var_id]]$include_LOD_groups)

          return(expr(J(!!tmp)))
        }
        if(self$is_fixed){

          tmp=self$SD_formula

          names(tmp[[4]])[2]<-create_unique_id(length=6)
          tmp[[5]]<-expr(self$vars[[!!self$var_id]]$BY)
          #tmp[[6]]<-NULL

          # on<-self$current_query_groups
          # on=syms(names(on))
          # by=expr(.(!!!on))
          # out<- expr(.FD[,!!tmp,by=,join=TRUE])
          # if(length(on)>0)out[[5]]<- by

          return(tmp)
        }
        if(self$has_I){
          return(self$SD_formula)
        }
        self$exists_in_data
        if(private$.has_been_evaluated)return(self$symbol)
        qformula=private$.SD_formula()[[4]]
        if(self$is_variable_stack){
          names( qformula)[2]<-self$name
          if(self$is_aggregation)qformula[[2]]<-expr(MELT)
          if(!self$is_aggregation)qformula[[2]]<-expr(STACK)
          return(qformula)
        }
        if(!isTRUE(self$is_aggregation)){
            if(!expr_has_named_bracket( qformula,".D",skip_first = F)){
              qformula=self$J
              eval(expr(self$data[,`:=`(!!self$symbol,!!qformula)]))
              private$.has_been_evaluated=TRUE
              return(self$symbol)
            }

         }
        return(  qformula)
      }
      private$.SD_formula=value
    },
    hidden_J_for_fixed=function(value){
      if(missing(value)){
        if(is.null(private$.SD_formula))return(private$.SD_formula)
        if(private$.is_source_variable){
          return(expr(self$vars[[!!self$var_id]]$symbol))
        }
        if(self$has_include){
          tmp=private$.SD_formula()[[4]]
          tmp[[2]][[5]]<-expr(self$vars[[!!self$var_id]]$include_LOD_groups)

          return(tmp)
        }
        if(self$is_fixed){

          tmp=self$SD_formula
          if(self$J[[2]]==expr(.FD))tmp[[4]]<-self$J[[4]]

          tmp[[5]]<-expr(self$vars[[!!self$var_id]]$BY)
          tmp[[6]]<-NULL
          tmp[[2]]<-expr(.FD)

          return(tmp)
        }
        if(self$has_I){
          return(self$SD_formula)
        }
        # if(self$has_include){
        #   tmp=private$.SD_formula()[[4]]
        #   tmp[[2]][[5]]<-expr(self$vars[[!!self$var_id]]$include_LOD_groups)
        #   return(tmp)
        # }
        return(private$.SD_formula()[[4]])
      }
      private$.SD_formula=value
    },
    J_formula=function(value){
      if(missing(value)){
        if(is.null(private$.J_formula))stop("No Formula Found")
        return(private$.J_formula())
      }

      if(!is_function(value)&!is.null(value))stop("J_formula must be a function")
      private$.J_formula<-value
    },
    J=function(value){
      if(missing(value)){
        if(is.null(private$.J))stop("No Formula Found")
        Jout<-private$.J()
        if(self$is_fixed)
          names(Jout)[2]<-create_unique_id(length=6)
        return(Jout)
      }

      if(!is_function(value)&!is.null(value))stop("J must be a function")
      private$.J<-value
    },
    formula=function(value){
      if(missing(value)){

        if(private$.is_source_variable){
          tmp=list(self$J)
          names(tmp)<-as.character(self$symbol)
          return(tmp)
        }
        if(self$is_fixed){
          tmp<-list(self$fixed_formula)

          names(tmp)<-as.character(self$symbol)
          return(tmp)
        }
        if(self$has_include){
          BY=self$include_LOD_groups
          J=self$J
          I=self$I

          jtmp=expr(.SD[,!!J,by=!!BY])
          if(!is.null(I))jtmp[[3]]<-I
          tmp=expr(J(!!jtmp))
          tmp=list(tmp)
          names(tmp)<-as.character(self$symbol)
          return(tmp)
        }
        if(self$has_I){
          tmp<-self$SD_formula
          tmp[[3]]<-self$I
          tmp[[4]]<-self$J
          tmp<-list(tmp)
          names(tmp)<-as.character(self$symbol)
          return(tmp)
        }
        self$exists_in_data
        if(private$.has_been_evaluated){
          tmp=list(self$symbol)
          names(tmp)<-as.character(self$symbol)
          return(tmp)

        }
        qformula=self$J
        if(self$is_stacked_variable_names){
         mess=str_remove(self$name,"\\snames")
          qformula<-expr(as_constant(!!glue("No {mess} values")))

        }
        if(self$is_variable_stack){
          names( qformula)[2]<-self$name
          if(self$is_aggregation)qformula[[1]]<-expr(MELT)
          if(!self$is_aggregation)qformula[[1]]<-expr(STACK)
        }
        if(!isTRUE(self$is_aggregation)&!self$is_variable_stack){
          if(!expr_has_named_bracket( qformula,".D",skip_first = F)){

            eval(expr(self$data[,`:=`(!!self$symbol,!!qformula)]))
            private$.has_been_evaluated=TRUE
            qformula=self$symbol
          }
        }
        tmp=list(qformula)
        names(tmp)<-as.character(self$symbol)
        return(tmp)
      }
    },
    query_formula=function(value){
      if(missing(value)){
        return(self$formula[[1]])
      }
    },
    by_formula=function(value){
      if(missing(value)){
        if(is.null(private$.by_formula))return()
        return(private$.by_formula())
      }

      if(!is_function(value)&!is.null(value))
        stop("by_formula must be a function")
      private$.by_formula<-value

    },
    by=function(value){
      if(missing(value)){
        if(is.null(private$.by))return()
        return(private$.by())
      }

      if(!is_function(value)&!is.null(value))
        stop("Input must be a function")
      private$.by<-value
    },
    by_list=function(value){
      if(missing(value)){
        tmp<-self$by_formula %>% as.list()

        by_names=as.character(tmp[-1])
        #if(l(by_names)==0)return()
        by_list=as.list(self$by)[-1]
        names(by_list)= by_names

        BY<-c(by_list,self$exclude_LOD_groups)
        BY<-BY[!duplicated(names(BY))]
        return(BY)
      }
    },
    BY=function(value){
      if(missing(value)){
        by_list= self$by_list
        return(expr(.(!!!by_list)))
      }
    },
    I_formula=function(value){
      if(missing(value)){
        if(is.null(private$.I_formula))return()
        return(private$.I_formula())
      }
      if(!is_function(value)&!is.null(value))
        stop("Input must be a function")

      private$.I_formula<-value
    },
    I=function(value){
      if(missing(value)){
        if(is.null(private$.I))return()
        return(private$.I())
      }

      if(!is_function(value)&!is.null(value))
        stop("Input must be a function")
      private$.I<-value
    },
    J_edit=function(value){
      if(missing(value)){
        deparse(self$visible_J)
      }
    },
    name=function(value){
      if(missing(value)){
        if(is(private$.name,'function'))
          return(private$.name())
        return(private$.name)
      }
      if(is(value,'function')){
        if(!is.character(value()))stop("Name must be class character")
        private$.name<-value
        private$.name_fn<-value
      }else{
        if(!is.character(value))stop("Name must be class character")
        private$.name<-value
      }
    },
    name_fn=function(value){
      if(missing(value)){
        tmp= list(function() self$name)
        names(tmp)=self$var_id
        return(tmp)

      }
      stop("name_fn is read only")
    },
    source_name=function(value){
      if(missing(value)){
        if(is.null(private$.name_fn))return(self$name)
        return( private$.name_fn())

      }
      stop("source_name is read only")
    },
    symbol=function(value){
      if(missing(value)) return(parse_expr(glue('`{self$name}`')))
    },
    var_id_sym=function(value){
      if(missing(value)) return(parse_expr(glue('`{self$var_id}`')))
    },
    var_id=function(value){
      if(missing(value)){
        return(private$.var_id)
      }
      private$.var_id=value
    },
    creation_id=function(value){
      if(missing(value)){
        return(private$.creation_id)
      }
      if(is.null(private$.creation_id)){
        if(!is_integerish(value))stop('creation_id must be an integer')
        private$.creation_id=value
        return()
      }
      if(!is.null(private$.creation_id))stop('creation_id can only be set on init')
    },
    is_fixed=function(value){
      if(missing(value)){
        any(
          !is.null(private$.by),
          !is.null(private$.exclude)
        )&self$has_include==F
      }
    },
    is_variable_stack=function(value){
      if(missing(value)){
        return(is_call(self$hidden_J_formula,c("GROUP")))
      }
      stop("is_variable_stack is read-only")
    },
    is_variable_melt=function(value){
      if(missing(value)){
        return(self$is_variable_stack&isTRUE(self$is_aggregation))
      }
      stop("is_variable_melt is read-only")
    },
    is_stacked_variable_names=function(value){
      jtmp=self$J
    if(!is_call(jtmp,"as_constant"))return(FALSE)
      jtmp[[2]]=='No {self$name} values'
    },
    has_I=function(value){
      if(missing(value)){
        !is.null(private$.I)
      }
    },
    is_source_variable=function(value){
      if(missing(value)){
        return(private$.is_source_variable)
      }
    },
    has_include=function(value){
      if(missing(value)){
        if(self$is_source_variable)return(FALSE)
        return(self$has_by&self$SD_formula[[2]]==sym('.SD'))
      }
    },
    has_exclude=function(value){
      if(missing(value)){
        if(self$is_source_variable)return(FALSE)
        return(!is.null(private$.exclude))
      }
    },
    has_by=function(value){
      if(missing(value)){
        if(self$is_source_variable)return(FALSE)
        return(!is.null(private$.by))
      }
    },
    include=function(value){
      if(missing(value)){
        if(self$has_include==F)return()
        return(private$.by())
      }
      stop("Include is read only")

    },
    include_formula=function(value){
      if(missing(value)){
        if(self$has_include==F)return()
        return(private$.by_formula())
      }
      stop(" include_formula is read only")
    },
    exclude=function(value){
      if(missing(value)){
        if(is.null(private$.exclude))return()
        return(private$.exclude())
      }

      if(!is_function(value)&!is.null(value))
        stop("Input must be a function")
      private$.exclude<-value
    },
    exclude_formula=function(value){
      if(missing(value)){
        if(is.null(private$.exclude_formula))return()
        return(private$.exclude_formula())
      }

      if(!is_function(value)&!is.null(value))
        stop("Input must be a function")
      private$.exclude_formula<-value
    },
    exclude_LOD_groups=function(value){
      if(missing(value)){
        if(!self$has_exclude)return(NULL)
        BY=self$current_query_groups
        exclude<- self$exclude
        if(is_integerish(exclude)){
          return(BY[-exclude])
        }
        exclude_names<-as.list(exclude)[-1] %>%unlist() %>% as.character()
        out<-BY[names(BY)%nin%exclude_names]

        out
      }

    },
    include_LOD_groups=function(value){
      if(missing(value)){
        if(!self$has_include)NULL
        include_names<-as.list(self$include)[-1] %>%unlist() %>% as.character()
        include_formula<-as.list(self$include_formula)[-1]
        names(include_formula)<-include_names
        out<-include_formula[include_names%nin%names(self$current_query_groups)]
        return(expr(.(!!!out)))
      }

    },
    on=function(value){
      if(missing(value)){
        if(!self$is_fixed)return(maybe_missing(missing_arg()))
        GRPS<-self$current_query_groups
        BY<-self$by_list
        if(any(names( BY)%in% names(  GRPS))){
          on<-BY[(names( BY)%in% names(  GRPS))]
          on=syms(names(on))
          return(expr(.(!!!on)))
        }
        return(maybe_missing(missing_arg()))
      }
    },
    group_join=function(value){
      if(missing(value)){
        if(is_missing(self$on))
          return(maybe_missing(missing_arg()))
        return(expr(.BY))
      }
    },
    fixed_formula=function(value){
      if(missing(value)){
        if(!self$is_fixed)return(NULL)


        BY=self$BY
        J=self$J
        I=self$I

        jtmp=expr(.D[,!!J,by=!!BY])
        if(!is.null(I))jtmp[[3]]<-I

        return(  jtmp)
      }
    },
    data=function(value){
      if(missing(value)){
        if(is.null(private$.data))return()
        return(private$.data())
      }

      if(!is_function(value)&!is.null(value))
        stop("Input must be a function")
      private$.data<-value
    },
    data_icon=function(value){
      if(missing(value)){
        proto=self$prototype
       if(is_data_type(proto,'duration'))
         return('timer-sand')
        if(is_data_type(proto,'date_time'))
          return('calendar-clock')
        if(is_data_type(proto,'date'))
          return('calendar-blank-outline')
        if(is_data_type(proto,'ordered'))
          return('sort-variant')
        if(is_data_type(proto,'logical'))
          return('plus-minus')
        if(is_data_type(proto,'identity'))
          return('alphabetical')
        return('numeric')
      }
      stop("`icon` is read only")
    },
    folder_group=function(value){
      if(missing(value)){
        return(private$.folder_group)
      }
      if(is.null(value)){
        private$.folder_group<-NULL
        return(invisible(NULL))
        }
      tmp<-assert_class(value,classes="VariableFolder")
      private$.folder_group<-tmp
    },
    folder_name=function(value){
      if(missing(value)){
       return(self$folder_group$name)
      }
      stop('"folder_name" is read-only')
    },
    folder_id=function(value){
      if(missing(value)){
        return(self$folder_group$folder_id)
      }
      stop('"folder_id" is read-only')
    },
    folder_id_name=function(value){
      if(missing(value)){
        if(self$folder_name%in%c("dimensions","measures"))
          return("none")
        return(paste0(self$folder_id,":::",self$folder_name))
      }
      stop('"folder_id_name" is read-only')
    },
    exists_in_data=function(value){
      if(missing(value)){
        name_test=self$name%in%names(self$data)
        if(name_test==FALSE){
          private$.has_been_evaluated=FALSE
        }
        return( name_test)
      }
      stop("exist_in_data is read only")
    },
    pill_data=function(value){
      if(missing(value)){
      out<-list(id=self$var_id,
      label=self$name,
      type=self$variable_type,
      folder=self$folder_id_name,
      icon=self$data_icon,
      is_hierachy=self$is_hierachy,
      data=drop_nulls(list(
           action_submenu_aggregation=self$is_aggregation==FALSE,
           action_edit=self$is_source_variable==FALSE,
           action_delete=self$is_source_variable==FALSE,
           conversion=self$default_conversion%or%NA_character_,
           conversion_opts=self$conversion_opts%or%NA_character_,
           hierachy_n=self$hierachy_n,
           dataset_id=self$dataset_id,
           option_aggregation=self$default_aggregation,
           action_number_format=self$is_class_numeric,
           action_label_format=self$is_class_identity,
           action_date_format=self$is_class_date,
           action_order_props=self$is_class_identity,
           action_shape_props=self$is_class_identity)

           ))
      return(out)
      }
      stop('"pill_data" is read-only')
    },
    dataset_id= function(value) {
      if (missing(value)) {
        return(private$.dataset_id)
      }
      stop('dataset_id is read only')

    },
    variable_type=function(value){
      if(missing(value)){
        if(self$is_class_identity|self$is_class_date)
                 return("dimension")
               else
                 return('measure')
    }
    stop('"variable_type" is read-only')
   }
  )
)

