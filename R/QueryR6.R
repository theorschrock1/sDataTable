#' @export
Query <-
  R6::R6Class(
  'Query',
  public = list(
    initialize=function(datasets=NULL,current_dataset=NULL){
      private$.datasets=datasets
      private$.current_dataset<-current_dataset
      if(is.null(current_dataset))
        private$.current_dataset<-
          self$datasets[[1]]$dataset_id

    }
    ),
    private = list(
      .current_query_groups=NULL,
      .current_query_prereqs=NULL,
      .current_filter=NULL,
      .current_J_filter=NULL,
      .current_agg_filter=NULL,
      .current_J=NULL,
      .current_by=NULL,
      .agg_filter_ranges=NULL,
      .pre_filter_ranges=NULL,
      .current_dataset=NULL,
      .datasets=NULL
    ),
active = list(
  pre_filter=function(value){
    if (missing(value)) {
      if(is_null(self$prv$.current_filter)){
        return(NULL)
      }
      return(self$prv$.current_filter)
    }
    if (is_empty(value)) {

      self$prv$.current_filter <- NULL
      self$prv$.pre_filter_ranges<- NULL
      return()
    }

    tmp=lapply(value,function(x)parse_expr(x$var))
    forms=self$active_data$parse_DT_list( tmp)
    value=map2(value,forms,function(x,i){x$formula=i
    return(x)})
    active_data<-self$active_data
    self$prv$.pre_filter_ranges=function(x=NULL){
      assert_choice(x,names( forms),null.ok=TRUE)
      jnames=as.list(names( forms))
      names(jnames)<-jnames
      if(nnull(x)){
        forms=forms[x]
        jnames=x
        }
      query<-active_data$build_query_expr(J=forms)
      query[[2]]<-active_data$pre_data
      data=active_data$eval_data_expr(query)
      if(nnull(x))
        return(d_range(data[[x]]))
      lapply(jnames,function(x)d_range(data[[x]]))
    }
    ranges=self$prv$.pre_filter_ranges()
    value=map(value,function(x){
      if(is.null(x$range))
        x$range=self$prv$.pre_filter_ranges(x$var)
      return(x)
    })

    self$prv$.current_filter=lapply(value,build_filter) %>% reduce(function(x,y)expr(!!x&!!y))

  },
  I=function(value){
    if (missing(value)) {
      if(is_null(self$J_filter))
        return(NULL)
      #pre=self$pre_filter
      # J=self$J_filter
      #by=self$by


      #vnames<-as.list(names(self$J_filter))
      #names(vnames)<-names(self$J_filter)

      if(is_missing(self$agg_filter))
        return(NULL)
      tmp<-self$active_data$build_query_expr(J=self$J_filter,BY=self$by,post_filter = self$agg_filter)
      return(insert_data_expr(tmp,pre=sym('.D')))

    }
    stop('I is read only')
  },
  agg_filter=function(value){
    if (missing(value)) {
      if(is_null(self$prv$.current_agg_filter)){
        return(missing_arg())
      }
      out=self$prv$.current_agg_filter  %>% reduce(function(x,y)expr(!!x&!!y))
      return(out)
    }
    if (is_empty(value)) {
      self$J_filter <- NULL
      self$prv$.current_agg_filter <- NULL
      self$prv$.agg_filter_ranges<- NULL
      return()
    }
    self$J_filter<-lapply(value,function(x)parse_expr(x$var))
    self$prv$.current_agg_filter<-
      drop_nulls(lapply(value,build_filter,use_formula=FALSE)) %or%NULL
    active_data=self$active_data
    self$prv$.agg_filter_ranges <- function(x=NULL) {
      assert_choice(x,names(self$J_filter),null.ok=TRUE)
      query <- active_data$build_query_expr(J = self$J_filter,
                                BY = self$by,
                                pre_filter = self$pre_filter) %>%
        insert_data_expr(pre=active_data$pre_data)
      data = active_data$eval_data_expr(query)
      if(is.null(x)){
        vnames<-as.list(names(self$J_filter))
        names(vnames)<-names(self$J_filter)
        return(lapply(vnames, function(x)
          d_range(data[[x]])))
      }
      d_range(data[[x]])
    }
  },
  current_query_dimensions=function(value){
    if(missing(value)){
      return(
        self$active_data$current_query_dimensions
      )
    }
    self$prv$.current_query_groups<-NULL
    if(!is.null(value)){
      self$active_data$current_query_dimensions<-
        lapply(value,self$active_data$eval_pointers)
    }
  },
  naming_functions = function(value) {
    if (missing(value)) {
    out<-unique(c(names(self$prv$.current_by),names(self$prv$.current_J)))
    if(len0(out))
      return(NULL)
    return(self$active_data$get_name_fns(parse_exprs(out)))
    }
    stop("naming_functions is read only")

  },
  J = function(value) {
    if (missing(value)) {
     if(is.null(self$prv$.current_J))
        return(list(` `=''))
      return(self$prv$.current_J)
    }
    if(is.null(value)){
      self$prv$.current_J=NULL
    }else{
    self$prv$.current_J<-self$active_data$parse_DT_list(assert_exprs(value))
    }
  },
  J_filter = function(value) {
    if (missing(value)) {
      return(self$prv$.current_J_filter)
    }
    if(is.null(value)){
      self$prv$.current_J_filter<-NULL
    }else{
    self$prv$.current_J_filter<-self$active_data$parse_DT_list(assert_exprs(value))
    }
  },
  by = function(value) {
    if (missing(value)) {
      by=self$prv$.current_by
      return( self$prv$.current_by)
    }
    if(is.null(value)){
      self$prv$.current_by=NULL
    }else{
    self$prv$.current_by<- self$active_data$parse_DT_list(rev(assert_exprs(value)))
}
  },
  pre_filter_ranges = function(value) {
    if (missing(value)) {
      if(is.null(self$prv$.pre_filter_ranges))
        return(function(){NULL})
      return(self$prv$.pre_filter_ranges)
    }
    stop("pre_filter_ranges  is read only")

  },
  agg_filter_ranges = function(value) {
    if (missing(value)) {
      if(is.null(self$prv$.agg_filter_ranges))
        return(function(){NULL})
      return(self$prv$.agg_filter_ranges)
    }
    stop("agg_filter_ranges is read only")

  },
  data=function(value){
    if(missing(value)){
      if(self$is_J_and_BY_null)
        return(list(data.table=data.table()))
      active_data=self$active_data
      var_names_fn<-self$naming_functions

      all_vars=var_names_fn$name
      ids=var_names_fn$var_id
      rows=NULL

      data_fn=function(rows=NULL){

        query<-active_data$build_query_expr(
            I = self$I,
            J = self$J,
            BY = self$by,
            pre_filter = self$pre_filter
          ) %>%
          insert_data_expr(pre=active_data$pre_data)

        #query<-expr(data[setnames=list(old=!!all_vars,new=!!ids,skip_absent=F)])

        #query[[2]]<-tmp

        active_data$eval_data_expr(query)

      }


      #is_quosure(data_q)
      #eval_tidy(data_q)
      #list(data= data_fn,var_names_fn =var_names_fn)
      out=dynDataTable$new(data= data_fn())

      out=dynDataTable$new(data= data_fn(),var_names_fn =var_names_fn)

      return(invisible(out))
    }
    stop("current query is read-only")
  },
  dataDT=function(value){
    if(missing(value)){
      if(self$is_J_and_BY_null)
        return(list(data.table=data.table()))
      active_data=self$active_data




      query<-active_data$build_query_expr(
          I = self$I,
          J = self$J,
          BY = self$by,
          pre_filter = self$pre_filter
        ) %>%
          insert_data_expr(pre=active_data$pre_data)




        return(active_data$eval_data_expr(query))

    }
    stop("current query is read-only")
  },
  dataId = function(value) {
    if (missing(value)) {
        return(self$prv$.current_dataset)
    }
   self$prv$.current_dataset=value
  },
  datasets = function(value) {
    if (missing(value)) {
      return(self$prv$.datasets$datasets)
    }
    stop("datasets is read only")

  },
  active_data = function(value) {
    if (missing(value)) {
        return(self$datasets[[self$dataId]])
    }
    stop("active_data is read only")

},
  prv = function(value) {
    if (missing(value)) {
        return(private)
    }
    private<-value

  },
  query_expr = function(value) {
    if (missing(value)) {
      active_data=self$active_data
        return(active_data$build_query_expr(
          I = self$I,
          J = self$J,
          BY = self$by,
          pre_filter = self$pre_filter
        ) %>%
          insert_data_expr(pre=active_data$pre_data)
        )
    }
    stop("query_expr is read only")

},
  is_J_and_BY_null = function(value) {
    if (missing(value)) {
        return(len0(unique(c(self$prv$.current_by,self$prv$.current_J))))
    }
    stop("is_J_and_BY_null is read only")

}

))
