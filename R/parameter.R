parameter <-  R6::R6Class(
  "parameter",
  public = list(
    name = NULL,
    class = NULL,
    formula = NULL,
    value=NULL,
    data_type=NULL,
    range=NULL,
    is_aggregation=F,
    initialize = function(name = NULL,
                          value = NA,
                          class = NULL,
                          tags = NULL,
                          formula = NULL,
                          range=NULL,
                          is_aggregation=F) {
      self$name <-  name
      self$class <-   private$.class(value)
      if(is.na(value))self$class="unknown"
      self$value <-value
      self$alias <- alias
      self$data_type <-  data_type
      self$nformat=format
      self$formula=formula
      if(is.null(formula)) self$formula <-  paste0("`",name,"`")
      if(is.null(formula_edit)) self$formula_edit <-  paste0("`",name,"`")
      private$.fields =unique( range)
      self$tags = tags
      self$groups = groups
      self$is_aggregation=is_aggregation
      rNFomatter <- newR6("rNFomatter", comma_format)
      formatter=rNFomatter$new()
      formatter$accuracy=accuracy
      formatter$scale = scale
      formatter$prefix = prefix
      formatter$suffix= suffix
      self$formatter =  formatter

      rNumericInputIcon <- newR6("rNFomatter", numericInputIcon)
      NumericInput<-rNumericInputIcon$new()
      NumericInput$value<-value

      NumericInput$step=step
      self$min<-range[1]
      self$max<-range[l(range)]
      self$step=step
      self$ui=NumericInput
      private$.var_id=private$.create_unique_id()
    },
    format=function(data){
      if(!is.numeric(data))return(data)
      formatter=eval(self$formatter$call)
      formatter(data)
    },
    render_ui=function(input_id){

      prefix=list(self$formatter$prefix)
      suffix=list(self$formatter$suffix)

      if(suffix=="")suffix=list(NULL)
      if(prefix=="")prefix=list(NULL)
      icon=c(prefix,suffix)
      self$ui$icon<-icon
      self$ui$label<-self$name
      self$ui$value = self$value*self$formatter$scale
      self$ui$inputId<-input_id
      #self$formula<-glue('input$`{self$name}`')
      eval(self$ui$call)
    }),
  private = list(
    .fields =NULL,
    .create_unique_id=function(){
      from=c(LETTERS,letters,as.character(1:9))
      paste0(sample(from,size=30,replace=T),collapse = "")
    },
    .var_id=NULL,
    .class=function(x){
      if(any(class(x)%in%c("numeric","integer")))return("numeric")
      return("character")
    }
  ),
  active=list(
    aggregation=function(value){
      if(missing(value)){
        return("")
      }
    },
    var_id=function(value){
      if(missing(value)){
        private$.var_id
      }
    },
    accuracy =function(value){
      if(missing(value)){
        self$formatter$accuracy
      }else{
        self$formatter$accuracy<-value
      }
    },
    background_color =function(value){
      if(missing(value)){
        if(self$data_type=="parameter")return("#bbd7fc")
        if(self$data_type=="scenerio")return('#fcdb03')
        if(self$class=="numeric")return("#bbd7fc")
        if(self$class=="factor")return("#c2ffd5")
        if(self$class=="character")return("#c2ffd5")
        return('#fcdb03')
      }
    },
    shelf_ui =function(value){
      if(missing(value)){

        if(self$class=="numeric")return("param-ui")

      }
    },
    scale =function(value){
      if(missing(value)){
        self$formatter$scale
      }else{
        self$formatter$scale<-value
      }
    },
    prefix =function(value){
      if(missing(value)){
        self$formatter$prefix
      }else{
        self$formatter$prefix<-value
      }
    },
    suffix =function(value){
      if(missing(value)){
        self$formatter$suffix
      }else{
        self$formatter$suffix <-value
      }
    },
    big.mark  =function(value){
      if(missing(value)){
        self$formatter$big.mark
      }else{
        self$formatter$big.mark<-value
      }
    },
    decimal.mark  =function(value){
      if(missing(value)){
        self$formatter$decimal.mark
      }else{
        self$formatter$decimal.mark<-value
      }
    },
    fields=function(value){
      if(missing(value)){
        list(private$.fields)
      }else{
        private$.fields<-value
      }
    })
)
