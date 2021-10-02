
STACK=function(...){
  assert_data_stackable(...)
  dtypes<-sapply(.(...),data_type)
  if(any(grepl("date_time",dtypes)))out<-proto$date_time
  if(any(grepl("date$",dtypes)))out<-proto$date
  if(any(grepl("integer",dtypes)))out<-proto$integer
  if(any(grepl("float",dtypes)))out<-proto$float
  if(any(grepl("identity",dtypes)))out<-proto$identity
  if(all(grepl("factor",dtypes)))out<-proto$factor
  if(all(grepl("ordered",dtypes)))out<-proto$ordered
  if(all(grepl("units",dtypes)))out<-proto$units
  if(all(grepl("duration",dtypes)))out<-proto$duration
  #class(out)<-c("unaggregated",class(out))
  out
}
MELT=function(...){
  assert_data_meltable(...)
  out=list("grouped aggregates")
  class(out)<-c("grouped aggregates")
  out
}
#' @export
GROUP=function(...){

  UseMethod("GROUP")
}
#' @export
GROUP.default=function(...){
  STACK(...)
}
#' @export
GROUP.aggregate=function(x,...){
  MELT(x,...)
}
#' @export
SUM<-function(x,na.rm=T,...){
  if(missing(x))
    g_stop('Missing argument in `SUM()`')
  ex <- eval_tidy(enquo(x))
  UseMethod('SUM',ex)
}
#' @export
SUM.default<-function(x,na.rm=TRUE,...){
  assert_dtype(x,dtype=c("unaggregated","continuous"),combine = '&',.vname=enexpr(x),.fname='SUM')
  out<-sum(x,na.rm=TRUE,...)
  class(out)<-c(class(out),'aggregate')
  out
}
#' @export
IF_ELSE<-function(condition, true, false, missing = NULL){

  fifelse(test = c(-5L:5L < 0L, NA), yes = 1L, no = 0L, na = 2)

}
#' @export
N<-function(x,na.rm=T,...){
  ex <- eval_tidy(enquo(x))
  UseMethod('N',ex)
}
#' @export
N.default<-function(x,na.rm=TRUE,...){
  assert_dtype(x,dtype=c("unaggregated","continuous"),.vname=enexpr(x),.fname='N')
  out<-n(x,na.rm=TRUE,...)
  class(out)<-c(class(out),'aggregate')
  out
}
#' @export
N_MISSING<-function(x,na.rm=T,...){
  ex <- eval_tidy(enquo(x))
  UseMethod('N_MISSING',ex)
}
#' @export
N_MISSING.default<-function(x,na.rm=TRUE,...){
  assert_dtype(x,dtype=c("unaggregated","continuous"),.vname=enexpr(x),.fname='N_MISSING')
  out<-n_missing(x,na.rm=TRUE,...)
  class(out)<-c(class(out),'aggregate')
  out
}
#' @export
NTH<-function(x,na.rm=T,...){
  ex <- eval_tidy(enquo(x))
  UseMethod('NTH',ex)
}
#' @export
NTH.default<-function(x,na.rm=TRUE,...){
  assert_dtype(x,dtype=c("unaggregated","continuous"),.vname=enexpr(x),.fname='NTH')
  out<-nth(x,na.rm=TRUE,...)
  class(out)<-c(class(out),'aggregate')
  out
}
#' @export
N_DISTINCT<-function(x,na.rm=T,...){
  ex <- eval_tidy(enquo(x))
  UseMethod('N_DISTINCT',ex)
}
#' @export
N_DISTINCT.default<-function(x,na.rm=TRUE,...){
  assert_dtype(x,dtype=c("unaggregated","continuous"),.vname=enexpr(x),.fname='N_DISTINCT')
  out<-n_distinct(x,na.rm=TRUE,...)
  class(out)<-c(class(out),'aggregate')
  out
}
#' @export
N_DUPLICATED<-function(x,na.rm=T,...){
  ex <- eval_tidy(enquo(x))
  UseMethod('N_DUPLICATED',ex)
}
#' @export
N_DUPLICATED.default<-function(x,na.rm=TRUE,...){
  assert_dtype(x,dtype=c("unaggregated","continuous"),.vname=enexpr(x),.fname='N_DUPLICATED')
  out<-n_duplicated(x,na.rm=TRUE,...)
  class(out)<-c(class(out),'aggregate')
  out
}
#' @export
FIRST<-function(x,na.rm=T,...){
  ex <- eval_tidy(enquo(x))
  UseMethod('FIRST',ex)
}
#' @export
FIRST.default<-function(x,na.rm=TRUE,...){
  assert_dtype(x,dtype=c("unaggregated","continuous"),.vname=enexpr(x),.fname='FIRST')
  out<-first(x,na.rm=TRUE,...)
  class(out)<-c(class(out),'aggregate')
  out
}
#' @export
LAST<-function(x,na.rm=T,...){
  ex <- eval_tidy(enquo(x))
  UseMethod('LAST',ex)
}
#' @export
LAST.default<-function(x,na.rm=TRUE,...){
  assert_dtype(x,dtype=c("unaggregated","continuous"),.vname=enexpr(x),.fname='LAST')
  out<-last(x,na.rm=TRUE,...)
  class(out)<-c(class(out),'aggregate')
  out
}
#' @export
IQR<-function(x,na.rm=T,...){
  ex <- eval_tidy(enquo(x))
  UseMethod('IQR',ex)
}
#' @export
IQR.default<-function(x,na.rm=TRUE,...){
  assert_dtype(x,dtype=c("unaggregated","continuous"),.vname=enexpr(x),.fname='IQR')
  out<-iqr(x,na.rm=TRUE,...)
  class(out)<-c(class(out),'aggregate')
  out
}
#' @export
MAD<-function(x,na.rm=T,...){
  ex <- eval_tidy(enquo(x))
  UseMethod('MAD',ex)
}
#' @export
MAD.default<-function(x,na.rm=TRUE,...){
  assert_dtype(x,dtype=c("unaggregated","continuous"),.vname=enexpr(x),.fname='MAD')
  out<-mad(x,na.rm=TRUE,...)
  class(out)<-c(class(out),'aggregate')
  out
}
#' @export
MEAN<-function(x,na.rm=T,...){
  ex <- eval_tidy(enquo(x))
  UseMethod('MEAN',ex)
}
#' @export
MEAN.default<-function(x,na.rm=TRUE,...){
  assert_dtype(x,dtype=c("unaggregated","continuous"),.vname=enexpr(x),.fname='MEAN')
  out<-mean(x,na.rm=TRUE,...)
  class(out)<-c(class(out),'aggregate')
  out
}
#' @export
MEDIAN<-function(x,na.rm=T,...){
  ex <- eval_tidy(enquo(x))
  UseMethod('MEDIAN',ex)
}
#' @export
MEDIAN.default<-function(x,na.rm=TRUE,...){
  assert_dtype(x,dtype=c("unaggregated","continuous"),.vname=enexpr(x),.fname='MEDIAN')
  out<-median(x,na.rm=TRUE,...)
  class(out)<-c(class(out),'aggregate')
  out
}
#' @export
MAX<-function(x,na.rm=T,...){
  ex <- eval_tidy(enquo(x))
  UseMethod('MAX',ex)
}
#' @export
MAX.default<-function(x,na.rm=TRUE,...){
  assert_dtype(x,dtype=c("unaggregated","continuous"),.vname=enexpr(x),.fname='MAX')
  if(all(is.na(x))){
    out<-c(-Inf)
    class(out)<-c(class(out),'aggregate')
    return(out)
  }
  out<-max(x,na.rm=TRUE,...)
  class(out)<-c(class(out),'aggregate')
  out
}
#' @export
MIN<-function(x,na.rm=T,...){
  ex <- eval_tidy(enquo(x))
  UseMethod('MIN',ex)
}
#' @export
MIN.default<-function(x,na.rm=TRUE,...){
  assert_dtype(x,dtype=c("unaggregated","continuous"),.vname=enexpr(x),.fname='MIN')

  if(all(is.na(x))){
    out<-Inf
    class(out)<-c(class(out),'aggregate')
    return(out)
  }
  out<-min(x,na.rm=TRUE,...)
  class(out)<-c(class(out),'aggregate')
  out
}
# SUM<-function(x,na.rm=T,...){
#   ex <- eval_tidy(enquo(x))
#   UseMethod('SUM',ex)
# }
# SUM.default<-function(x,na.rm=TRUE,...){
#   assert_dtype(x,dtype=c("unaggregated","continuous"),.vname=enexpr(x),.fname='SUM')
#   out<-sum(x,na.rm=TRUE,...)
#   class(out)<-c(class(out),'aggregate')
#   out
# }
SD<-function(x,na.rm=T,...){
  ex <- eval_tidy(enquo(x))
  UseMethod('SD',ex)
}
#' @export
SD.default<-function(x,na.rm=TRUE,...){
  assert_dtype(x,dtype=c("unaggregated","continuous"),.vname=enexpr(x),.fname='SD')
  out<-sd(x,na.rm=TRUE,...)
  class(out)<-c(class(out),'aggregate')
  out
}
#' @export
VAR<-function(x,na.rm=T,...){
  ex <- eval_tidy(enquo(x))
  UseMethod('VAR',ex)
}
#' @export
VAR.default<-function(x,na.rm=TRUE,...){
  assert_dtype(x,dtype=c("unaggregated","continuous"),.vname=enexpr(x),.fname='VAR')
  out<-var(x,na.rm=TRUE,...)
  class(out)<-c(class(out),'aggregate')
  out
}
#' @export
COR<-function(x,na.rm=T,...){
  ex <- eval_tidy(enquo(x))
  UseMethod('COR',ex)
}
#' @export
COR.default<-function(x,na.rm=TRUE,...){
  assert_dtype(x,dtype=c("unaggregated","continuous"),.vname=enexpr(x),.fname='COR')
  out<-cor(x,na.rm=TRUE,...)
  class(out)<-c(class(out),'aggregate')
  out
}
#' @export
KURTOSIS<-function(x,na.rm=T,...){
  ex <- eval_tidy(enquo(x))
  UseMethod('KURTOSIS',ex)
}
#' @export
KURTOSIS.default<-function(x,na.rm=TRUE,...){
  assert_dtype(x,dtype=c("unaggregated","continuous"),.vname=enexpr(x),.fname='KURTOSIS')
  out<-kurtosis(x,na.rm=TRUE,...)
  class(out)<-c(class(out),'aggregate')
  out
}
#' @export
SKEWNESS<-function(x,na.rm=T,...){
  ex <- eval_tidy(enquo(x))
  UseMethod('SKEWNESS',ex)
}
#' @export
SKEWNESS.default<-function(x,na.rm=TRUE,...){
  assert_dtype(x,dtype=c("unaggregated","continuous"),.vname=enexpr(x),.fname='SKEWNESS')
  out<-skewness(x,na.rm=TRUE,...)
  class(out)<-c(class(out),'aggregate')
  out
}
#' @export
MOMENT<-function(x,na.rm=T,...){
  ex <- eval_tidy(enquo(x))
  UseMethod('MOMENT',ex)
}
#' @export
MOMENT.default<-function(x,na.rm=TRUE,...){
  assert_dtype(x,dtype=c("unaggregated","continuous"),.vname=enexpr(x),.fname='MOMENT')
  out<-moment(x,na.rm=TRUE,...)
  class(out)<-c(class(out),'aggregate')
  out
}
#' @export
YEAR<-function(x){
  ex <- eval_tidy(enquo(x))
  UseMethod('YEAR',ex)
}
#' @export
YEAR.default<-function(x){
  assert_dtype(x,dtype=c("unaggregated","date"),.vname=enexpr(x),.fname='YEAR')
  out<-data.table::year(x)
  out
}
#' @export
QUARTER<-function(x){
  ex <- eval_tidy(enquo(x))
  UseMethod('QUARTER',ex)
}
#' @export
QUARTER.default<-function(x){
  assert_dtype(x,dtype=c("unaggregated","date"),.vname=enexpr(x),.fname='QUARTER')
  x<-data.table::quarter(x)
  out<-factor(x,levels=1:4,labels = c("Q1","Q2","Q3","Q4"))
  out
}
#' @export
MONTH<-function(x){
  ex <- eval_tidy(enquo(x))
  UseMethod('MONTH',ex)
}
#' @export
MONTH.default<-function(x){
  assert_dtype(x,dtype=c("unaggregated","date"),.vname=enexpr(x),.fname='MONTH')
  x=data.table::month(x)
  out<-factor(x,labels = month.abb)
  out
}
#' @export
DAY<-function(x){
  ex <- eval_tidy(enquo(x))
  UseMethod('DAY',ex)
}
#' @export
DAY.default<-function(x){
  assert_dtype(x,dtype=c("unaggregated","date"),.vname=enexpr(x),.fname='DAY')
  out<-factor(day(x))
  out
}
#' @export
HOUR<-function(x){
  ex <- eval_tidy(enquo(x))
  UseMethod('HOUR',ex)
}
#' @export
HOUR.default<-function(x){
  assert_dtype(x,dtype=c("unaggregated","date_time"),.vname=enexpr(x),.fname='HOUR')
  out<-factor(hour(x))
  out
}
#' @export
MINUTE<-function(x,...){
  ex <- eval_tidy(enquo(x))
  UseMethod('MINUTE',ex)
}
#' @export
MINUTE.default<-function(x,...){
  assert_dtype(x,dtype=c("unaggregated", "date_time"),.vname=enexpr(x),.fname='MINUTE')
  factor(minute(x,...))
}
#' @export
SECOND<-function(x,...){
  ex <- eval_tidy(enquo(x))
  UseMethod('SECOND',ex)
}
#' @export
SECOND.default<-function(x,...){
  assert_dtype(x,dtype=c("unaggregated", "date_time"),.vname=enexpr(x),.fname='SECOND')
  factor(round(second(x,...)))
}
#' @export
DOW<-function(x,...){
  ex <- eval_tidy(enquo(x))
  UseMethod('DOW',ex)
}
#' @export
DOW.default<-function(x,...){
  assert_dtype(x,dtype=c("unaggregated", "date"),.vname=enexpr(x),.fname='DOW')
 factor(data.table::wday(x),labels="Mon",'Tue',"Wed","Thr",'Fri',"Sat",'Sun')
}
#' @export
WEEK<-function(x,...){
  ex <- eval_tidy(enquo(x))
  UseMethod('WEEK',ex)
}
#' @export
WEEK.default<-function(x,...){
  assert_dtype(x,dtype=c("unaggregated", "date"),.vname=enexpr(x),.fname='WEEK')
  factor(data.table::week(x,...))
}

YDAY<-function(x,...){
  ex <- eval_tidy(enquo(x))
  UseMethod('YDAY',ex)
}
YDAY.default<-function(x,...){
  assert_dtype(x,dtype=c("unaggregated", "date"),.vname=enexpr(x),.fname='YDAY')
  factor(data.table::yday(x,...))
}

