cCat=function(x){
  list(expr(c(!!!x))) %>% exprs_deparse() %>% as_glue()
}

new_S3_methods=function(fname,na.rm=T,dtypes,classes=NULL){
  classes=c('default',classes)
  if(na.rm){
   out= glue("#' @export\n &&fname&&<-function(x,na.rm=T,...){
  ex <- eval_tidy(enquo(x))
  UseMethod('&&fname&&',ex)
  }\n#' @export\n&&fname&&.&&classes&&<-function(x,na.rm=TRUE,...){
        assert_dtype(x,dtype=&&cCat(dtypes&&,.vname=enexpr(x),.fname='&&fname&&')
       out<-&&tolower(fname)&&(x,na.rm=TRUE,...)
      class(out)<-c(class(out),'aggregate')
      out
        }
       ",.open="&&",.close="&&")%sep%"\n"
  }else{
   out= glue("&&fname&&<-function(x,...){
  ex <- eval_tidy(enquo(x))
  UseMethod('&&fname&&',ex)
  }\n&&fname&&.&&classes&&<-function(x,...){
        assert_dtype(x,dtype=&&cCat(dtypes)&&,.vname=enexpr(x),.fname='&&fname&&')
       &&tolower(fname)&&(x,...)
        }
       ",.open="&&",.close="&&")%sep%"\n"
  }
  clipr::write_clip(out)
  sDevTools::g_success("Copied to clipboard")
}
#new_S3_methods(toupper(self$agg),na.rm=TRUE,dtypes=c("unaggregated","continuous"))

as_constant=function(x){
  x
}
