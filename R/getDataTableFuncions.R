#Get valid sDataTable functions
#' @export
getSDTfns=function(){
names(environment(MELT))%grep%'^[A-Z]+$'%NIN%'DT'
}
