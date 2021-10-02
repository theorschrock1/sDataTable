newDT<-function(x){
  assert(
    checkDataFrame(x),
    checkDataTable(x),
    checkMatrix(x)
  )

  DataTable$new(x)
}
