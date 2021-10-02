#' @export
Datasets = R6::R6Class(
  'Datasets',
  public = list(
    initialize = function(datasets=NULL) {
      if(nnull(datasets)){

        if(!is.list(datasets)){
          self$new_dataset(datasets)
        }else{
        private$.datasets=lapply(datasets,  self$new_dataset)
        }
      }
    },
    new_dataset=function(dataset){
      if(!is(dataset,'dynDataTable')){
        dataset=dynDT(dataset)
      }
      id=dataset$dataset_id
      private$.datasets[[id]]<-dataset
    }),
  private = list(
    .datasets=list()
  ),
  active = list(
    datasets = function(value) {
      if (missing(value)) {
        return(private$.datasets)
      }
      stop("datasets is read only. Use `new_dataset` method to add new data")

    }
  )
)
