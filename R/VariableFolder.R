VariableFolder = R6::R6Class(
  'VariableFolder',
  public = list(
    initialize = function(name,type,id=NULL) {
      assert_choice(type,c("measure",'dimension'))
      assert_string(name)
      private$.type=type
      private$.name=name
      private$.folder_id=id%or%paste0('folder_',create_unique_id(10))
    }

  ),
  private = list(
    .type=NULL,
    .name=NULL,
    .folder_id=NULL
  ),
  active = list(
    name=function(value){
      if(missing(value)){
        return(private$.name)
      }
      private$.name=assert_string(value)
    },
    folder_id=function(value){
      if(missing(value)){
        return(private$.folder_id)
      }
     stop('"folder_id" is read only')
    },
    type=function(value){
      if(missing(value)){
        return(private$.type)
      }
      stop('folder "type" is read only')
    }

  )
)
