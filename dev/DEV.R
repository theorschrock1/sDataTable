#Check Usage -----
checks=checkPackageUsage()
sDevTools::runTests(package="sDataTable")
devtools::test()
xc<-sDevTools::runTests('sDataTable_main')

library(ggplot2)
library(sDataTable)
diamonds<-DT(diamonds)
diamonds[,Today:=Sys.Date()]
diamonds[,Yesterday:=Sys.Date()-1]
diamonds[,Now:=Sys.time()]


#d<-newDT(diamonds)
#d[,.N,by=cut]
d<-dynDT(diamonds)


d[,`is_cut_premium`:=cut=="Premium"]
#d<-d$data
self=Query$new(Datasets$new(d))
self$dataDT$data
self$J=exprs(SUM(carat))
self$by=exprs(color,cut,clarity)
self$pre_filter= value=list(list(var="cut",
                      range=c("Fair","Average","Good"),type='exact',exclude=FALSE))
self$agg_filter=
 list(list(var="SUM(carat)",range=c(.70,234),type='range',exclude=FALSE))
self$agg_filter
self$agg_filter_ranges("SUM(carat)")
names(self$J_filter)
self$by=exprs(clarity,color, cut)
self$data
fd<-function(i){
  tic.clearlog()
lapply(1:i,function(i){
  #tic()

  tic('self$active_data')
  active_data=self$active_data
  toc(log = TRUE, quiet = TRUE)
  # toc(log = TRUE, quiet = TRUE)
  #tic()
  tic('I')
  I = self$I
  toc(log = TRUE, quiet = TRUE)
  tic('J')
  J = self$J
  toc(log = TRUE, quiet = TRUE)
  tic('BY')
  BY = self$by
   toc(log = TRUE, quiet = TRUE)
  tic('pre_filter')
  pre_filter = self$pre_filter
   toc(log = TRUE, quiet = TRUE)
  tic('build_query_expr')
  query<-active_data$build_query_expr(
    I = I,
    J = J,
    BY =BY,
    pre_filter = pre_filter
  )
   toc(log = TRUE, quiet = TRUE)
  tic("insert_data_expr")
  query=insert_data_expr(query,pre=active_data$pre_data)
   toc(log = TRUE, quiet = TRUE)
  # toc(log = TRUE, quiet = TRUE)
#tic()
  tic("eval_data_expr")
   active_data$eval_data_expr(query)
    toc(log = TRUE, quiet = TRUE)
    tic("self$dataDT")
    self$dataDT
    toc(log = TRUE, quiet = TRUE)
    tic("self$data")
    self$data$data.table
    toc(log = TRUE, quiet = TRUE)
# toc(log = TRUE, quiet = TRUE)
return()
})
  out=lapply(tic.log(format = FALSE),as.data.table) %>% rbindlist()
  out[,avg_time:=toc - tic]
 out= out[,.(avg_time=mean(avg_time)),by=msg]
  out[,pot:=avg_time/sum(avg_time)*100]
  out
}

self$data$data.table

fd(100)[]

self$pre_filter=list(
            list(var="is_cut_premium",
                 range=NULL,type='exact',exclude=FALSE),
            list(var="cut",
                 range=NULL,type='exact',exclude=FALSE))

self$pre_filter_ranges('cut')

tmp<-self$data
tmp$print_data

self$J=exprs(SUM(carat),SUM(price))



print.dynDataTable(self)
is(self,"dynDataTable")
print(self)
l(unlist(tmp$name_fn))
c(data,var_names_fn) %<-%parent$current_query
self=dynDataTable$new()

build_filter(filter)
self=newDT(diamonds)

self[.D[,.(out=sum(carat)),by=.(cut)][out>10100],.(sum(price)),by=cut]

  self[,.(out=sum(carat)),by=.(cut)][out>10100]
self$vars[["var_Zqun8q9wnL"]]$is_class_numeric
folder_id=self$new_variable_folder('Folder',type='measure')
self$add_variable_to_folder(  self$vars[[1]]$var_id,folder_id)
c(mids,mdata)%<-%self$measure_pills
c(ids,data)%<-%self$dimension_pills

library(ggplot2)
diamonds<-DT(diamonds)
diamonds[,Today:=Sys.Date()]
diamonds[,Yesterday:=Sys.Date()-1]
diamonds[,Now:=Sys.time()]
self<-dynDT(diamonds)
self$measure_pills




self$get_pill_html(self$get_variable_id('Now'))
var_id<-self[,`time difference`:=Today-Yesterday]
self[cut=='Ideal',.(sum(Price)),by=.()]
id=self$add_variable_to_new_folder(self$get_variable_id('carat'))

self$add_variable_to_folder(variable_id =var_id,folder_id = id )
self$add_variable_to_folder(var_id,folder_id='measures')
self$get_attrs(vars=self$get_folder_variable_ids('measures' ) )

self$measure_vars
new_id<-self$duplicate_variable(var_id)
tmp=c("r","s","sd","a","S")
tmp[order(tmp)]
self$rename_object_by_id(new_id,"time3")
self$dimension_vars$name
is_data_type(self$vars$`6MwAklTHiN`$prototype,'duration')
add_snapshot_test('sdatatable_error_messages',{
  folder_id=self$new_variable_folder('df','measure')
  variable_id=self$vars[[1]]$var_id
  self$add_variable_to_folder(  variable_id,folder_id)
  })

c(ids,data)%<-%self$measure_pills
self$vars[[1]]$folder_group
pill_item
pill_folder=function(folder_name,pill_items){
  c(id,label)%<-%str_split(folder_name,":::")[[1]]

  out<-folder_collapse(inputId=ns(paste0('folder_',id)),label=label,
                       pill_card(inputId =ns(id),
                                 type='column',
                                 sort_ops = sortable_options(
                                   name = 'nav_dimensions',
                                   pull = 'clone',
                                   put = FALSE,
                                   dragClass = 'drag',
                                   sort = FALSE
                                 ),pill_items)
  )
  div(class='pill-folder',`data-id`=ns(id),out)
}
pill_shelf=function(ids,data){
  assert_list(ids)
  assert_list(data)
  out<-map2(ids,data,function(x,y){
    pill_item(id=x$id,label=x$label,type=unique(x$type),data=y)
  })
  folders<-out[names(out)!='none']
  fpills<-map2(names(folders),folders,pill_folder)
  base<-out$none
  fnames=lapply(names(folders),function(x)str_split(x,":::")[[1]][2]) %>% unlist()
  pnames<-c(ids$none$label,fnames)
  all_pills<-c(base,fpills)
  all_pills[order(pnames)]

}
pill_shelf(ids,data)
class(pill_items)
function(id, label, type = "measure",data=list()) {
  # Create pill items
  assert_character(id)
  assert_character(label, len=length(id))
  assert_list(data)
  assert_choice(type,c("measure",'dimension'))
  order=ifelse(type=='measure',1,0)

  assert_any(type, check_character(len = 1), check_character(len = length(id)))
  class=glue("pill-item pill-{type} order-{order}")
  data_attributes<-"`data-null`=NULL"
  if('id'%nin%names(data)){
    data$id=id
  }
  # print(data)
  if(length(data)>0){
    input_type=sapply(data,function(x){
      if(length(x)!=length(id))
        g_stop("length of data attributes not equal to length of ids")
      if(is.logical(x))return("checkbox")
      if(is.character(x))return('radio')
      if(is.numeric(x))return('action')
      g_stop('data attributes must be logical,numeric, or character')
    })
    data_init=lapply(data,function(x){
      if(is.logical(x))x<-js_logical(x)
      if(is.numeric(x))x<-as.character(x)
      x[is.na(x)]<-"NA"
      glue('"{x}"')
    })

    data_names<-glue('`data-{input_type}_{names(data)}`')
    data_names[data_names=='`data-radio_id`']<-'`data-id`'

    data_attributes=map2(data_names,data_init,function(x,y)paste0(x,"=",y)) %>%
      reduce(function(x,y){paste(x,y,sep=',')})
  }
  out <- expr_glue(tags$div(id="{id}",
                            {data_attributes},
                            class='{class}',
                            tags$span("{label}",class="pill-label",`data-id`="{id}"),
                            icon_mdi('menu-down',class='icon-right dropdown-handle float-right')
  )
  ) %>% exprs_eval()

  out
  # Returns: [html]
}

args2env(
pill_item(id=data$id,label=data$label,type=data$type,data=data$data,icon=data$icon)
)
assert_character(id)
assert_character(label, len=length(id))
assert_list(data)
assert_choice(type,c("measure",'dimension'))
order=ifelse(type=='measure',2,1)

assert_any(type, check_character(len = 1), check_character(len = length(id)))
class=glue("pill-item pill-{type} order-{order}")
data_attributes<-"`data-null`=NULL"
if('id'%nin%names(data)){
  data$id=id
}
# print(data)
if(length(data)>0){
  # input_type=sapply(data,function(x){
  #     if(length(x)!=length(id))
  #         g_stop("length of data attributes not equal to length of ids")
  #     if(is.logical(x))return("checkbox")
  #     if(is.character(x))return('radio')
  #     if(is.numeric(x))return('action')
  #     g_stop('data attributes must be logical,numeric, or character')
  # })
  data_init=lapply(data,function(x){
    if(is.logical(x))x<-js_logical(x)
    if(is.numeric(x))x<-as.character(x)
    x[is.na(x)]<-"NA"
    glue("'{x}'")
  })

  data_names<-glue('`data-{names(data)}`')
  data_names[data_names=='`data-radio_id`']<-'`data-id`'

  data_attributes=map2(data_names,data_init,function(x,y)paste0(x,"=",y)) %>%
    reduce(function(x,y){paste(x,y,sep=',')})
}
out <- expr_glue(tags$div(id="{id}",
                          {data_attributes},
                          class='d-flex flex-row {class} align-items-center',
                          div(class='pill-data-icon flex-nowrap',span(class='data-calculation',"="),icon_mdi('{icon}')),
                          div(class='pill-area d-flex flex-fill',tags$div("{label}",class="pill-label flex-fill",`data-id`="{id}"),div(icon_mdi('menu-down',class='icon-right dropdown-handle float-right')))
)
) %>% exprs_eval()

out


onClone='function(evt){
        var clones=[];
 var clones=evt.clones;
 if(clones.length===0){
      clones[0]==evt.clone;
 }
let fixlab=function(d,i){
    let $el=$(d);

    elid.addClass("pill-selected");
    let agg=elid.data("option_aggregation");
    let conversion=elid.data("conversion");
    let lab=elid.find(".pill-label").get(0);
     var current=$(lab).text();
    if(conversion!="NA"){
       current=conversion+"("+current+")";
    }
    if(agg!=="NA"){
     current=agg+"("+current+")";
    }
    $(lab).text(current);
    }

  clones.map(fixlab)
   }',




