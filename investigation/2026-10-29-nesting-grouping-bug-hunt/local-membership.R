pkgload::load_all(quiet=TRUE)
library(dplyr)
set.seed(492763)
name_pool <- c('fixed','g','h','f','data','.key','.by','.groups','.rows','.data','.env','T','F','..marginplyr_nest_1','..marginplyr_nest_2','..marginplyr_column_1','..marginplyr_cell_1','n','list','set','x y','x\ny')
fail<-list()
count<-0L
for(case in seq_len(300L)) {
  n<-sample(0:15,1); cn<-sample(name_pool,5); fixed<-cn[1]; dims<-cn[2:4]; rid<-cn[5]
  input<-tibble(fixed=sample(c('north','south',NA),n,TRUE),g=sample(c('a','b','Total',NA),n,TRUE),h=sample(c(1L,2L,NA),n,TRUE),f=factor(sample(c('r','s',NA),n,TRUE),levels=c('s','r','unused')),rid=seq_len(n));names(input)<-cn
  chosen<-sample(0:7,sample(1:8,1),replace=FALSE)
  sets<-lapply(chosen,function(bits) dims[as.logical(intToBits(bits)[1:3])])
  expr<-rlang::call2('grouping_sets',!!!lapply(sets,function(vars)rlang::call2('grouping_set',rlang::call2('all_of',vars))))
  spec<-eval(expr)
  key<-sample(setdiff(name_pool,c(fixed,dims)),1)
  identifier<-sample(setdiff(name_pool,c(cn,key)),1)
  use_fixed<-sample(c(TRUE,FALSE),1)
  keep<-sample(c(TRUE,FALSE),1)
  sort<-sample(c('none','last','first'),1)
  label<-sample(list(NULL,NA_character_,'Total'),1)[[1]]
  # Fixed columns remain a key; any other dimension not used in the plan is payload.
  all_dims<-unique(unlist(sets))
  for(verbname in c('nest_with_margins','nest_by_with_margins')) {
    if(verbname=='nest_by_with_margins' && '.rows'%in%c(if(use_fixed)fixed,all_dims,identifier))next
    msg<-paste(case,verbname)
    count<-count+1L
    tryCatch({
      out<-get(verbname)(input,.by=if(use_fixed)all_of(fixed),.grouping=spec,.key=key,.id=identifier,.margin_label=label,.check_margin_label=FALSE,.keep=keep,.sort=sort)
      want_groups<-if(verbname=='nest_by_with_margins')c(if(use_fixed)fixed,all_dims,identifier) else character()
      stopifnot(identical(dplyr::group_vars(out),want_groups))
      want<-list()
      for(s in seq_along(sets)) {
        keys<-c(if(use_fixed)fixed,sets[[s]])
        members<-if(length(keys)) vctrs::vec_group_loc(input[keys])$loc else if(n || (verbname=='nest_by_with_margins' && !use_fixed && !length(all_dims)))list(seq_len(n)) else list()
        for(rows in members) want[[paste(s,paste(rows,collapse=','),sep=':')]]<-list(id=s,rows=rows)
      }
      seen<-character()
      for(i in seq_len(nrow(out))) {
        cell<-out[[key]][[i]]
        rows<-cell[[rid]]
        id<-out[[identifier]][i]
        tag<-paste(id,paste(rows,collapse=','),sep=':')
        if(!tag %in% names(want))stop('unexpected membership ',tag)
        seen<-c(seen,tag)
        for(d in c(if(use_fixed)fixed,all_dims)){
          actual<-vctrs::vec_slice(out[[d]],i)
          if(d %in% c(if(use_fixed)fixed,sets[[id]])) {
            expected<-vctrs::vec_slice(input[[d]],rows[1])
            if(d %in% all_dims && !is.null(label) && !is.na(label) && !is.factor(expected))expected<-as.character(expected)
            if(!identical(is.na(actual),is.na(expected))||!identical(as.character(actual),as.character(expected)))stop('outer key wrong ',d)
          } else {
            if(is.null(label)||is.na(label)) {if(!all(is.na(actual)))stop('missing label wrong ',d)} else if(!identical(as.character(actual),label))stop('string label wrong ',d)
          }
        }
        wanted_names<-if(keep)c(if(use_fixed)fixed,all_dims,setdiff(cn,c(if(use_fixed)fixed,all_dims))) else setdiff(cn,c(if(use_fixed)fixed,all_dims))
        if(!identical(names(cell),wanted_names))stop('cell names wrong')
        for(p in wanted_names) if(!identical(cell[[p]],vctrs::vec_slice(input[[p]],rows)))stop('cell column wrong ',p)
      }
      if(!identical(sort(seen),sort(as.character(names(want)))))stop('missing membership')
    },error=function(e) { fail[[msg]]<<-list(error=conditionMessage(e),input=input,sets=sets,key=key,id=identifier,by=use_fixed,keep=keep,sort=sort,label=label) })
  }
}
cat('checked',count,'\n'); print(fail)
stopifnot(length(fail) == 0L)
cat('PASS: 556 checked cases\n')
