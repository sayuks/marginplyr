Sys.setenv(TESTTHAT_PARALLEL='false')
pkgload::load_all(quiet=TRUE)
suppressPackageStartupMessages(library(dplyr))
keys<-c('x','.data','.env','...','..1','.','.by','.grouping','.id','.sort','_data','FALSE','TRUE','NA','NULL','Inf','NaN','if','{','[','::',':::','(',')','a`b','a\nb','a\tb','x y','c','sum','pick','across','n','grouping_bit','share_of_total','..marginplyr_key_1','..marginplyr_set_id_1','..marginplyr_share_1')
count<-0L
for(nm in keys)for(form in 1:5)for(group in c(FALSE,TRUE)){
 d<-tibble(g=c('a','b'),temp=c(1L,3L));names(d)[[2]]<-nm
 ref<-rlang::sym(nm)
 e<-switch(form,rlang::expr(sum(.data[[!!nm]])),rlang::expr(sum(!!ref)),rlang::expr(sum(pick(all_of(!!nm)))),rlang::expr(across(all_of(!!nm),sum)),rlang::expr(identity(pick(all_of(!!nm)))))
 dots<-list(rlang::new_quosure(e,globalenv()))
 if(form<4)names(dots)<-'answer'
 a<-tryCatch(suppressWarnings(summarize(d,!!!dots,.by=all_of(if(group)'g' else character()))),error=identity)
 b<-tryCatch(suppressWarnings(summarize_with_margins(d,!!!dots,.by=all_of(if(group)'g' else character()))),error=identity)
 eq<-if(inherits(a,'error')||inherits(b,'error'))inherits(a,'error')&&inherits(b,'error')else isTRUE(all.equal(a,b,check.attributes=FALSE))
 count<-count+1L
 if(!eq){cat('\nDIFF',encodeString(nm),form,group,'\n');print(a);print(b)}
}
cat('completed',count,'comparisons\n')
