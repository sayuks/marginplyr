pkgload::load_all('.', quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
set.seed(103010)
cons<-list(duckdb=DBI::dbConnect(duckdb::duckdb(shared_home=FALSE)),sqlite=DBI::dbConnect(RSQLite::SQLite(),':memory:'))
ratio<-function(n,d)if(is.na(n)||is.na(d)||d==0)NA_real_ else n/d
specs<-list(rollup=rollup(g,h),overlap=rollup(g,c(g,h)),repeated=rollup(g,g,h),composite=rollup(c(g,h)),empty=rollup(g,c()),cube=cube(g,h),sets=grouping_sets(grouping_set(g),grouping_set(),grouping_set(g,h),grouping_set(),grouping_set(h),grouping_set(g)),product=grouping_spec(rollup(g),rollup(h)))
count<-0L
for(iter in 1:40){
 n<-sample(c(0,1,4,12),1);d<-tibble(fixed=sample(c('x','y',NA_character_),n,TRUE),g=sample(c('a','b',NA_character_),n,TRUE),h=sample(c('c','d',NA_character_),n,TRUE),x=sample(c(-2,0,1,4,NA_real_),n,TRUE))
 inputs<-list(local=d,dtplyr=dtplyr::lazy_dt(d),duckdb=copy_to(cons$duckdb,d,'facts',overwrite=TRUE),sqlite=copy_to(cons$sqlite,d,'facts',overwrite=TRUE))
 for(sn in names(specs))for(b in names(inputs))for(fixed in c(FALSE,TRUE)){
 dots<-rlang::exprs(value=coalesce(sum(x,na.rm=TRUE),0),total=share_of_total(value),bg=grouping_bit(g)); if(sn!='empty')dots$bh<-rlang::expr(grouping_bit(h)); if(sn%in%c('rollup','overlap','repeated','composite','empty'))dots$parent<-rlang::expr(share_of_parent(value))
 spec<-specs[[sn]]
 out<-tryCatch({z<-summarize_with_margins(inputs[[b]],!!!dots,.by=!!if(fixed)quote(fixed) else NULL,.grouping=spec,.id='set',.duplicates='keep',.check_share_source=FALSE,.margin_label=NULL);if(b!='local')z<-collect(z);z},error=identity)
 tag<-paste(iter,sn,b,fixed); count<-count+1L
 if(inherits(out,'error'))stop(tag, ': ', conditionMessage(out))
 plan<-inspect_grouping(d,.grouping=spec,.format='list',.duplicates='keep')
 for(i in seq_len(nrow(out))){
 part<-if(fixed)which(vctrs::vec_equal(out$fixed,out$fixed[i],na_equal=TRUE))else seq_len(nrow(out))
 sets<-plan$included; grandset<-which(lengths(sets)==0);grands<-part[out$set[part]%in%grandset]; expected<-if(out$set[i]%in%grandset)1 else ratio(out$value[i],out$value[grands[1]])
 if(!isTRUE(all.equal(out$total[i],expected)))stop(tag,' total row ',i)
 if('parent'%in%names(out)){
  current<-sets[[out$set[i]]];candidate<-which(vapply(sets,function(s)length(s)<length(current)&&all(s%in%current),TRUE));target<-if(!length(candidate))out$set[i]else candidate[which.max(lengths(sets[candidate]))];keys<-sets[[target]];par<-part[out$set[part]==target];for(k in keys)par<-par[vctrs::vec_equal(out[[k]][par],out[[k]][i],na_equal=TRUE)]
  expected<-if(length(current)==0)1 else ratio(out$value[i],out$value[par[1]]); if(!isTRUE(all.equal(out$parent[i],expected)))stop(tag,' parent row ',i)
 }
 }
 }
}
for(con in cons)DBI::dbDisconnect(con)
cat('PASS',count,'runs\n')
