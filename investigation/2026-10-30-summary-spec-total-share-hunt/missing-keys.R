pkgload::load_all('.', quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
d<-tibble(fixed=c(NA_real_,NaN,1,Inf,-Inf,NA,NaN,1,Inf,-Inf),g=rep(c('a','b'),each=5),h=rep(c(NA_real_,NaN,1,Inf,-Inf),2),x=1:10)
con<-DBI::dbConnect(duckdb::duckdb(shared_home=FALSE));inputs<-list(local=d,dtplyr=dtplyr::lazy_dt(d),duckdb=copy_to(con,d,'facts'))
for(b in names(inputs)){
 cat('\nBACKEND',b,'\n');out<-summarize_with_margins(inputs[[b]],value=sum(x),parent=share_of_parent(value),total=share_of_total(value),bh=grouping_bit(h),bg=grouping_bit(g),.by=fixed,.grouping=rollup(g,h),.margin_label=NULL,.id='set');if(b!='local')out<-collect(out);out<-arrange(out,fixed,set,g,h);print(out,n=50)
 for(i in seq_len(nrow(out))){part<-which(vctrs::vec_equal(out$fixed,out$fixed[i],na_equal=TRUE));grand<-part[out$set[part]==3];stopifnot(length(grand)==1);expected<-if(out$set[i]==3)1 else out$value[i]/out$value[grand];stopifnot(isTRUE(all.equal(out$total[i],expected)))}
}
DBI::dbDisconnect(con)
cat('PASS missing numeric fixed/dimension keys\n')
