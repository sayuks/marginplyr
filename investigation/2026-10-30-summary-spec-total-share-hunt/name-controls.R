pkgload::load_all('.', quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
for(b in c('local','dtplyr'))for(role in c('source','output','fixed')){
 d<-tibble(g=c('a','b'),fixed=c('x','x'),v=c(1,3));dots<-rlang::exprs(total=sum(v));by<-rlang::expr(c(fixed,g))
 if(role%in%c('source','output'))names(dots)<-'...'
 if(role=='fixed'){names(d)[2]<-'...';by<-rlang::expr(c(all_of('...'),g))}
 src<-if(b=='local')d else dtplyr::lazy_dt(d)
 out<-tryCatch({z<-summarise(src,!!!dots,.by=!!by);if(b=='dtplyr')z<-collect(z);z},error=identity)
 cat(b,role,'\n');if(inherits(out,'error'))cat(conditionMessage(out),'\n')else print(out)
}
