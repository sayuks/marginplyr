suppressPackageStartupMessages(library(dplyr))
args <- commandArgs(trailingOnly = TRUE)
repo <- if (length(args)) normalizePath(args[[1L]]) else getwd()
devtools::load_all(repo, quiet = TRUE)
con_sq <- DBI::dbConnect(RSQLite::SQLite(), ':memory:')
con_du <- DBI::dbConnect(duckdb::duckdb(shared_home=FALSE))
d <- tibble(f=c('B','A','B','A','B','A'),g=c('a','a','b','b',NA,NA),h=c(1L,2L,1L,2L,1L,2L),x=c(1,NA,4,-2,0,9),y=c(10,20,30,40,50,60),z=c(TRUE,NA,FALSE,TRUE,FALSE,TRUE))
inputs <- list(dtplyr=dtplyr::lazy_dt(d),arrow=arrow::Table$create(d),sqlite=copy_to(con_sq,d,'data'),duckdb=copy_to(con_du,d,'data'))
cases <- list()
for(fun in c('sum','mean','min','max','median','sd','var','n_distinct')) for(col in c('x','z')) {
 e <- rlang::call2(fun,rlang::sym(col),na.rm=TRUE); cases[[paste(fun,col)]] <- rlang::quos(v=!!e)
}
more <- list(rlang::quos(v=n()),rlang::quos(v=sum(is.na(x))),rlang::quos(v=sum(x*y,na.rm=TRUE)),rlang::quos(v=any(z,na.rm=TRUE)),rlang::quos(v=all(z,na.rm=TRUE)),rlang::quos(v=first(x)),rlang::quos(v=last(x)),rlang::quos(v=nth(x,2L)),rlang::quos(v=first(x,order_by=y)),rlang::quos(v=sum(x,na.rm=TRUE)/n()),rlang::quos(v=if_else(all(is.na(x)),NA_real_,sum(x,na.rm=TRUE))),rlang::quos(across(c(x,y),~sum(.x,na.rm=TRUE))),rlang::quos(across(c(x,y),list(s=~sum(.x,na.rm=TRUE),m=~mean(.x,na.rm=TRUE)))),rlang::quos(v=list(x)),rlang::quos(v=paste(x,collapse=',')),rlang::quos(v=quantile(x,.5,na.rm=TRUE)),rlang::quos(v=length(x)),rlang::quos(v=sum(x>0,na.rm=TRUE)),rlang::quos(v=sum(x,na.rm=TRUE),w=v+1),rlang::quos(x=sum(x,na.rm=TRUE),v=x+1))
names(more)<-paste0('extra',seq_along(more));cases<-c(cases,more)
cap <- function(e) tryCatch(suppressWarnings(e),error=identity)
normalize <- function(a){a<-as.data.frame(a);a[]<-lapply(a,function(x){if(is.factor(x)) as.character(x) else x}); a<-a[do.call(order,c(lapply(a,function(x){if(is.list(x)) vapply(x,deparse1,'') else x}),list(na.last=TRUE))),,drop=FALSE];rownames(a)<-NULL;a}
counts <- c(total=0,pass=0,both_error=0,expected_arrow_refusal=0,mismatch=0)
for(b in names(inputs)) for(s in list(c('g','h'),'g',character())) for(nm in names(cases)){
 inp<-inputs[[b]];dots<-cases[[nm]];counts['total']<-counts['total']+1
 expected<-cap(collect(rlang::eval_tidy(rlang::expr(summarise(!!inp,!!!dots,.by=all_of(c('f',s)))))))
 # Keep the Grouping plan's dimensions constant while comparing every branch separately.
 sets<-rlang::call2('grouping_set',!!!rlang::syms(s))
 actual<-cap(collect(rlang::eval_tidy(rlang::expr(summarize_with_margins(!!inp,!!!dots,.by=f,.grouping=!!sets,.margin_label=NULL)))))
 if(inherits(expected,'error')&&inherits(actual,'error')){counts['both_error']<-counts['both_error']+1;next}
 if(!inherits(expected,'error')&&!inherits(actual,'error')&&isTRUE(all.equal(normalize(expected),normalize(actual),check.attributes=FALSE))){counts['pass']<-counts['pass']+1;next}
 if(b == 'arrow' && !inherits(expected,'error') && inherits(actual,'marginplyr_error') && grepl('would read',conditionMessage(actual),fixed=TRUE)){counts['expected_arrow_refusal']<-counts['expected_arrow_refusal']+1;next}
 counts['mismatch']<-counts['mismatch']+1;cat('\nMISMATCH',b,paste(s,collapse=','),nm,'\n');cat('EXPECT\n');if(inherits(expected,'error'))cat(conditionMessage(expected),'\n')else print(expected);cat('ACTUAL\n');if(inherits(actual,'error'))cat(conditionMessage(actual),'\n')else print(actual)
}
print(counts)
DBI::dbDisconnect(con_sq);DBI::dbDisconnect(con_du,shutdown=TRUE)
