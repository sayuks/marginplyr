suppressPackageStartupMessages({library(dplyr);library(dtplyr);pkgload::load_all(quiet=TRUE)})
set.seed(28131029)
out <- Sys.getenv('MARGINPLYR_HUNT_OUTPUT', unset = file.path(tempdir(), 'marginplyr-expand-hunt-lazy'))
dir.create(out, recursive = TRUE, showWarnings = FALSE)
attempt<-function(e)tryCatch(suppressWarnings(e),error=identity)
records<-list(); counts<-c(match=0L,refused=0L,source_error=0L,package_error=0L,diff=0L)
canon<-function(x)as.data.frame(arrange(x,across(everything())))
for(i in 1:600) {
 dat<-tibble(k=sample(c(1:3,NA_integer_),sample(0:7,1),TRUE),g=sample(c('a','b',NA_character_),length(k),TRUE),v=sample(c(1:3,NA_integer_),length(k),TRUE))
 dat2<-tibble(k=sample(c(1:3,NA_integer_),sample(0:7,1),TRUE),w=sample(c(1:3,NA_integer_),length(k),TRUE))
 q<-lazy_dt(dat);r<-lazy_dt(dat2);steps<-sample(1:8,sample(0:5,1),TRUE)
 for(s in steps) q<-attempt(switch(s,filter(q,is.na(k)|k>1L),arrange(q,desc(v)),mutate(q, new_v=v+1L),select(q,everything()),compute(q),left_join(q,r,by='k',relationship='many-to-many'),semi_join(q,r,by='k'),union_all(q,q)))
 if(inherits(q,'error')){counts['source_error']<-counts['source_error']+1L;next}
 actualsource<-attempt(collect(q));if(inherits(actualsource,'error')){counts['source_error']<-counts['source_error']+1L;next}
 sort<-sample(c('none','last','first'),1); label<-if(sample(c(TRUE,FALSE),1))NULL else 'Total'; id<-if(sample(c(TRUE,FALSE),1))'sid' else NULL
 got<-attempt(expand_with_margins(q,.by=k,.grouping=rollup(g),.margin_label=label,.id=id,.sort=sort))
 if(inherits(got,'marginplyr_error')){counts['refused']<-counts['refused']+1L;next}
 want<-attempt(expand_with_margins(actualsource,.by=k,.grouping=rollup(g),.margin_label=label,.id=id,.sort=sort))
 got<-attempt(collect(got));
 if(inherits(got,'error') && !inherits(want,'error')) {counts['package_error']<-counts['package_error']+1L;records[[length(records)+1L]]<-list(iteration=i,steps=steps,status='ERROR',error=conditionMessage(got),source=actualsource)} else if(!isTRUE(all.equal(if(sort=='none')canon(got) else got,if(sort=='none')canon(want) else want,check.attributes=TRUE))) {
 counts['diff']<-counts['diff']+1L;records[[length(records)+1L]]<-list(iteration=i,steps=steps,status='DIFF',got=got,want=want,source=actualsource)
 } else counts['match']<-counts['match']+1L
}
print(counts);saveRDS(records,file.path(out,'dtplyr-graph-fuzz.rds'));for(x in records)str(x,max.level=2)
