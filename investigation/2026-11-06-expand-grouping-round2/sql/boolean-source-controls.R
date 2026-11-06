pkgload::load_all('.',quiet=TRUE);suppressPackageStartupMessages(library(dplyr))
con<-DBI::dbConnect(RSQLite::SQLite(),':memory:');DBI::dbWriteTable(con,'input',setNames(data.frame('a',7L),c('g','TRUE')))
src<-tbl(con,'input');obs<-function(n,expr){x<-tryCatch(force(expr),error=identity);cat(n,'\n');if(inherits(x,'error'))cat(conditionMessage(x),'\n')else print(x);invisible(x)}
for(gn in c('window','collapse','union')){
 x<-switch(gn,window=mutate(src,z=row_number(g)),collapse=collapse(filter(src,TRUE)),union=union_all(src,src))
 obs(paste(gn,'source'),collect(x))
 obs(paste(gn,'projection'),collect(select(x,everything())))
 obs(paste(gn,'rename'),collect(rename(x,payload=all_of('TRUE'))))
 obs(paste(gn,'mutate'),collect(mutate(x,g=NA_character_)))
 obs(paste(gn,'union'),collect(union_all(x,mutate(x,g=NA_character_))))
 obs(paste(gn,'expand-noargs'),collect(expand_with_margins(x)))
}
DBI::dbDisconnect(con)
