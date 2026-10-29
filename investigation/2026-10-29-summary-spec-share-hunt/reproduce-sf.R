pkgload::load_all('.',quiet=TRUE)
suppressPackageStartupMessages(library(dplyr))
x <- sf::st_as_sf(data.frame(v=c(1,3),x=1:2,y=3:4),coords=c('x','y'))
for(empty in c(FALSE,TRUE)) {
 d<-if(empty)x[0,] else x
 cat('\nEMPTY',empty,'\n')
 print(summarise(d,total=sum(v),.by=geometry))
 observed<-tryCatch(summarize_with_margins(d,total=sum(v),.by=geometry),error=identity)
 if(inherits(observed,'error'))cat('MARGIN ERROR:',conditionMessage(observed),'\n') else print(observed)
 aliased<-mutate(d,private_geometry=.data$geometry)
 pre<-summarise(aliased,total=sum(v),.by=all_of('private_geometry'))
 cat('BEFORE',paste(names(pre),collapse=','),'sf_column=',attr(pre,'sf_column'),'\n')
 post<-rlang::set_names(pre,sub('private_geometry','geometry',names(pre),fixed=TRUE))
 cat('AFTER',paste(names(post),collapse=','),'sf_column=',attr(post,'sf_column'),'\n')
 print(tryCatch(sf::st_geometry(post),error=function(e)conditionMessage(e)))
 if('--assert' %in% commandArgs(TRUE)) stopifnot(!inherits(observed,'error'))
}
