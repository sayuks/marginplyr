pkgload::load_all('.',quiet=TRUE)
options(lifecycle_verbosity='quiet')
base <- data.frame(g=c('a','b'),x=1:2,y=c(10L,20L))
inputs <- list(frame=base,tibble=tibble::as_tibble(base),dt=data.table::as.data.table(base))
forms <- c(bare='cols',parenthesized='(cols)',all_of='tidyselect::all_of(cols)',env='.env$cols',envsub='.env[["cols"]]')
run <- function(input,helper,selection,named,prior,margin,throw=FALSE) {
 env <- new.env(parent=globalenv());state <- new.env(parent=emptyenv());state$n <- 0L
 makeActiveBinding('cols',function(value){state$n <- state$n+1L;if(throw&&state$n>1L)stop('selector was read twice');if(state$n==1L)'x' else 'y'},env)
 expr <- if(helper=='across') parse(text=paste0('dplyr::across(',selection,',sum)'))[[1L]] else parse(text=paste0('dplyr::pick(',selection,')'))[[1L]]
 dots <- list(rlang::new_quosure(expr,env));if(named) names(dots)<-'out'
 if(prior)dots<-c(list(first=rlang::quo(1L)),dots)
 value <- tryCatch(suppressWarnings(if(margin) summarize_with_margins(input,!!!dots,.by=g) else dplyr::summarise(input,!!!dots,.by=g)),error=identity)
 list(value=value,reads=state$n)
}
rows <- list();i<-0L
for(input in names(inputs)) for(helper in c('across','pick')) for(selection in names(forms)) for(named in c(FALSE,TRUE)) for(prior in c(FALSE,TRUE)) {
 a<-run(inputs[[input]],helper,forms[[selection]],named,prior,FALSE)
 b<-run(inputs[[input]],helper,forms[[selection]],named,prior,TRUE)
 eq<-identical(a$value,b$value)
 i<-i+1L;rows[[i]]<-data.frame(input,helper,selection,named,prior,ordinary_reads=a$reads,margin_reads=b$reads,equal=eq)
 expected_bad<-helper=='across'&&selection=='bare'&&!prior
 stopifnot(identical(!eq,expected_bad),b$reads==a$reads+as.integer(expected_bad))
}
results<-do.call(rbind,rows)
print(aggregate(equal~selection+prior,results,function(x)c(n=length(x),equal=sum(x))))
cat('matrix cases=',nrow(results),'extra-read wrong-column=',sum(!results$equal),'\n')
a<-run(base,'across','cols',FALSE,FALSE,FALSE,TRUE)
b<-run(base,'across','cols',FALSE,FALSE,TRUE,TRUE)
stopifnot(!inherits(a$value,'error'),inherits(b$value,'error'),grepl('selector was read twice',conditionMessage(b$value),fixed=TRUE))
cat('read-once control successful; Margin extra read raised:',conditionMessage(b$value),'\n')
if('--assert'%in%commandArgs(TRUE))stopifnot(identical(a$value,b$value))
