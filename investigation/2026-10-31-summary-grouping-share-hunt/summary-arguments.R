pkgload::load_all('.',quiet=TRUE)
d <- tibble::tibble(g=c('a','a','b'),x=1:3)
run <- function(kind,mode,head,named,prior,margin) {
 env <- new.env(parent=globalenv());state <- new.env(parent=emptyenv());state$n <- 0L
 get_arg <- function() {state$n <- state$n+1L;switch(kind,fns=if(state$n==1L) sum else mean,names=if(state$n==1L)'first' else 'later',unpack=state$n!=1L)}
 if(mode=='binding') makeActiveBinding('arg',function(value)get_arg(),env) else env$get_arg <- get_arg
 args <- list(rlang::sym('x'),if(kind=='unpack') quote(function(x)tibble::tibble(z=sum(x))) else quote(sum))
 arg_expr <- if(mode=='binding') rlang::sym('arg') else quote(get_arg())
 if(kind=='fns')args[[2]] <- arg_expr
 if(kind=='names')args$.names <- arg_expr
 if(kind=='unpack')args$.unpack <- arg_expr
 expr <- rlang::call2('across',!!!args,.ns='dplyr')
 if(head=='head') expr[[1]] <- rlang::call2('(',expr[[1]])
 if(head=='whole') expr <- rlang::call2('(',expr)
 if(head=='nested') expr <- rlang::call2('identity',expr)
 dots<-list(rlang::new_quosure(expr,env));if(named)names(dots)<-'out'
 if(prior)dots<-c(list(before=rlang::quo(1L)),dots)
 value<-tryCatch(suppressWarnings(if(margin)summarize_with_margins(d,!!!dots,.by=g) else dplyr::summarise(d,!!!dots,.by=g)),error=identity)
 list(value=value,reads=state$n)
}
n<-0L;diffs<-0L
for(kind in c('fns','names','unpack'))for(mode in c('binding','call'))for(head in c('direct','head','whole','nested'))for(named in c(FALSE,TRUE))for(prior in c(FALSE,TRUE)) {
 a<-run(kind,mode,head,named,prior,FALSE);b<-run(kind,mode,head,named,prior,TRUE);n<-n+1L
 eq<-if(inherits(a$value,'error')&&inherits(b$value,'error'))TRUE else identical(a$value,b$value)
 if(!eq || a$reads!=b$reads){diffs<-diffs+1L;cat('DIFF',kind,mode,head,named,prior,'reads',a$reads,b$reads,'equal',eq,'\n');if(!eq){print(a$value);print(b$value)}}
}
cat('argument cases=',n,'differences=',diffs,'\n')
