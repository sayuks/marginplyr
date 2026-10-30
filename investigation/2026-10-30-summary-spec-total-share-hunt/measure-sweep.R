pkgload::load_all('.', quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
nameset<-c('.','..','..x','..v','..g','..N','.N','.I','.SD','.BY','.GRP','.NGRP','.EACHI','.SDall','.SDcols','.Last.updated','V1','i.g','x.g','NA','NULL','TRUE','if','function','{','a\tb','..-1','..+1','.. 1','..0','..00','..01','..1.0','..1e0','..1L','..0x1','..Inf','..NaN','..+ 1','.. 01','..\t1','..١')
checks<-0L
for(nm in nameset)for(form in c('bare','pronoun','across','pick')){
 d<-tibble(g=c('a','b'),v=c(1,3));names(d)[2]<-nm;z<-dtplyr::lazy_dt(d)
 source<-switch(form,bare=rlang::expr(sum(!!rlang::sym(nm))),pronoun=rlang::expr(sum(.data[[!!nm]])),across=rlang::expr(sum(across(all_of(!!nm),identity))),pick=rlang::expr(sum(pick(all_of(!!nm)))))
 run<-function(shares){dots<-rlang::exprs(value=!!source);if(shares)dots$fraction<-rlang::expr(share_of_total(value));tryCatch(collect(summarize_with_margins(z,!!!dots,.grouping=rollup(g))),error=identity)}
 control<-run(FALSE);shared<-run(TRUE)
 if(!inherits(control,'error')&&(inherits(shared,'error')||!isTRUE(all.equal(shared$fraction,c(0.25,0.75,1)))))stop(encodeString(nm,quote='"'),form,'CONTROL',paste(control$value,collapse=','),'SHARE',if(inherits(shared,'error'))conditionMessage(shared)else paste(shared$fraction,collapse=','),'\n')
 checks<-checks+1L
}

cat('PASS',checks,'measure cases with no-share controls\n')
