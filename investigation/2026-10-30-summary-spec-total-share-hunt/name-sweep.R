pkgload::load_all('.', quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
nameset<-c('.','..','...','....','..x','..v','..g','..fixed','..N','.N','.I','.SD','.BY','.GRP','.NGRP','.EACHI','.SDall','.SDcols','.Last.updated','V1','i.g','x.g','NA','NULL','TRUE','if','function','{','\n','a\tb','._marginplyr_set','__marginplyr_set','denominator','..-01','..+01','..1.0','..1e0','..1L','..0x1','..Inf','..NaN','..+ 1','.. 01','..\t1','..١')
checks<-0L; upstream<-0L
base<-tibble(g=c('a','b'),fixed=c('x','x'),v=c(1,3))
for(b in c('local','dtplyr'))for(role in c('source','output','fixed'))for(nm in nameset){
 d<-base; dots<-rlang::exprs(total=sum(v),fraction=share_of_total(total)); by<-rlang::expr(fixed)
 if(role=='source'){names(dots)[1]<-nm;dots[[2]]<-rlang::call2('share_of_total',rlang::sym(nm))}
 if(role=='output')names(dots)[2]<-nm
 if(role=='fixed'){names(d)[2]<-nm;by<-rlang::expr(all_of(!!nm))}
 src<-if(b=='local')d else dtplyr::lazy_dt(d)
 res<-tryCatch({z<-summarize_with_margins(src,!!!dots,.by=!!by,.grouping=rollup(g));if(b=='dtplyr')z<-collect(z);z},error=identity)
 if(inherits(res,'error')){if(nm!='...')stop(b,role,nm,conditionMessage(res));upstream<-upstream+1L;cat(b,role,encodeString(nm,quote='"'),'ERR',conditionMessage(res),'\n')}
 else {share<-if(role=='output')res[[nm]]else res$fraction;if(!isTRUE(all.equal(share,c(0.25,0.75,1))))stop(b,role,nm,'WRONG',paste(share,collapse=','));checks<-checks+1L}
}

cat('PASS',checks,'ratio checks;',upstream,'documented upstream-name refusals\n')
