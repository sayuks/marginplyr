source('investigation/2026-10-29-summary-spec-share-hunt/summary-rollup.R')
exprs2 <- list(
 rlang::exprs(grouping_bit(g)),
 rlang::exprs(grouping_id()),
 rlang::exprs((grouping_bit)(g)),
 list(rlang::parse_expr("((grouping_bit(g)))")),
 rlang::exprs({grouping_bit(g)}),
 rlang::exprs(identity(grouping_bit(g))),
 rlang::exprs(as.numeric(grouping_bit(g))),
 rlang::exprs(I(grouping_bit(g))),
 rlang::exprs(list(grouping_bit(g))),
 rlang::exprs(tibble::tibble(bit=grouping_bit(g),n=dplyr::n())),
 rlang::exprs(packed=tibble::tibble(bit=grouping_bit(g),n=dplyr::n())),
 rlang::exprs(identity(tibble::tibble(bit=grouping_bit(g),n=dplyr::n()))),
 rlang::exprs(identity(tibble::tibble(bit=grouping_bit(g),n=dplyr::n())),later=bit+n),
 rlang::exprs(data.frame(bit=grouping_bit(g))),
 rlang::exprs(as.data.frame(cbind(bit=grouping_bit(g)))),
 rlang::exprs(setNames(list(grouping_bit(g)),'bit')),
 rlang::exprs(do.call(tibble::tibble,list(bit=grouping_bit(g)))),
 rlang::exprs(dplyr::across(c(x,y),~sum(.x)+grouping_bit(g))),
 rlang::exprs(dplyr::across(c(x,y),~tibble::tibble(total=sum(.x),bit=grouping_bit(g)),.unpack=TRUE)),
 rlang::exprs(dplyr::across(c(x,y),~tibble::tibble(total=sum(.x),bit=grouping_bit(g)))),
 rlang::exprs((dplyr::across)(c(x,y),~sum(.x)+grouping_bit(g))),
 rlang::exprs((dplyr::across(c(x,y),~sum(.x)+grouping_bit(g)))),
 rlang::exprs(dplyr::if_all(c(x,y),~.x>grouping_bit(g))),
 rlang::exprs(dplyr::if_any(c(x,y),~.x>grouping_bit(g))),
 rlang::exprs(nrow(dplyr::pick(x))+grouping_bit(g)),
 rlang::exprs((function(z) z+1)(grouping_bit(g))),
 rlang::exprs((function() grouping_bit(g))()),
 rlang::exprs((function(z=grouping_bit(g)) z)()),
 rlang::exprs((function(z=grouping_bit(g)) z)(5)),
 rlang::exprs(local({b<-grouping_bit(g); b+1})),
 rlang::exprs(with(list(b=grouping_bit(g)),b+1)),
 rlang::exprs(eval(quote(grouping_bit(g)))),
 rlang::exprs(eval(str2lang('grouping_bit(g)'))),
 rlang::exprs(rlang::eval_tidy(rlang::quo(grouping_bit(g)))),
 rlang::exprs(list(base::bquote(x + .(grouping_bit(g))))),
 rlang::exprs(list(quote(grouping_bit(g)))),
 rlang::exprs(list(expression(grouping_bit(g)))),
 rlang::exprs(list(substitute(grouping_bit(g)))),
 rlang::exprs(grouping_bit(g),grouping_bit(g)),
 rlang::exprs(grouping_bit(g),following=dplyr::pick(dplyr::where(is.numeric))),
 rlang::exprs(1+2+3+4+5+6+7+8+9+10+11+12+13+14+15+grouping_bit(g))
)
# Independent dplyr oracle: lexically bound helpers ignore their unevaluated input.
check_helpers<-function(d,dots,i){
 grouping_bit<-function(...) bit
 grouping_id<-function(...) bit
 bit<-0L
 exp<-tryCatch(suppressWarnings({
  a<-rlang::eval_tidy(rlang::expr(dplyr::summarise(d,!!!dots,.by=g)),env=environment())
  bit<-1L
  b<-rlang::eval_tidy(rlang::expr(dplyr::summarise(d[setdiff(names(d),'g')],!!!dots)),env=environment())
  b<-dplyr::mutate(b,g=vctrs::vec_init(d$g,nrow(b)),.before=1L)
  dplyr::bind_rows(a,b)
 }),error=identity)
 got<-tryCatch(suppressWarnings(rlang::eval_tidy(rlang::expr(summarize_with_margins(d,!!!dots,.grouping=rollup(g),.margin_label=NULL)),env=environment())),error=identity)
 if(inherits(exp,'error')&&inherits(got,'error'))return(TRUE)
 if(!inherits(exp,'error'))exp<-multiset(exp)
 if(!inherits(got,'error'))got<-multiset(got)
 if(!isTRUE(all.equal(exp,got,check.attributes=TRUE))){cat('HELPER FAIL',i,'\n');print(dots);print(exp);print(got);saveRDS(list(d=d,dots=dots,expected=exp,actual=got),sprintf('/tmp/marginplyr-helper-fail-%s.rds',i));return(FALSE)}
 TRUE
}
fails<-0L;n<-0L
for(d in allframes)for(dots in exprs2){n<-n+1L;if(!check_helpers(d,dots,n))fails<-fails+1L}
cat('HELPER SUMMARY',n,'cases;',fails,'mismatches\n')
