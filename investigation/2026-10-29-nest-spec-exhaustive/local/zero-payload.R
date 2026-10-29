args <- commandArgs(TRUE)
pkgload::load_all(if(length(args))args[[1L]]else ".",quiet=TRUE)
source("investigation/2026-10-29-nest-spec-exhaustive/local/local-fixtures.R")
set.seed(29102913)
calls <- cells <- 0L
for(kind in kinds)for(n in c(0L,1L,6L))for(fixed in c(FALSE,TRUE))for(p in c(1L,3L)) {
  sets <- plans[[p]]
  x <- tibble::tibble(a=make_value(kind,n),b=rep(c("b","a",NA_character_),length.out=n))
  if(fixed)x$fixed<-make_factor(n)
  for(verb in c("nest_with_margins","nest_by_with_margins"))for(output in list(c("data","set"),c("..1","..."),c(".rows","..01"))) {
    key<-output[[1L]];id<-output[[2L]]
    out <- rlang::inject((get(verb))(x,.by=!!if(fixed)rlang::sym("fixed")else NULL,
      .grouping=!!expr_sets(sets),.margin_label=NULL,.id=id,.key=key,.sort=sample(c("none","first","last"),1L)))
    out<-dplyr::ungroup(out);calls<-calls+1L
    for(j in seq_along(sets)) {
      keys<-c(if(fixed)"fixed",sets[[j]])
      loc<-if(length(keys))vctrs::vec_group_loc(x[keys]) else list(key=tibble::new_tibble(list(),nrow=as.integer(n>0L)),loc=if(n>0L)list(seq_len(n))else list())
      actual<-out[out[[id]]==j,,drop=FALSE]
      stopifnot(nrow(actual)==length(loc$loc))
      if(nrow(actual)) {
        match<-if(length(keys))vctrs::vec_match(actual[keys],loc$key)else 1L
        stopifnot(!anyNA(match))
        for(i in seq_len(nrow(actual))){
          cell<-actual[[key]][[i]];cells<-cells+1L
          stopifnot(is.data.frame(cell),ncol(cell)==0L,nrow(cell)==length(loc$loc[[match[[i]]]]))
        }
      }
    }
  }
}
for(n in c(0L,1L,6L))for(verb in c("nest_with_margins","nest_by_with_margins"))for(id in list(NULL,"...","..01"))for(key in c("data","..1",".rows")) {
  x<-tibble::new_tibble(list(),nrow=n)
  out<-(get(verb))(x,.id=id,.key=key,.sort="last")
  calls<-calls+1L
  count<-as.integer(n>0L||verb=="nest_by_with_margins")
  stopifnot(nrow(out)==count)
  if(count)stopifnot(nrow(out[[key]][[1L]])==n,ncol(out[[key]][[1L]])==0L)
}
cat("seed 29102913;",calls,"calls;",cells,"keyed empty-column cells; PASS: no-payload cells preserve independent source-group cardinalities\n")
