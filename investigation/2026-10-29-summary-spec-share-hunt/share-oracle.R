args <- commandArgs(TRUE)
root <- if(length(args)) args[[1]] else '.'
pkgload::load_all(root, quiet=TRUE)
suppressPackageStartupMessages(library(dplyr))
set.seed(72531)
cons <- list(duckdb=DBI::dbConnect(duckdb::duckdb(shared_home=FALSE)), sqlite=DBI::dbConnect(RSQLite::SQLite(), ':memory:'))
on.exit <- function() invisible(NULL)
canon <- function(x) { x <- as.data.frame(arrange(x, fixed,g,h,set)); for(n in intersect(c('total','t','p'),names(x))) x[[n]] <- as.double(x[[n]]); x }
check <- function(out, info) {
  for (i in seq_len(nrow(out))) {
    partition <- which(vctrs::vec_equal(out$fixed, out$fixed[i], na_equal=TRUE))
    grand <- partition[out$bg[partition]==1 & out$bh[partition]==1]
    stopifnot(length(grand)>=1)
    ratio <- function(num,den) if (is.na(num)||is.na(den)||den==0) NA_real_ else as.double(num)/as.double(den)
    expected <- if(i %in% grand) 1 else ratio(out$total[i],out$total[grand[1]])
    if(!isTRUE(all.equal(out$t[i],expected,tolerance=1e-8))) stop(info, ' total row ',i)
    if('p'%in%names(out)) {
      par <- if(out$bh[i]==0) partition[out$bg[partition]==0 & out$bh[partition]==1 & vctrs::vec_equal(out$g[partition],out$g[i],na_equal=TRUE)] else grand
      expected <- if(i %in% grand) 1 else ratio(out$total[i],out$total[par[1]])
      if(!isTRUE(all.equal(out$p[i],expected,tolerance=1e-8))) stop(info, ' parent row ',i)
    }
  }
}
count <- 0L
for(iter in seq_len(40)) {
 n <- sample(c(0,1,2,5,20),1)
 data <- tibble(fixed=sample(c('x','y',NA_character_),n,TRUE),g=sample(c('a','b',NA_character_),n,TRUE),h=sample(c('i','j',NA_character_),n,TRUE),v=sample(c(-3,0,2,5,NA_real_),n,TRUE))
 for(kind in c('rollup','cube','sets','duplicates')) {
  spec <- switch(kind,rollup=rollup(g,h),cube=cube(g,h),sets=grouping_sets(grouping_set(g),grouping_set(h),grouping_set()),duplicates=grouping_sets(grouping_set(g),grouping_set(),grouping_set(),.duplicates='keep'))
  dots <- rlang::exprs(total=dplyr::coalesce(sum(v,na.rm=TRUE),0), t=share_of_total(total), bg=grouping_bit(g), bh=grouping_bit(h))
  if(kind=='rollup') dots$p <- rlang::expr(share_of_parent(total))
  if(kind=='duplicates') dots$bh <- NULL
  if(kind=='duplicates') next
  report <- function(input) summarize_with_margins(input, !!!dots,.by=fixed,.grouping=spec,.id='set',.margin_label=NULL,.check_share_source=FALSE)
  local <- report(data); check(local,paste(iter,kind,'local')); count<-count+1L
  backends <- list(dtplyr=dtplyr::lazy_dt(data),duckdb=copy_to(cons$duckdb,data,'facts',overwrite=TRUE),sqlite=copy_to(cons$sqlite,data,'facts',overwrite=TRUE))
  for(b in names(backends)) {
    actual <- collect(report(backends[[b]])); check(actual,paste(iter,kind,b)); count<-count+1L
    cmp <- all.equal(canon(actual),canon(local),check.attributes=FALSE,tolerance=1e-8)
    if(!isTRUE(cmp)) stop(paste(iter,kind,b,paste(cmp,collapse='; ')))
  }
 }
}
for(con in cons) DBI::dbDisconnect(con)
cat('PASS',count,'oracle/differential runs\n')
