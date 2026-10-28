suppressPackageStartupMessages(library(dplyr))
pkgload::load_all(if (length(commandArgs(TRUE))) commandArgs(TRUE)[1L] else getwd(), quiet=TRUE)
for (nm in c('.N', '.I', '.SD', '.GRP', '.NGRP', '.BY')) {
  for (role in c('fixed', 'dimension')) {
    data <- data.frame(f=c('P','P','Q','Q',NA,NA),g=rep(c('a','b'),3L),x=c(1,3,2,6,3,3))
    names(data)[if(role=='fixed') 1L else 2L] <- nm
    fixed <- if(role=='fixed') nm else 'f'
    dimension <- if(role=='dimension') nm else 'g'
    original <- data
    source <- dtplyr::lazy_dt(data)
    ordinary <- summarize_with_margins(source, value=sum(x), .by=all_of(fixed), .grouping=rollup(all_of(dimension)), .margin_label=NULL) |> collect()
    stopifnot(nrow(ordinary)==9L, identical(as.data.frame(collect(source)),original))
    for(helper in c('share_of_total','share_of_parent')) for(style in c('direct','across')) {
      seen <- 0L
      tally <- function(x) {seen <<- seen+1L;sum(x)}
      share <- if(style=='direct') rlang::quos(fraction=!!rlang::call2(helper,quote(value))) else rlang::quos(across(value, !!rlang::sym(helper), .names='fraction'))
      query <- tryCatch(summarize_with_margins(source,value=tally(x),!!!share,.by=all_of(fixed),.grouping=rollup(all_of(dimension)),.margin_label=NULL),error=identity)
      stopifnot(seen==0L, identical(as.data.frame(collect(source)),original))
      cat(nm,role,helper,style,': ')
      if(inherits(query,'error')) {
        cat(class(query)[1L], gsub('\n',' ',conditionMessage(query)),'\n')
        stopifnot(role=='fixed', inherits(query,'vctrs_error_names_must_be_unique'))
      } else {
        result <- collect(query)
        stopifnot(role=='dimension',identical(sort(result$fraction),sort(c(0.25,0.75,1,0.25,0.75,1,0.5,0.5,1))))
        cat('PASS; no premature evaluation; source unchanged\n')
      }
    }
  }
}
# A fixed-only plan needs no denominator lookup and therefore avoids this defect.
data <- data.frame(.N=c('P','P'),x=c(1,3))
root <- summarize_with_margins(dtplyr::lazy_dt(data),value=sum(x),fraction=share_of_total(value),.by=.N) |> collect()
stopifnot(identical(root$fraction,1))
cat('fixed-only plan: PASS\n')
# The named vector produced by vapply recreates the original special key as a
# computed group after margin_safe_column_reads has renamed that same key.
keys <- c('.N','..marginplyr_share_dimension_1')
safe_name <- function(x) if(x=='.N') '..marginplyr_dtplyr_column_1' else x
cat('group_by dots from current builder:\n')
dput(rlang::syms(vapply(keys,safe_name,character(1))))
