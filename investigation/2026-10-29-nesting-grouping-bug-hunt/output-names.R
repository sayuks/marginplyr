pkgload::load_all(commandArgs(TRUE)[[1]],quiet=TRUE)
d <- tibble::tibble(g=c('a','b'),v=1:2)
for(nm in c('...','..1','..0','..01','..x','.SD','.BY','.N','.I','.GRP','.env','.data','names','row.names','__marginplyr_key_1','a\nb')) for(kind in c('id','key')) for(verb in c('nest_with_margins','nest_by_with_margins','inspect_grouping')) {
  if(verb=='inspect_grouping') next
  args <- list(.data=d,.grouping=rollup(g))
  args[[paste0('.',kind)]] <- nm
  ans <- tryCatch(do.call(get(verb),args),error=identity)
  if(inherits(ans,'error')) stop(conditionMessage(ans))
}
cat('PASS: 64 output-name/verb/role cases\n')
