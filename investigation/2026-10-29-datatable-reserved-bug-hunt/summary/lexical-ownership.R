# Evidence only. All assertions measure the investigated behavior on 2026-10-29.
pkgload::load_all(quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
args <- commandArgs(trailingOnly = TRUE)
mode <- intersect(args, c('--expect-bug', '--assert-correct'))
args <- setdiff(args, mode)
out <- if (length(args)) args[[1L]] else 'investigation/2026-10-29-datatable-reserved-bug-hunt/summary'
input <- data.table::data.table(g = c('a', 'b'), .BY = c(1, 3), x = c(2, 6))
expressions <- c(
  string_lhs = 'local({".BY" <- c(5, 7); sum(.BY)})',
  string_right_assignment = 'local({c(5, 7) -> ".BY"; sum(.BY)})',
  string_lhs_function = '(function() {".BY" <- c(5, 7); sum(.BY)})()',
  deferred_assignment = 'local({do.call("<-", list(as.name(".BY"), c(5, 7)), envir = environment()); sum(.BY)})',
  computed_assign_get = 'local({(get("assign"))(".BY", c(5, 7)); sum(.BY)})',
  computed_assign_matchfun = 'local({(match.fun("assign"))(".BY", c(5, 7)); sum(.BY)})',
  explicit_eval_list = 'eval(quote(sum(.BY)), list(.BY = c(5, 7)))',
  explicit_evalq_list = 'evalq(sum(.BY), list(.BY = c(5, 7)))',
  explicit_with_list = 'with(list(.BY = c(5, 7)), sum(.BY))',
  # Existing #1033 repair: these are appropriate refusals, not new findings.
  direct_assign = 'local({assign(".BY", c(5, 7)); sum(.BY)})',
  direct_symbol_lhs = 'local({.BY <- c(5, 7); sum(.BY)})',
  formal = '(function(.BY) sum(.BY))(c(5, 7))'
)
run <- function(data, expr, helper, style) {
  dots <- if (style == 'direct') rlang::exprs(value = sum(x)) else {
    rlang::exprs(dplyr::across(x, sum, .names = 'value'))
  }
  summarize_with_margins(data, !!!dots, independent = !!expr,
    fraction = !!rlang::call2(helper, quote(value)), .grouping = rollup(g))
}
attempt <- function(code) tryCatch(list(ok=TRUE, value=force(code)),
  error=function(e)list(ok=FALSE, message=conditionMessage(e), class=class(e)[1]))
fmt <- function(value) if(value$ok) paste(value$value,collapse=',') else value$message
rows <- list(); i <- 0L
for(name in names(expressions)) {
  expr <- rlang::parse_expr(expressions[[name]])
  ordinary <- attempt(collect(summarise(dtplyr::lazy_dt(input), independent=!!expr,.by=g))$independent)
  no_share <- attempt(collect(summarize_with_margins(dtplyr::lazy_dt(input), value=sum(x), independent=!!expr,.grouping=rollup(g)))$independent)
  for(helper in c('share_of_parent','share_of_total')) for(style in c('direct','across')) {
    before <- data.table::copy(input)
    local <- attempt(run(tibble::as_tibble(input),expr,helper,style)$independent)
    safe <- data.table::copy(input)
    data.table::setnames(safe,'.BY','measure')
    renamed <- attempt(collect(run(dtplyr::lazy_dt(safe),expr,helper,style))$independent)
    query <- attempt(run(dtplyr::lazy_dt(input),expr,helper,style))
    actual <- if(query$ok)attempt(collect(query$value)$independent) else query
    stopifnot(identical(input,before))
    if(query$ok && name %in% c('string_lhs','explicit_eval_list') && style == 'direct') {
      writeLines(capture.output(show_query(query$value)),file.path(out,paste0('query-',name,'-',helper,'.txt')))
    }
    i<-i+1L
    rows[[i]]<-data.frame(name,helper,style,expr=expressions[[name]],ordinary=fmt(ordinary),
      no_share=fmt(no_share),local=fmt(local),renamed=fmt(renamed),actual=fmt(actual),
      actual_ok=actual$ok,wrong_value=actual$ok && local$ok && !identical(actual$value,local$value),
      unchanged=identical(input,before),stringsAsFactors=FALSE)
  }
}
results<-do.call(rbind,rows)
write.csv(results,file.path(out,'lexical-ownership.csv'),row.names=FALSE)
print(results[c('name','helper','style','actual_ok','wrong_value','unchanged')],row.names=FALSE)
cat('\nWrong successful values:',sum(results$wrong_value),'of',nrow(results),'cases.\n')
# Minimized calls distinguish expression rewriting from public execution.
for(name in c('string_lhs','explicit_eval_list')) {
  expr<-rlang::parse_expr(expressions[[name]])
  cat('\nINPUT ',name,': ',deparse(expr),'\n',sep='')
  cat('REWRITE: ',deparse(utils::getFromNamespace('rewrite_dtplyr_share_measure', 'marginplyr')(expr, 'safe_alias')),'\n',sep='')
  cat('BINDS: ',utils::getFromNamespace('dtplyr_share_binds_measure', 'marginplyr')(expr),'\n',sep='')
}

stopifnot(all(results$local == '12,12,12'), all(results$renamed == '12,12,12'))
if ("--assert-correct" %in% mode) stopifnot(!any(results$wrong_value)) else stopifnot(sum(results$wrong_value) == 36L, all(results$unchanged))
