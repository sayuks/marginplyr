# Standalone investigation harness. No repository mutations.
args <- commandArgs(trailingOnly = TRUE)
root <- if (length(args)) args[[1L]] else getwd()
pkgload::load_all(root, quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
options(lifecycle_verbosity = "quiet")
set.seed(20261029)
data <- data.frame(a = c(1L, 2L, NA_integer_), b = c(1.5, 2.5, NA_real_), ch = c("x", "y", NA), l = c(TRUE, FALSE, NA), f = factor(c("a", "b", NA)), d = as.Date(c("2026-01-01", "2026-01-02", NA)), check.names = FALSE)
data[["a,b"]] <- c("comma", "other", NA)
data[["日本語"]] <- c(3L, 4L, 5L)
selector <- function() names(tidyselect::peek_data())[vapply(tidyselect::peek_data(), is.numeric, logical(1))]
cols <- c("ch", "a")
choices <- list(quote(a), quote(c(a, ch)), quote(c(ch, -a)), quote(c(a:b, ch)), quote(!ch), quote(-ch), quote(a/ch), quote(a | ch), quote(a & ch), quote(all_of(c("ch", "a"))), quote(any_of(c("absent", "ch"))), quote(starts_with("a")), quote(matches("[ab]")), quote(where(is.numeric)), quote(where(is.factor)), quote(where(is.character)), quote(where(is.logical)), quote(where(~inherits(.x,"Date"))), quote(all_of(rev(tidyselect::peek_vars()))), quote(all_of(names(tidyselect::peek_data()))), quote(selector()), quote(base::c(cols)), quote((c)(cols)), quote((`|`)(a = 1L, b = 2L)), quote(c()), quote(NULL), quote(character()), quote(.data$a), quote(.data[["ch"]]), quote(`a,b`), quote(`日本語`), quote(all_of(setdiff(names(tidyselect::peek_data()),"ch"))))
leaf <- function(actual, nonempty = FALSE) {
 repeat {
  e <- choices[[sample.int(length(choices), 1)]]
  if (runif(1) < .3) for (i in seq_len(sample(1:3,1))) e <- rlang::call2("(",e)
  cols <- tryCatch(names(tidyselect::eval_select(e, actual, env=globalenv())),error=identity)
  if (!inherits(cols,"error") && (!nonempty || length(cols))) break
 }
 list(expr=e,sets=list(cols),unit=cols)
}
node <- function(actual, depth=0L) {
 kind <- if(depth>=2) "grouping_set" else sample(c("grouping_set","grouping_sets","grouping_spec","rollup","cube"),1)
 count <- if(kind=="grouping_set") sample(0:3,1) else if(kind=="grouping_spec") sample(0:2,1) else sample(1:3,1)
 if(kind=="grouping_set") {
  children <- lapply(seq_len(count),function(i)leaf(actual))
  selected <- unique(unlist(lapply(children, `[[`, "unit"),use.names=FALSE))
  return(list(expr=rlang::call2(kind,!!!lapply(children, `[[`,"expr")),sets=list(selected),unit=selected))
 }
 if(kind %in% c("rollup","cube")) {
  children <- lapply(seq_len(count),function(i) {
   x<-leaf(actual,TRUE)
   if(runif(1)<.5) { x$expr<-rlang::call2("grouping_set",x$expr) } else if(length(x$unit)>1) {
    # Each ordinary selected column is a separate dimension.
    x$unit<-as.list(x$unit)
   }
   x
  })
  units<-unlist(lapply(children,function(x) if(is.list(x$unit))x$unit else list(x$unit)),recursive=FALSE)
  n<-length(units)
  # Refuse to materialize an oracle family that the outer bound will skip.
  if (kind == "cube" && n > 8L) {
   return(list(expr=rlang::call2(kind,!!!lapply(children,`[[`,"expr")), sets=rep(list(character()),257L)))
  }
  if(kind=="rollup") sets<-lapply(rev(seq.int(0L,n)),function(n)unique(unlist(units[seq_len(n)],use.names=FALSE))) else {
   indices<-unlist(lapply(rev(seq.int(0L,n)),function(k)combn(seq_len(n),k,simplify=FALSE)),recursive=FALSE)
   sets<-lapply(indices,function(ix)unique(unlist(units[ix],use.names=FALSE)))
  }
 } else {
  children<-lapply(seq_len(count),function(i)if(runif(1)<.35)leaf(actual) else node(actual,depth+1L))
  if(kind=="grouping_sets") sets<-unlist(lapply(children,`[[`,"sets"),recursive=FALSE) else {
   sets<-list(character())
   for(child in children) sets<-unlist(lapply(sets,function(l)lapply(child$sets,function(r)unique(c(l,r)))),recursive=FALSE)
  }
 }
 list(expr=rlang::call2(kind,!!!lapply(children,`[[`,"expr")),sets=sets)
}
con<-DBI::dbConnect(duckdb::duckdb(shared_home=FALSE))
sources<-list(local=data,dtplyr=dtplyr::lazy_dt(data),arrow=arrow::Table$create(data),duckdb=copy_to(con,data,"grouping_typed_hunt",temporary=TRUE))
checked<-0L;skipped<-0L
for(b in names(sources)) {
 actual<-if(b=="local")data else collect(sources[[b]])
 for(i in seq_len(350)) {
  candidate<-node(actual)
  if(length(candidate$sets)>256L) {skipped<-skipped+1L;next}
  spec<-eval(candidate$expr,globalenv())
  observed<-tryCatch(inspect_grouping(sources[[b]],.grouping=spec,.duplicates="keep",.format="list"),error=identity)
  dims<-unique(unlist(candidate$sets,use.names=FALSE))
  expected<-lapply(candidate$sets,function(x)dims[dims %in% x])
  expected<-lapply(expected,as.character)
  if(inherits(observed,"error")||!identical(observed$included,expected)) {
   cat("FAIL",b,i,"\n");print(candidate$expr);print(expected);print(observed)
   saveRDS(list(candidate=candidate,observed=observed,backend=b),file.path(tempdir(), "margin-hunt-typed-plan-failure.rds"))
   stop("Grouping plan disagrees with independent typed selection oracle")
  }
  checked<-checked+1L
 }
}
DBI::dbDisconnect(con,shutdown=TRUE)
cat("PASS typed Grouping plan oracle:",checked,"checks;",skipped,"occurrence-bound skips\n")
