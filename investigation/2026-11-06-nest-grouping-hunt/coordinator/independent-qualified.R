Sys.setenv(TESTTHAT_PARALLEL="false")
pkgload::load_all(Sys.getenv("MARGINPLYR_HUNT_SOURCE", "."), quiet=TRUE)
x <- tibble::tibble(detail=c("east","west"), partition=c("one","one"), value=11:12)
seen <- 0L
lexical <- new.env(parent=globalenv())
lexical$held <- grouping_set(detail)
lexical[["::"]] <- function(package, name) {
  seen <<- seen + 1L
  function(arg) grouping_set(partition)
}
q <- rlang::new_quosure(quote(base::`(`(held)), lexical)
evaluated <- rlang::eval_tidy(q)
stopifnot(identical(seen,1L))
expected <- inspect_grouping(x,.grouping=evaluated,.format="list")
stopifnot(identical(expected$included,list("partition")))
seen <- 0L
observed <- inspect_grouping(x,.grouping=grouping_sets(!!q),.format="list")
cat("Evaluated authored expression:",expected$included[[1L]],"\n")
cat("Nested expression:",observed$included[[1L]],"namespace lookups:",seen,"\n")
if ("--assert" %in% commandArgs(TRUE)) stopifnot(identical(observed,expected),identical(seen,1L))
stopifnot(identical(observed$included,list("detail")),identical(seen,0L))
for (verb in list(nest_with_margins,nest_by_with_margins)) {
  wrong <- verb(x,.grouping=grouping_sets(!!q))
  right <- verb(x,.grouping=evaluated)
  cat("Nested cells:",nrow(wrong),"evaluated expression cells:",nrow(right),"\n")
  stopifnot(nrow(wrong)==2L,nrow(right)==1L,
            identical(sort(unlist(lapply(wrong$data,`[[`,"value"))),11:12),
            identical(right$data[[1L]]$value,11:12))
}
plain <- rlang::new_quosure(quote(base::`(`(held)),rlang::env(held=grouping_set(detail)))
stopifnot(identical(inspect_grouping(x,.grouping=grouping_sets(!!plain),.format="list")$included,list("detail")))
cat("Plain base-qualified wrapper control passed.\n")
