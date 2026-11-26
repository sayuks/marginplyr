# Investigated SHA: 0fe5f1e40daa14e2ba2b5246452fa668bc330f80
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# When a bare name in .grouping is bound and its evaluation fails, the bare-column refusal replaces the error whenever the input has a column of that name.
suppressMessages(pkgload::load_all(".", quiet = TRUE))
suppressPackageStartupMessages({library(dplyr)})
invisible(Sys.setlocale("LC_MESSAGES", "C")); Sys.setenv(LANGUAGE = "en")
show <- function(label, expr) {
  r <- tryCatch({force(expr); "RETURNED"}, error = function(e) sprintf("ERROR <%s>: %s | parent: %s",
    paste(class(e), collapse = "/"), gsub("\n", " ", conditionMessage(e)),
    if (is.null(e$parent)) "NULL" else conditionMessage(e$parent)))
  setTimeLimit(elapsed = Inf); cat(label, "\n   ", r, "\n")
}
sales <- data.frame(region = c("E", "E", "W"), store = c("A", "B", "C"), v = 1:3)
# A caller's specification builder that refuses an unknown dimension.
make_spec <- function(dim) {
  if (!identical(dim, "region")) stop("unknown dimension: ", dim)
  rollup(region)
}
# A wrapper whose parameter happens to be named like a column.
report <- function(data, store) summarize_with_margins(data, n = n(), .grouping = store)
peek <- function(data, store) inspect_grouping(data, .grouping = store)

cat("expected: the builder's own error 'unknown dimension: zone' (or a condition carrying it)\n")
show("observed, summarize_with_margins(), data has a column `store`:", report(sales, make_spec("zone")))
show("observed, inspect_grouping(), same:", peek(sales, make_spec("zone")))
show("control, same wrapper and builder, data without a `store` column:",
     report(rename(sales, shop = store), make_spec("zone")))
show("control, builder succeeds, data with `store`:", report(sales, make_spec("region")))
native <- function(data, store) { spec <- store; summarise(group_by(data, region), n = n()) }
show("native control: plain dplyr wrapper forcing the same promise:", native(sales, make_spec("zone")))
slow_spec <- function() { for (i in 1:20) Sys.sleep(0.025); rollup(region) }
show("observed, time limit 0.2 s firing while the promise evaluates:",
     {setTimeLimit(elapsed = 0.2, transient = TRUE); report(sales, slow_spec())})
show("native control, same time limit, plain R forcing the same promise:",
     {setTimeLimit(elapsed = 0.2, transient = TRUE); native(sales, slow_spec())})
e <- new.env(); makeActiveBinding("region", function() stop("binding failed"), e)
show("observed, an active binding named like a column that raises:",
     eval(quote(summarize_with_margins(sales, n = n(), .grouping = region)), e))
