Sys.setenv(TESTTHAT_PARALLEL="false")
args <- commandArgs(trailingOnly=TRUE)
pkgload::load_all(args[[1]], quiet=TRUE)
library(dplyr)
d <- tibble(g=c("a","b"), value=c(2,6), p=c(10,20))
inner <- tibble(x=1)
forms <- list(positional=quote(dplyr::mutate(tibble(x=p))), exact=quote(dplyr::mutate(.data=tibble(x=p))), partial=quote(dplyr::mutate(.d=tibble(x=p))), reorder=quote(dplyr::mutate(z=x, .data=tibble(x=p))), rowwise=quote(dplyr::rowwise(data=tibble(x=p))), count=quote(dplyr::count(x=tibble(x=p))), nested=quote(dplyr::mutate(dplyr::mutate(tibble(x=p)))))
for (name in names(forms)) {
 expr <- expr(summarize_with_margins(d, n=sum(value), p=share_of_total(n), out=list(!!forms[[name]]), .grouping=rollup(g)))
 ans <- tryCatch(eval(expr), error=identity)
 stopifnot(inherits(ans,"marginplyr_error"))
 cat(name, class(ans)[[1]], "\n")
}
cat("PASS: all seven nested-input spellings retain prohibited dependency refusal\n")
