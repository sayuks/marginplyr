suppressPackageStartupMessages(library(dplyr))
pkgload::load_all(if(length(commandArgs(TRUE)))commandArgs(TRUE)[1L] else getwd(), quiet=TRUE)
data <- data.frame(g=c('a','b'),x=c(1,3))
cases <- list(
  quote=quote(length(quote(share_of_total(value)))),
  substitute=quote(length(substitute(share_of_total(value)))),
  qualified_bquote_share=quote(length(base::bquote(share_of_total(value)))),
  qualified_bquote_grouping=quote(length(base::bquote(grouping_id(g)))),
  qualified_expr_share=quote(length(rlang::expr(share_of_total(value)))),
  qualified_quo_share=quote(length(rlang::quo(share_of_total(value)))),
  qualified_bquote_substitution=quote(length(base::bquote(.(grouping_id(g)))))
)
for(nm in names(cases)) {
  result <- summarize_with_margins(data,value=sum(x),out=!!cases[[nm]],.grouping=rollup(g))
  expected <- if(nm=='qualified_quo_share') 2L else if(nm=='qualified_bquote_substitution') 1L else 2L
  stopifnot(identical(result$out,rep(expected,3L)))
  cat(nm,'PASS\n')
}
# Bare bquote is intentionally ordinary code under the documented qualification
# rule; it is retained here as a rejected candidate rather than a regression.
error <- tryCatch(summarize_with_margins(data,value=sum(x),out=length(bquote(share_of_total(value))),.grouping=rollup(g)),error=identity)
stopifnot(inherits(error,'marginplyr_error'))
cat('bare bquote: documented refusal; discarded candidate\n')
