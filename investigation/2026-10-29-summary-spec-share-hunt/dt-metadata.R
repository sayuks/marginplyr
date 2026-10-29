pkgload::load_all(quiet=TRUE)
library(dplyr)
x <- data.table::data.table(a=1:2, b=c('x','y'))
r <- dtplyr::lazy_dt(x)
exprs <- list(quote(a+0.5),quote(a/2L),quote(a+1),quote(a*1.0),quote(-a),quote(a+a),quote(1/a),quote(a/0),quote(a-a))
for(e in exprs) {
 q <- mutate(r, a=!!e)
 actual <- collect(q)
 p <- utils::getFromNamespace("dtplyr_selection_proxy", "marginplyr")(q)
 cat(deparse(e),' actual=',typeof(actual$a),' proxy=',typeof(p$a),'\n')
 print(inspect_grouping(q,.by=where(is.double)))
}
