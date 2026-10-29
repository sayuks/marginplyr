pkgload::load_all('.',quiet=TRUE)
d <- data.frame(g='a')
run <- function(expr) tryCatch(rlang::eval_tidy(rlang::expr(
 summarize_with_margins(d,bit=!!expr,.grouping=grouping_set(g))
)),error=identity)
for (expr in rlang::exprs(
 eval(quote(grouping_bit(g))),
 eval(str2lang('grouping_bit(g)')),
 base::eval(base::str2lang('grouping_bit(g)')),
 eval(parse(text='grouping_bit(g)')),
 rlang::eval_tidy(str2lang('grouping_bit(g)')),
 eval(str2lang('dplyr::cur_group_id()'))
)) {
 actual <- run(expr)
 cat(deparse1(expr), '\n')
 if(inherits(actual,'error')) cat(class(actual)[[1L]],': ',conditionMessage(actual),'\n',sep='') else print(actual)
}
grouping_bit <- function(...) 99L
shadow <- run(quote(eval(str2lang('grouping_bit(g)'))))
cat('Shadow control: ',paste(shadow$bit,collapse=','),'\n',sep='')
rm(grouping_bit)
actual <- run(quote(eval(str2lang('grouping_bit(g)'))))
stopifnot(!inherits(actual,'error'),identical(actual$bit,0L))
