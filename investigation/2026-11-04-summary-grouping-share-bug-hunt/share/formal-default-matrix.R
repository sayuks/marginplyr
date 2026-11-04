Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(quiet = TRUE)
removals <- alist(
  base_rm = {base::rm(prior); 1},
  remove_string = {base::remove("prior"); 1},
  rm_list = {base::rm(list = "prior"); 1}
)
# The literal-true branch is test input, rather than a harness decision.
removals$conditional <- rlang::call2(
  "{", rlang::call2("if", TRUE, quote(base::rm(prior))), 1
)
forms <- list(data_frame=identity,tibble=tibble::as_tibble,data_table=data.table::as.data.table)
rows <- list()
for(class in names(forms)) for(kind in c("parent","total")) for(direction in c("after_share","source")) for(fixed in c(FALSE,TRUE)) for(removal in names(removals)) for(demand in c("bare","force")) {
  input <- forms[[class]](data.frame(k=c("one",NA),g=c("a","b"),x=c(1,3),prior=c(100,300)))
  bytes <- serialize(input,NULL)
  by <- if(fixed) quote(k) else NULL
  helper <- rlang::call2(paste0("share_of_",kind),quote(value))
  trigger <- if(demand=="force") quote(force(prior)) else quote(prior)
  fn <- as.call(list(as.name("function"), as.pairlist(list(prior = removals[[removal]])), rlang::expr({!!trigger; sum(prior)})))
  expr <- as.call(list(fn))
  body <- rlang::expr((function(prior=1) {prior; base::rm(prior); sum(prior)})())
  retained <- quote((function(prior=1) {prior; sum(prior)})())
  run <- function(expr, shares=TRUE) tryCatch(if(direction=="after_share") {
    if(shares) summarize_with_margins(input,value=sum(x),prior=!!helper,derived=!!expr,.by=!!by,.grouping=rollup(g)) else summarize_with_margins(input,value=sum(x),prior=sum(x),derived=!!expr,.by=!!by,.grouping=rollup(g))
  } else {
    if(shares) summarize_with_margins(input,prior=sum(x),value=!!expr,fraction=!!helper,.by=!!by,.grouping=rollup(g)) else summarize_with_margins(input,prior=sum(x),value=!!expr,.by=!!by,.grouping=rollup(g))
  },error=identity)
  observed<-run(expr); control<-run(body); constant<-run(retained); ordinary<-run(expr,FALSE)
  stopifnot((is.data.frame(observed) || inherits(observed,"marginplyr_error")), inherits(control,"marginplyr_error"), is.data.frame(constant), is.data.frame(ordinary),identical(bytes,serialize(input,NULL)))
  rows[[length(rows)+1L]] <- data.frame(class,kind,direction,fixed,removal,demand,result=if(is.data.frame(observed)) "missing_refusal" else "Package_refusal",body_control="Package_refusal",retained_control="success",ordinary_control="success")
}
result<-do.call(rbind,rows)
write.csv(result,"investigation/2026-11-04-summary-grouping-share-bug-hunt/share/formal-default-matrix.csv",row.names=FALSE)
print(with(result,table(removal,result)))
cat("Rows:",nrow(result),"; all body/retained/ordinary/source-byte controls passed\n")
