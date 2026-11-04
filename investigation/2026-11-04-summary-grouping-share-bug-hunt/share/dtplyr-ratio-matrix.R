Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(quiet = TRUE)
helpers <- c("is.na", "as.double", "/", "*", "==", "|", "%in%")
make_callback <- function(helper, mode, root) {
  original <- get(helper, baseenv())
  corrupt <- switch(helper,
    "is.na" = function(x) rep(TRUE, length(x)),
    "as.double" = function(x) base::as.double(x) + 1,
    "/" = function(e1, e2) rep(17, length(e1)),
    "*" = function(e1, e2) e1 * e2 * 10,
    "==" = function(e1, e2) rep(TRUE, max(length(e1), length(e2))),
    "|" = function(e1, e2) rep(TRUE, max(length(e1), length(e2))),
    "%in%" = function(x, table) rep(FALSE, length(x)))
  function(...) {
    root$calls <- root$calls + 1L
    if (mode == "raise") stop("caller ratio helper ran")
    if (mode == "corrupt") corrupt(...) else original(...)
  }
}
rows <- list()
for (helper in helpers) for (kind in c("parent","total")) for (late in c(FALSE,TRUE)) for (mode in c("count","raise","corrupt")) for (fixed in c(FALSE,TRUE)) for (across in c(FALSE,TRUE)) {
  root <- new.env(parent = globalenv())
  root$source <- data.frame(k=c("one","one",NA,NA), a=c("x","x","y","y"), b=c("p","q","p","q"), x=c(1,2,3,4))
  original_source <- serialize(root$source,NULL)
  input <- evalq(dtplyr::lazy_dt(source),root)
  by <- if(fixed) quote(k) else NULL
  source <- quote(dplyr::across(x,sum,.names="value"))
  dots <- if(across) list(source) else list(value=quote(sum(x)))
  call <- rlang::expr(summarize_with_margins(input, !!!dots, fraction=!!rlang::call2(paste0("share_of_",kind),quote(value)), .by=!!by,.grouping=rollup(a,b), .id="id",.sort="none"))
  expected <- as.data.frame(eval(call))
  plain <- summarize_with_margins(input,value=sum(x),.by=!!by,.grouping=rollup(a,b),.id="id",.sort="none")
  root$calls <- 0L
  if(!late) assign(helper,make_callback(helper,mode,root),root)
  query <- eval(call)
  if(late) assign(helper,make_callback(helper,mode,root),root)
  actual <- tryCatch(as.data.frame(dplyr::collect(query)), error=identity)
  calls <- root$calls
  plain_result <- tryCatch(as.data.frame(dplyr::collect(plain)),error=identity)
  input_result <- tryCatch(as.data.frame(dplyr::collect(input)),error=identity)
  stopifnot(!inherits(plain_result,"error"),!inherits(input_result,"error"),identical(original_source,serialize(root$source,NULL)))
  result <- if(inherits(actual,"error")) "callback_error" else if(!isTRUE(all.equal(actual$fraction,expected$fraction))) "wrong_ratio" else if(calls>0) "callback_invoked" else "isolated"
  if(helper=="%in%") stopifnot(result=="isolated") else stopifnot(result==switch(mode,count="callback_invoked",raise="callback_error",corrupt="wrong_ratio"))
  rows[[length(rows)+1L]] <- data.frame(helper,kind,late,mode,fixed,across,calls,result)
}
result <- do.call(rbind,rows)
write.csv(result,"investigation/2026-11-04-summary-grouping-share-bug-hunt/share/dtplyr-ratio-matrix.csv",row.names=FALSE)
print(with(result,table(helper,result)))
cat("Rows:",nrow(result),"; all no-share/input/source-byte controls passed\n")
