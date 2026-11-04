Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(quiet = TRUE)
probe <- function(helper, late, kind) {
  scope <- new.env(parent = globalenv())
  scope$input <- data.frame(a = c("x", "x", "y"), b = c("p", "q", "p"), x = c(1,2,3))
  step <- evalq(dtplyr::lazy_dt(input), scope)
  make <- function() summarize_with_margins(step, value = sum(x), frac = !!rlang::call2(paste0("share_of_",kind),rlang::sym("value")), .grouping = rollup(a,b), .id = "id", .sort = "none")
  scope$calls <- 0L
  callback <- evalq(function(...) {calls <<- calls + 1L; stop("CALLER_HELPER_RAN")}, scope)
  if (!late) assign(helper, callback, scope)
  out <- tryCatch({result <- make(); if(late) assign(helper, callback, scope); as.data.frame(dplyr::collect(result))},error=function(e)e)
  data.frame(helper,late,kind,calls=scope$calls,result=if(inherits(out,"error")) conditionMessage(out) else paste(round(out$frac,4),collapse=","))
}
rows <- do.call(rbind,lapply(c("is.na","as.double","/","*","==","|","!","[","match","ifelse","fifelse","%in%","c","list"),function(helper)do.call(rbind,lapply(c(TRUE,FALSE),function(late)do.call(rbind,lapply(c("total","parent"),function(kind)probe(helper,late,kind)))))))
print(rows,row.names=FALSE)
write.csv(rows,"investigation/2026-11-04-summary-grouping-share-bug-hunt/share/helper-discovery.csv",row.names=FALSE)
