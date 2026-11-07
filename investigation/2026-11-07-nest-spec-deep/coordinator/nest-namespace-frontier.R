pkgload::load_all(".",quiet=TRUE)
Sys.setenv(TESTTHAT_PARALLEL="false")
rows <- list()
for (kind in c("empty_payload","special_payload","na_factor")) {
  for (verb in c("nest_with_margins","nest_by_with_margins")) {
    target <- switch(kind,empty_payload="dplyr::tibble",special_payload="data.table::setnames",na_factor="base::as.integer")
    root <- new.env(parent=globalenv())
    root$source <- switch(kind,
      empty_payload=data.frame(g=c("a","a")),
      special_payload=structure(data.frame(g=c("a","a"),x=1:2),names=c("g",".SD")),
      na_factor=data.frame(g=structure(c(1L,2L,NA_integer_),levels=c("a",NA_character_),class="factor"),x=1:3))
    root$target <- target
    root$calls <- 0L
    input <- evalq(dtplyr::lazy_dt(source),root)
    before <- serialize(root$source,NULL)
    f <- getExportedValue("marginplyr",verb)
    run <- function() {
      z <- f(input,.grouping=grouping_set(g),.margin_label=NULL)
      if(inherits(z,"dtplyr_step")) dplyr::collect(z) else z
    }
    expected <- run()
    root$`::` <- evalq(function(pkg,name) {
      package <- as.character(substitute(pkg)); member <- as.character(substitute(name))
      if(identical(paste(package,member,sep="::"),target)) {
        calls <<- calls+1L
        stop("NEST_FRONTIER_INTERCEPT",call.=FALSE)
      }
      getExportedValue(package,member)
    },root)
    observed <- tryCatch(run(),error=identity)
    stopifnot(identical(serialize(root$source,NULL),before))
    intercepted <- inherits(observed,"error") && grepl("NEST_FRONTIER_INTERCEPT",conditionMessage(observed),fixed=TRUE)
    rm("::",envir=root)
    stopifnot(identical(run(),expected))
    rows[[length(rows)+1L]] <- data.frame(kind,verb,target,intercepted,calls=root$calls)
  }
}
r <- do.call(rbind,rows)
print(r,row.names=FALSE)
utils::write.csv(r,"investigation/2026-11-07-nest-spec-deep/coordinator/nest-namespace-frontier.csv",row.names=FALSE)
stopifnot(all(r$intercepted),all(r$calls>0L))
