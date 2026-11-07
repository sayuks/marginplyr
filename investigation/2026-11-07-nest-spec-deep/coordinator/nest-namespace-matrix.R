args <- commandArgs(trailingOnly=TRUE)
source_root <- if(length(args)) args[[1L]] else "."
pkgload::load_all(source_root,quiet=TRUE)
Sys.setenv(TESTTHAT_PARALLEL="false")
out <- file.path(source_root,"investigation/2026-11-07-nest-spec-deep/coordinator")
results <- list()
for(target in c("data.table::as.data.table","base::list","base::min","vctrs::vec_slice")) {
  for(verb in c("nest_with_margins","nest_by_with_margins")) {
    for(stage in c("early","late")) for(plan in c("one","rollup")) {
      for(keep in c(FALSE,TRUE)) for(sort in c("none","first","last")) {
        root <- new.env(parent=globalenv())
        root$source <- data.frame(region=c("east","west"),units=c(13L,29L))
        root$target <- target
        root$calls <- 0L
        input <- evalq(dtplyr::lazy_dt(source),root)
        bytes <- serialize(root$source,NULL)
        f <- getExportedValue("marginplyr",verb)
        grouping <- if(plan=="one") grouping_set(region) else rollup(region)
        build <- function() f(input,.grouping=grouping,.margin_label=NULL,
                              .keep=keep,.sort=sort,.id="set")
        collect <- function(x) if(inherits(x,"dtplyr_step")) dplyr::collect(x) else x
        expected <- collect(build())
        dispatcher <- evalq(function(pkg,name) {
          package <- as.character(substitute(pkg))
          member <- as.character(substitute(name))
          if(identical(paste(package,member,sep="::"),target)) {
            calls <<- calls+1L
            stop("NEST_NAMESPACE_INTERCEPT",call.=FALSE)
          }
          getExportedValue(package,member)
        },root)
        query <- if(stage=="late") build() else NULL
        root$`::` <- dispatcher
        native <- dplyr::collect(input)
        ordinary <- dplyr::collect(dplyr::summarise(input,n=dplyr::n(),.by=region))
        upstream <- dplyr::collect(tidyr::nest(input,data=-region))
        native_calls <- root$calls
        observed <- tryCatch({if(stage=="early") query<-build(); collect(query)},error=identity)
        generated_calls <- root$calls-native_calls
        intercepted <- inherits(observed,"error") && grepl("NEST_NAMESPACE_INTERCEPT",conditionMessage(observed),fixed=TRUE)
        stopifnot(native_calls==0L,identical(native$units,c(13L,29L)),
                  identical(sort(ordinary$n),c(1L,1L)),
                  identical(unlist(lapply(upstream$data,`[[`,"units")),c(13L,29L)),
                  identical(serialize(root$source,NULL),bytes))
        rm("::",envir=root)
        restored <- collect(if(stage=="late" && verb=="nest_with_margins") query else build())
        stopifnot(identical(restored,expected))
        results[[length(results)+1L]] <- data.frame(target,verb,stage,plan,keep,sort,
          native_calls,generated_calls,intercepted,restored=TRUE,source_unchanged=TRUE)
      }
    }
  }
  cat(target,"completed\n")
}
result <- do.call(rbind,results)
utils::write.csv(result,file.path(out,"nest-namespace-matrix.csv"),row.names=FALSE)
print(with(result,table(target,intercepted)))
# The row-wise verb has already collected when a late binding is installed.
expected_interception <- !(result$stage=="late" & result$verb=="nest_by_with_margins")
stopifnot(identical(result$intercepted,expected_interception),
          all((result$generated_calls>0L)==expected_interception))
