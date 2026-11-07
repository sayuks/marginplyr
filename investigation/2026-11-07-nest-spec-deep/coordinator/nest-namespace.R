args <- commandArgs(trailingOnly = TRUE)
pkgload::load_all(if (length(args) && args[[1L]] != "--assert") args[[1L]] else ".", quiet=TRUE)
Sys.setenv(TESTTHAT_PARALLEL="false")
root <- new.env(parent=globalenv())
root$source <- data.frame(region=c("east","west"), units=c(13L,29L))
input <- evalq(dtplyr::lazy_dt(source), root)
before <- serialize(root$source,NULL)
query <- nest_with_margins(input, .grouping=grouping_set(region), .margin_label=NULL)
expected <- dplyr::collect(query)
root$calls <- character()
root$`::` <- evalq(function(pkg,name) {
  package <- as.character(substitute(pkg))
  member <- as.character(substitute(name))
  if (paste(package,member,sep="::") %in% c("data.table::as.data.table","base::list","base::min","vctrs::vec_slice")) {
    calls <<- c(calls,paste(package,member,sep="::"))
    if (identical(member,"as.data.table")) {
      return(function(x,...) { x$units <- rep(-99L,length(x$units)); getExportedValue("data.table","as.data.table")(x,...) })
    }
  }
  getExportedValue(package,member)
},root)
observed <- dplyr::collect(query)
print(observed)
print(lapply(observed$data,as.list))
print(root$calls)
stopifnot(identical(serialize(root$source,NULL),before))
native <- dplyr::collect(input)
stopifnot(identical(native$units,c(13L,29L)))
rm("::",envir=root)
restored <- dplyr::collect(query)
stopifnot(identical(restored,expected))
if ("--assert" %in% args) {
  stopifnot(identical(lapply(observed$data,`[[`,"units"),lapply(expected$data,`[[`,"units")))
}
