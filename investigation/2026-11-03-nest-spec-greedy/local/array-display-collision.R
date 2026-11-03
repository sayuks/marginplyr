pkgload::load_all(quiet=TRUE)
Sys.setenv(TESTTHAT_PARALLEL="false")
# A scalar array whose display method retains its one-dimensional shape.
# posterior::rvar supplies an independent installed-package witness.
as.character.hunt_display_array <- function(x, ...) {
  array(as.character(unclass(x)),dim=dim(x))
}
g<-structure(1L,dim=1L,class=c("hunt_display_array","array"))
input<-tibble::tibble(g=g,row_id=1L)
stopifnot(identical(tidyr::nest(input,.by=g)$data[[1]]$row_id,1L))
results<-list()
for(verb in c("nest_with_margins","nest_by_with_margins","expand_with_margins","summarize_with_margins")) {
  invoke<-function(check,label="Total") {
    if(verb=="summarize_with_margins") {
      summarize_with_margins(input,n=dplyr::n(),.grouping=grouping_set(g),
        .margin_label=label,.check_margin_label=check)
    } else get(verb)(input,.grouping=grouping_set(g),.margin_label=label,
      .check_margin_label=check)
  }
  result<-tryCatch(invoke(TRUE),error=identity)
  cat(verb,":",if(inherits(result,"error")) conditionMessage(result) else "success","\n")
  results[[verb]]<-result
  unchecked<-invoke(FALSE)
  stopifnot(nrow(unchecked)==1L,identical(as.vector(unchecked$g),"1"))
  collision<-tryCatch(invoke(TRUE,"1"),error=identity)
  cat("observed collision:",if(inherits(collision,"error")) class(collision)[1] else "silently accepted","\n")
}
print(sessionInfo())
if("--assert" %in% commandArgs(trailingOnly=TRUE)) {
  stopifnot(all(vapply(results,function(z) !inherits(z,"error") && nrow(z)==1L,logical(1))))
} else stopifnot(all(vapply(results,function(z) inherits(z,"error") && !inherits(z,"marginplyr_error"),logical(1))))
