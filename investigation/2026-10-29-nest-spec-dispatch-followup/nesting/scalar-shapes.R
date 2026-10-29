args <- commandArgs(TRUE)
pkgload::load_all(if(length(args)) args[[1]] else ".",quiet=TRUE)
arguments <- list(.id="set", .key="cell", .margin_label="Total", .margin_label_position="last", .sort="none", .duplicates="error", .keep=TRUE, .check_margin_label=TRUE)
shapes <- list(named=function(x)stats::setNames(x,"arbitrary"), AsIs=I, matrix=function(x)matrix(x,1,1), array=function(x)array(x,c(1,1,1)))
canon <- function(result,key) {
  result <- dplyr::ungroup(dplyr::collect(result))
  result[[key]] <- lapply(result[[key]],function(z)as.list(z))
  as.list(result)
}
errors <- list(); matches <- calls <- 0L
for(n in c(0L,2L))for(backend in c("local","dtplyr"))for(verb in c("nest_with_margins","nest_by_with_margins"))for(plan in c("none","rollup"))for(arg in names(arguments))for(shape in names(shapes)) {
  if(arg==".margin_label" && shape=="named") next
  x <- tibble::tibble(g=rep("a",n),value=seq_len(n))
  data <- if(backend=="local")x else dtplyr::lazy_dt(x)
  base_args <- list(.data=data,.grouping=if(plan=="rollup")rollup(g) else NULL)
  baseline <- do.call(get(verb),c(base_args,stats::setNames(list(arguments[[arg]]),arg)))
  altered <- tryCatch(do.call(get(verb),c(base_args,stats::setNames(list(shapes[[shape]](arguments[[arg]])),arg))),error=identity)
  calls <- calls+1L
  if(inherits(altered,"error")) {
    errors[[length(errors)+1L]] <- list(n=n,backend=backend,verb=verb,plan=plan,arg=arg,shape=shape,error=conditionMessage(altered))
    stopifnot(n==0L,verb=="nest_by_with_margins",plan=="none",arg==".id",shape %in% c("matrix","array"))
  } else {
    key <- if(arg==".key")"cell" else "data"
    stopifnot(identical(canon(baseline,key),canon(altered,key)))
    matches <- matches+1L
  }
}
cat("altered calls",calls,"matched canonical scalar controls",matches,"errors",length(errors),"\n")
if(length(errors)) print(vapply(errors,function(z)paste(z$backend,z$verb,z$shape,z$error,sep=" | "),character(1)))
cat("PASS: only the separately reproduced empty rowwise dimensional-identifier defect was observed\n")
