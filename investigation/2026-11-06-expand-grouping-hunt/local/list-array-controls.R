# Small related-boundary controls supplement the primary minimal reproducer.
pkgload::load_all(quiet = TRUE)
Sys.setenv(TESTTHAT_PARALLEL = "false")
capture <- function(expr) tryCatch(list(value=force(expr)),error=function(e)list(error=class(e)[[1L]],message=conditionMessage(e)))
for (rank in 1:3) {
  shape <- c(2L, rep(1L, rank-1L))
  key <- array(list(7L, NULL),dim=shape)
  input <- tibble::tibble(g=key,payload=c(19L,23L))
  prototype <- vctrs::vec_init(key,2L)
  control <- dplyr::union_all(input,dplyr::mutate(input,g=prototype))
  stopifnot(nrow(control)==4L,identical(dim(control$g),c(4L,shape[-1L])),
            identical(control$payload,c(19L,23L,19L,23L)))
  included <- expand_with_margins(input,.grouping=grouping_set(g),.margin_label=NULL)
  stopifnot(identical(included,input))
  payload <- expand_with_margins(input,.grouping=grouping_set(),.margin_label=NULL)
  stopifnot(identical(payload,input))
  for (verb in c("expand_with_margins","summarize_with_margins","nest_with_margins","nest_by_with_margins")) {
    f<-getExportedValue("marginplyr",verb)
    actual<-capture(f(input,.grouping=rollup(g),.margin_label=NULL))
    stopifnot(!is.null(actual$error),grepl("cannot be represented",actual$message))
    cat("rank=",rank," verb=",verb,": false representability refusal; ordinary/included/payload controls PASS\n",sep="")
  }
}
# A raw array has no typed missing cell: its refusal remains required.
raw_input <- tibble::tibble(g=matrix(as.raw(7L),nrow=1L),payload=19L)
raw_result <- capture(expand_with_margins(raw_input,.grouping=rollup(g),.margin_label=NULL))
stopifnot(!is.null(raw_result$error),grepl("cannot be represented",raw_result$message))
cat("Raw-array negative control: required refusal PASS\n")
# String display labels remain outside the local structured-key contract.
text_input <- tibble::tibble(g=matrix(list(7L),nrow=1L),payload=19L)
text_result <- capture(expand_with_margins(text_input,.grouping=rollup(g),.margin_label="All"))
stopifnot(!is.null(text_result$error),grepl("matrix or array",text_result$message))
cat("String-label negative control: required refusal PASS\n")
