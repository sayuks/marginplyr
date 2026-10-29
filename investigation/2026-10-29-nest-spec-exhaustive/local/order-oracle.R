args <- commandArgs(TRUE)
pkgload::load_all(if (length(args)) args[[1L]] else ".", quiet = TRUE)
source("investigation/2026-10-29-nest-spec-exhaustive/local/local-fixtures.R")
set.seed(29102912)
Sys.setlocale("LC_COLLATE", "C")
calls <- 0L
for(kind in kinds)for(fixed in c(FALSE,TRUE))for(p in seq_along(plans))for(sort in c("first","last")) {
  x <- tibble::tibble(fixed=make_factor(6L), a=make_value(kind,6L),b=c("c","a",NA_character_,"a","a","c"),row_id=1:6)
  sets <- plans[[p]]; dimensions <- unique(unlist(sets)); groups <- c(if(fixed)"fixed",dimensions)
  source_indices <- set_ids <- integer()
  expected_sigs <- character()
  for(j in seq_along(sets)) {
    keys <- c(if(fixed)"fixed",sets[[j]])
    locs <- if(length(keys))vctrs::vec_group_loc(x[keys])$loc else list(seq_len(nrow(x)))
    source_indices <- c(source_indices,vapply(locs,function(z)z[[1L]],integer(1)))
    set_ids <- c(set_ids,rep(j,length(locs)))
    expected_sigs <- c(expected_sigs,vapply(locs,function(z)paste(j,paste(z,collapse=","),sep=":"),character(1)))
  }
  key <- list()
  add <- function(x) {key[[paste0("k",length(key)+1L)]] <<- x}
  if(fixed) {
    values <- vctrs::vec_slice(x$fixed,source_indices)
    add(as.integer(vctrs::vec_detect_missing(values)));add(values)
  }
  for(dimension in dimensions) {
    missing <- !vapply(sets[set_ids],function(s)dimension %in% s,logical(1))
    add(if(sort=="first")-as.integer(missing) else as.integer(missing))
    values <- vctrs::vec_slice(x[[dimension]],ifelse(missing,1L,source_indices))
    add(ifelse(missing,1L,as.integer(vctrs::vec_detect_missing(values))));add(values)
  }
  add(set_ids)
  expected <- expected_sigs[vctrs::vec_order(tibble::new_tibble(key,nrow=length(set_ids)))]
  for(keep in c(FALSE,TRUE))for(verb in c("nest_with_margins","nest_by_with_margins")) {
    calls <- calls+1L
    out <- rlang::inject((get(verb))(x,.by=!!if(fixed)rlang::sym("fixed")else NULL,
      .grouping=!!expr_sets(sets),.keep=keep,.margin_label=NULL,.id="set",.sort=sort))
    actual <- vapply(seq_len(nrow(out)),function(i)paste(out$set[[i]],paste(out$data[[i]]$row_id,collapse=","),sep=":"),character(1))
    if(!identical(actual,expected))stop("order mismatch ",paste(kind,fixed,p,sort,keep,verb),"\nactual ",paste(actual,collapse=";"),"\nexpected ",paste(expected,collapse=";"))
  }
}
cat("seed 29102912;",calls,"calls; PASS: independent source representatives plus Grouping bits determined requested Margin order\n")
