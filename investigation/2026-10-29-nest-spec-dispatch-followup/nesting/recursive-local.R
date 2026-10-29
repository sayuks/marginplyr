args <- commandArgs(TRUE)
repo <- if (length(args)) args[[1]] else getwd()
pkgload::load_all(repo, quiet = TRUE)
set.seed(291029)
make_factor <- function(n) {
  f <- factor(sample(c("x", "y", NA_character_), n, TRUE),
    levels = c("x", NA_character_, "y", "unused"), exclude = NULL)
  if (n > 1) f[[1L]] <- NA_integer_
  f
}
make_val <- function(kind, n) switch(kind,
  integer = sample(c(NA_integer_, -1L, 0L, 2L), n, TRUE),
  character = sample(c(NA_character_, "", "1", "2"), n, TRUE),
  double = sample(c(NA, NaN, Inf, -Inf, 0, 1, 1 + 1e-14), n, TRUE),
  factor = make_factor(n),
  ordered = ordered(sample(c("x", "y", NA), n, TRUE), levels = c("unused", "x", "y")),
  date = as.Date(sample(c(NA_real_, 0, 1), n, TRUE), origin = "1970-01-01"),
  time = as.POSIXct(sample(c(NA_real_, 0, 1), n, TRUE), origin = "1970-01-01", tz = "Asia/Tokyo"),
  difftime = as.difftime(sample(c(NA_real_, 0, 1), n, TRUE), units = "hours"),
  complex = sample(c(NA_complex_, 1i, 1+1i), n, TRUE),
  raw = as.raw(sample(0:2, n, TRUE)),
  list = sample(list(NULL, NA, 1L, list(1L), quote(x+1), factor(NA, exclude=NULL)), n, TRUE),
  matrix = matrix(sample(c(NA_real_, 1, 2), n * 2, TRUE), nrow=n, ncol=2),
  matrix0 = matrix(integer(), nrow=n, ncol=0),
  array3 = array(sample(c(NA_real_, 1, 2), n * 4, TRUE), c(n,2,2)),
  packed = tibble::tibble(f=make_factor(n), a=sample(c(NA_real_, NaN, 1), n, TRUE)),
  recursive = tibble::tibble(a=tibble::tibble(f=make_factor(n), x=seq_len(n)),
    b=matrix(seq_len(n * 2), nrow=n, ncol=2)),
  packed0 = tibble::new_tibble(list(), nrow=n))
kinds <- c("integer", "character", "double", "factor", "ordered", "date", "time", "difftime", "complex", "raw", "list", "matrix", "matrix0", "array3", "packed", "recursive", "packed0")
plans <- list(list(c("a","b"),"a",character()), list(c("b"),character(),"a"), list(c("a","b")), list(character()))
sets_expr <- function(sets) {
  rlang::call2("grouping_sets", !!!lapply(sets, function(s) rlang::call2("grouping_set", !!!rlang::syms(s))))
}
comparisons <- calls <- 0L
failures <- list()
for (iteration in seq_len(260)) {
  n <- sample(0:10, 1)
  ka <- sample(kinds, 1); kb <- sample(kinds, 1)
  x <- tibble::tibble(fixed=sample(c("one","two",NA), n, TRUE),
    a=make_val(ka,n), b=make_val(kb,n), row_id=seq_len(n),
    payload=make_val(sample(kinds,1),n),
    recursive=make_val("recursive",n))
  sets <- plans[[sample(seq_along(plans),1)]]
  fixed <- sample(c(FALSE,TRUE),1)
  for (verb in c("nest_with_margins","nest_by_with_margins")) for (keep in c(FALSE,TRUE)) {
    calls <- calls+1L
    by <- if (fixed) rlang::sym("fixed") else NULL
    out <- tryCatch(rlang::inject((get(verb))(x, .by=!!by,
      .grouping=!!sets_expr(sets), .keep=keep, .id="set", .margin_label=NULL,
      .sort=sample(c("none","first","last"),1))), error=identity)
    if (inherits(out,"error")) {
      stopifnot(inherits(out,"marginplyr_error"), grepl("typed-missing", conditionMessage(out),fixed=TRUE), "raw" %in% c(ka,kb))
      failures[[length(failures)+1L]] <- list(iteration=iteration, ka=ka,kb=kb,verb=verb,keep=keep,error=conditionMessage(out))
      next
    }
    out <- dplyr::ungroup(out)
    for (j in seq_along(sets)) {
      keys <- c(if (fixed) "fixed", sets[[j]])
      locs <- if (length(keys)) vctrs::vec_group_loc(x[keys])$loc else if (n || (verb == "nest_by_with_margins" && !fixed && !length(unique(unlist(sets))))) list(seq_len(n)) else list()
      hit <- out[out$set==j,,drop=FALSE]
      if (nrow(hit) != length(locs)) stop("count mismatch ",iteration," ",verb," ",ka," ",kb," ",j)
      got_ids <- lapply(hit$data,function(z) z$row_id)
      stopifnot(identical(sort(vapply(got_ids,paste,character(1),collapse=",")),
        sort(vapply(locs,function(ix) paste(x$row_id[ix],collapse=","),character(1)))))
      cols <- if (keep) c(if(fixed) "fixed",unique(unlist(sets)),setdiff(names(x),c(if(fixed) "fixed", unique(unlist(sets))))) else setdiff(names(x),c(if(fixed) "fixed",unique(unlist(sets))))
      for (z in hit$data) {
        expected <- vctrs::vec_slice(x,z$row_id)[cols]
        if (!identical(as.list(z),as.list(expected))) stop("payload mismatch ",iteration," ",verb," ",keep," ",ka," ",kb)
      }
      comparisons <- comparisons+1L
    }
  }
}
cat("calls",calls,"grouping-set comparisons",comparisons,"failures",length(failures),"\n")
if(length(failures)) print(table(vapply(failures,function(z)z$error,character(1))))
cat("PASS: all admitted local cells matched independently grouped source projections\n")
