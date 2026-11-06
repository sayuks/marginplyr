args <- commandArgs(TRUE)
stopifnot(length(args) == 2L)
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(args[[1L]], quiet = TRUE)
library(dplyr)
inner_names <- list(c("x", "x"), c("", ""), c(NA, "b"), c(".data", ".env"), c("..1", "..."), c("TRUE", "FALSE"), c("line\nbreak", "tick`quote"), c("日本語", "😀"))
missing_rows <- function(x) {
  if (is.data.frame(x)) return(Reduce(`&`, lapply(x, missing_rows), init = rep(TRUE, nrow(x))))
  vctrs::vec_detect_missing(x)
}
rows <- list()
for (nm in seq_along(inner_names)) for (depth in 0:2) for (n in c(0L, 1L, 3L)) for (role in c("dimension", "fixed", "payload")) for (container in c("tibble", "data.frame")) {
  g <- structure(list(c(2L, NA, 1L), c(NA, NA, 3L)), names=inner_names[[nm]], class="data.frame", row.names=1:3)
  for (d in seq_len(depth)) g <- tibble(packed = g)
  g <- vctrs::vec_slice(g,seq_len(n))
  x <- tibble(fixed = c("g", "g", NA)[seq_len(n)], a = c(2L, NA, 1L)[seq_len(n)], b = c("b", "a", NA)[seq_len(n)], g = g, payload = seq_len(n))
  if (container == "data.frame") x <- as.data.frame(x)
  before <- serialize(x,NULL)
  dimensions <- if (role == "dimension") c("g", "b") else c("a", "b")
  fixed <- if (role == "fixed") "g" else "fixed"
  sets <- list(dimensions, dimensions[[1L]], character())
  reference <- tryCatch({
    branches <- lapply(seq_along(sets), function(s) {
      y <- as_tibble(x)
      for (k in setdiff(dimensions,sets[[s]])) y[[k]] <- vctrs::vec_init(x[[k]],n)
      y$sid <- rep(s,n)
      y
    })
    y <- Reduce(union_all,branches)
    y[c(fixed, dimensions, "sid",setdiff(names(x),c(fixed,dimensions)))]
  },error=identity)
  for (sorting in c("none", "first", "last")) {
    expected <- tryCatch({
      if (inherits(reference,"error")) stop(reference)
      y <- reference
      if (sorting != "none") {
        keys <- list(missing_rows(y[[fixed]]),y[[fixed]])
        for (k in dimensions) {
          bits <- vapply(y$sid,function(s) as.integer(!k %in% sets[[s]]),integer(1))
          if (sorting == "first") bits <- -bits
          keys <- c(keys,list(bits,missing_rows(y[[k]]),y[[k]]))
        }
        keys <- c(keys,list(y$sid))
        y <- vctrs::vec_slice(y,vctrs::vec_order(tibble::new_tibble(setNames(keys,paste0("v",seq_along(keys))),nrow=nrow(y))))
      }
      y
    },error=identity)
    actual <- tryCatch(expand_with_margins(x,.by=all_of(fixed),.grouping=rollup(all_of(dimensions[[1L]]),all_of(dimensions[[2L]])),.margin_label=NULL,.id="sid",.sort=sorting),error=identity)
    if (!inherits(actual,"error")) actual <- as_tibble(actual)
    status <- if (inherits(expected,"error")) "ordinary_failure" else if (inherits(actual,"error")) "package_error" else if (!identical(expected,actual)) "difference" else "match"
    detail <- if (inherits(actual,"error")) conditionMessage(actual) else if (status == "difference") paste(all.equal(actual,expected),collapse="; ") else if (inherits(expected,"error")) conditionMessage(expected) else ""
    stopifnot(identical(serialize(x,NULL),before))
    rows[[length(rows)+1L]] <- data.frame(names=nm,depth=depth,n=n,role=role,container=container,sort=sorting,status=status,detail=detail)
  }
}
result <- bind_rows(rows)
write.csv(result,args[[2L]],row.names=FALSE)
print(table(result$status))
print(head(result[result$status %in% c("difference","package_error"),],20))
cat("comparisons",nrow(result),"\n")
print(sessionInfo())
