Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(".", quiet = TRUE)
set.seed(20261106)
key_values <- list(
  character = c("a", "b", NA_character_, ""),
  numeric = c(0, 1, NA_real_, NaN),
  factor = factor(c("a", "b", NA, "a"), exclude = NULL),
  date = as.Date(c("2020-01-01", "2020-01-02", NA, "2020-01-01")),
  datetime = as.POSIXct(c("2020-01-01", "2020-01-02", NA, "2020-01-01")),
  integer64 = bit64::as.integer64(c("9007199254740992", "9007199254740993", NA, "0"))
)
same <- function(x, y) vctrs::vec_equal(x, y, na_equal = TRUE)
check <- function(out, input, duplicate) {
  # The extra a repeats one grain; the next strictly coarser set supplies a Parent.
  sets <- if (duplicate) list(c("a","b"), "a", "a", character()) else {
    list(c("a","b"), "a", character())
  }
  for(i in seq_len(nrow(out))) {
    id <- out$id[[i]]
    cols <- c("f", sets[[id]])
    matching <- rep(TRUE,nrow(input))
    for(nm in cols) matching <- matching & same(input[[nm]],out[[nm]][i])
    expected_value <- sum(input$x[matching])
    stopifnot(isTRUE(all.equal(out$value[[i]], expected_value)))
    for(kind in c("parent","total")) {
      if(length(sets[[id]])==0L) expected <- 1 else {
        target <- if(kind=="total") length(sets) else {
          which(vapply(sets, function(s) length(s)<length(sets[[id]]),logical(1)))[1]
        }
        parent_rows <- which(out$id==target)
        for(nm in c("f",sets[[target]])) {
          parent_rows <- parent_rows[same(out[[nm]][parent_rows],out[[nm]][i])]
        }
        stopifnot(length(parent_rows)==1L)
        denom <- out$value[[parent_rows]]
        expected <- if(is.na(expected_value)||is.na(denom)||denom==0) NA_real_ else {
          expected_value/denom
        }
      }
      stopifnot(isTRUE(all.equal(out[[kind]][[i]],expected)))
    }
  }
}
rows <- list()
for(a_kind in names(key_values)) {
  for(b_kind in names(key_values)) {
    d <- data.frame(f=sample(c("A","B",NA),18,TRUE),x=sample(c(-2,0,1,3,NA_real_),18,TRUE))
    d$a <- key_values[[a_kind]][sample(4L,18,TRUE)]
    d$b <- key_values[[b_kind]][sample(4L,18,TRUE)]
    for(backend in c("local","dtplyr")) {
      for(duplicate in c(FALSE,TRUE)) {
        input <- if(backend=="local") d else dtplyr::lazy_dt(d)
        spec <- if(duplicate) rollup(a,a,b) else rollup(a,b)
        result <- tryCatch(summarize_with_margins(
          input, value=sum(x), parent=share_of_parent(value),total=share_of_total(value),
          .by=f,.grouping=spec,.duplicates="keep",.margin_label=NULL,.id="id",.sort="last"
        ),error=identity)
        if(!inherits(result,"condition")&&backend=="dtplyr") {
          result<-tryCatch(dplyr::collect(result),error=identity)
        }
        if(inherits(result,"condition")) {
          cat("ERROR",a_kind,b_kind,backend,duplicate,conditionMessage(result),"\n")
          stop("Unexpected operation failure")
        }
        check(result,d,duplicate)
        rows[[length(rows)+1L]] <- data.frame(a=a_kind,b=b_kind,backend=backend,duplicate=duplicate,rows=nrow(result))
      }
    }
  }
}
cat("Completed",length(rows),"comparisons;",sum(vapply(rows,function(r)r$rows,integer(1))),"row oracles\n")
