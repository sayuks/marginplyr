args <- commandArgs(trailingOnly = TRUE)
source_root <- if (length(args)) args[[1L]] else "."
pkgload::load_all(source_root, quiet = TRUE)
Sys.setenv(TESTTHAT_PARALLEL = "false")
out <- file.path(source_root, "investigation/2026-11-07-nest-spec-deep/coordinator")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
fixtures <- list(
  character = c("b", "a", NA, "a"),
  numeric = c(2, 1, NA, NaN),
  logical = c(TRUE, FALSE, NA, FALSE),
  factor = factor(c("b", "a", NA, "a"), levels = c("b", "a", "unused")),
  ordered = ordered(c("b", "a", NA, "a"), levels = c("b", "a", "unused")),
  na_factor = structure(c(1L, 2L, NA_integer_, 1L), levels = c("a", NA_character_), class = "factor"),
  date = as.Date(c("2020-01-02", "2020-01-01", NA, "2020-01-01")),
  datetime = as.POSIXct(c(2, 1, NA, 1), origin = "1970-01-01", tz = "Asia/Tokyo"),
  difftime = as.difftime(c(2, 1, NA, 1), units = "hours"),
  hms = hms::hms(c(2, 1, NA, 1)),
  integer64 = bit64::as.integer64(c("9007199254740992", "9007199254740993", NA, "9007199254740992")),
  complex = c(2+1i, 1+2i, NA_complex_, 1+2i),
  raw = as.raw(c(2, 1, 0, 1)),
  list = list(2L, 1L, NULL, 1L),
  matrix = I(matrix(c(2L,1L,NA,1L,4L,3L,NA,3L), ncol=2))
)
canonical <- function(x, keep_order) {
  x <- dplyr::ungroup(tibble::as_tibble(x, .name_repair = "minimal"))
  records <- lapply(seq_len(nrow(x)), function(i) {
    cell <- x$data[[i]]
    ids <- cell$row_id
    # Source row order is not part of the API; exact projected values are.
    cell <- tibble::as_tibble(cell, .name_repair = "minimal")
    cell <- cell[order(ids), , drop = FALSE]
    # Backend-specific element classes are delegated, not compared.
    list(outer = as.list(x[i, setdiff(names(x), "data"), drop = FALSE]),
         cell = as.list(cell), n = nrow(cell))
  })
  if (!keep_order && length(records)) {
    keys <- vapply(records, function(z) paste(z$outer$set, paste(z$cell$row_id, collapse=":")), character(1))
    records <- records[order(keys)]
  }
  records
}
rows <- list()
for (type in names(fixtures)) for (size in c(0L, 4L)) {
  dat <- tibble::tibble(g = vctrs::vec_slice(fixtures[[type]], seq_len(size)),
                        fixed = rep(c("q", NA_character_), length.out=size),
                        row_id = seq_len(size),
                        payload = factor(rep(c("z", "x"), length.out=size), levels=c("z","x","unused")))
  for (verb in c("nest_with_margins", "nest_by_with_margins")) {
    for (keep in c(FALSE, TRUE)) for (sort in c("none", "first", "last")) {
      for (fixed in c(FALSE, TRUE)) {
        f <- getExportedValue("marginplyr", verb)
        run <- function(data) tryCatch({
          z <- f(data, .by = dplyr::all_of(if (fixed) "fixed" else character()), .grouping = rollup(g),
                 .margin_label = NULL, .id="set", .sort=sort, .keep=keep)
          if (inherits(z,"dtplyr_step")) z <- dplyr::collect(z)
          canonical(z, sort != "none")
        }, error=function(e) e)
        local <- run(dat)
        lazy <- run(dtplyr::lazy_dt(dat))
        status <- if (inherits(local,"error") || inherits(lazy,"error")) "refusal" else if (identical(local,lazy)) "match" else "MISMATCH"
        detail <- if (status == "MISMATCH") paste(capture.output(all.equal(local,lazy)),collapse=" | ") else paste(
          if (inherits(local,"error")) paste("local:",conditionMessage(local)),
          if (inherits(lazy,"error")) paste("dtplyr:",conditionMessage(lazy)), collapse=" | ")
        rows[[length(rows)+1L]] <- data.frame(type,size,verb,keep,sort,fixed,status,detail)
      }
    }
  }
  cat(type,size,"completed\n")
}
result <- do.call(rbind,rows)
utils::write.csv(result,file.path(out,"dt-nest-differential.csv"),row.names=FALSE)
print(table(result$status))
print(unique(result[result$status=="MISMATCH",c("type","size","keep","sort","fixed","detail")]))
capture.output(sessionInfo(),file=file.path(out,"environment.txt"))
