pkgload::load_all(quiet = TRUE)

dot_names <- c("..0", "..-1", "..+1", "..01", ".. 1", ".. 0")
safe_names <- c("v", ".rows", ".data", ".env", ".N", ".SD", "T", "F")
count <- c(matches = 0L, empty_payload_errors = 0L)
for (name in c(dot_names, safe_names)) {
  for (size in c(0L, 1L)) for (route in c("tibble", "frame", "dtplyr")) {
    for (id in list(NULL, "set")) for (sort in c("none", "last", "first")) {
      data <- stats::setNames(tibble::tibble(v = seq_len(size)), name)
      native <- dplyr::nest_by(data)
      stopifnot(nrow(native) == 1L, identical(as.list(native$data[[1L]]), as.list(data)))
      input <- switch(route, tibble = data, frame = as.data.frame(data), dtplyr = dtplyr::lazy_dt(data))
      result <- tryCatch(nest_by_with_margins(input, .id = id, .key = "cell", .sort = sort),
        error = identity)
      if (name %in% dot_names && size == 0L) {
        stopifnot(inherits(result, "error"),
          grepl("indexing '...' with non-positive index|used in an incorrect context",
            conditionMessage(result)))
        count[["empty_payload_errors"]] <- count[["empty_payload_errors"]] + 1L
      } else {
        stopifnot(nrow(result) == 1L, identical(names(result), c(id, "cell")),
          identical(dplyr::group_vars(result), if (is.null(id)) character() else id),
          identical(as.list(result$cell[[1L]]), as.list(data)))
        if (!is.null(id)) stopifnot(identical(result[[id]], 1L))
        count[["matches"]] <- count[["matches"]] + 1L
      }
    }
  }
}
print(count)
