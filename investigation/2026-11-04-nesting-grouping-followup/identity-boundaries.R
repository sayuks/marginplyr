pkgload::load_all(quiet = TRUE)
ns <- c(0L, 1L, 2L, 7L, 15L, 29L, 30L, 31L, 32L, 33L, 40L)
consumers <- list(
  local = function(d) d,
  grouped = function(d) dplyr::group_by(d, fixed),
  remote = function(d) dbplyr::tbl_lazy(d, con = dbplyr::simulate_postgres())
)
checks <- 0L
for (n in ns) {
  keys <- if (n) paste0("dimension, `", rev(seq_len(n))) else character()
  d <- tibble::new_tibble(
    stats::setNames(rep(list(1L), n + 1L), c("fixed", keys)),
    nrow = 1L
  )
  full <- grouping_set(dplyr::all_of(keys))
  empty <- grouping_set()
  spec <- grouping_sets(full, empty, full)
  for (source in consumers) {
    for (format in c("list", "text")) {
      for (policy in c("error", "drop", "keep")) {
        result <- tryCatch(
          if (inherits(source(d), "grouped_df")) {
            inspect_grouping(source(d), .grouping = spec, .duplicates = policy, .format = format)
          } else {
            inspect_grouping(source(d), .by = fixed, .grouping = spec, .duplicates = policy, .format = format)
          },
          error = identity
        )
        if (policy == "error") {
          stopifnot(inherits(result, "marginplyr_error"),
                    grepl("Duplicate grouping sets", conditionMessage(result), fixed = TRUE))
          checks <- checks + 1L
          next
        }
        incl <- list(keys, character(), keys)
        omit <- list(character(), keys, character())
        bits <- list(
          stats::setNames(integer(n), keys),
          stats::setNames(rep(1L, n), keys),
          stats::setNames(integer(n), keys)
        )
        if (!n) bits <- rep(list(integer()), 3)
        ids <- if (n > 31L) rep(NA_integer_, 3) else c(0L, as.integer(2^n-1), 0L)
        take <- if (policy == "keep") 1:3 else if (n) 1:2 else 1L
        if (format == "text") {
          fmt <- function(cols) paste0("(", paste(cols, collapse=", "), ")")
          incl <- vapply(incl, fmt, "")
          omit <- vapply(omit, fmt, "")
          bits <- vapply(bits, function(b) if (!length(b)) "()" else paste(names(b), b, sep="=", collapse=", "), "")
        }
        expected <- tibble::tibble(
          set_id = seq_along(take),
          fixed = if (format == "list") rep(list("fixed"), length(take)) else rep("(fixed)", length(take)),
          included = incl[take], omitted = omit[take], grouping_bits = bits[take], grouping_id = ids[take]
        )
        stopifnot(identical(result, expected))
        checks <- checks + 1L
      }
    }
  }
}
cat(checks, "exact plan/diagnostic controls\n")
