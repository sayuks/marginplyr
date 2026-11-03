# Dated public-seam reproduction; --assert requires correct Margin order.
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(Sys.getenv("MARGINPLYR_HUNT_SOURCE", getwd()), quiet = TRUE)
args <- commandArgs(trailingOnly = TRUE)
helper <- if ("%in%" %in% args) "%in%" else "is.na"
assert_correct <- "--assert" %in% args
cat("Baseline: 4b34fdecb2d8a41034b3d9b5d5af8e51592555e1\n")
root <- new.env(parent = globalenv())
root$source <- data.table::data.table(g = c(NA_character_, "a"), row_id = 1:2)
before <- serialize(root$source, NULL)
source <- evalq(dtplyr::lazy_dt(source), root)
spec <- if (helper == "is.na") grouping_set(g) else rollup(g)
query <- nest_with_margins(source, .grouping = spec, .id = "set",
                           .sort = "last", .keep = TRUE)
expected <- dplyr::collect(query)
cat("Unmasked lazy query:\n"); dplyr::show_query(query)
reads <- 0L
root[[helper]] <- if (helper == "is.na") {
  function(x) { reads <<- reads + 1L; !base::is.na(x) }
} else {
  function(x, table) { reads <<- reads + 1L; !base::`%in%`(x, table) }
}
ordinary <- dplyr::collect(dplyr::arrange(source, g))
counted <- dplyr::collect(dplyr::arrange(
  dplyr::summarize(source, n = dplyr::n(), .by = g), g
))
upstream <- dplyr::collect(dplyr::arrange(
  tidyr::nest(dplyr::group_by(source, g), data = row_id), g
))
stopifnot(identical(ordinary$g, c("a", NA_character_)),
          identical(counted$g, ordinary$g), identical(upstream$g, ordinary$g),
          reads == 0L)
actual <- dplyr::collect(query)
cat("Masked helper:", helper, "reads:", reads, "\n")
print(actual)
stopifnot(identical(serialize(root$source, NULL), before))
# Every source group retains its original rows even when its position changes.
cell_key <- function(out) {
  result <- lapply(seq_len(nrow(out)), function(i) {
    list(key = out$g[[i]], set = out$set[[i]], rows = out$data[[i]]$row_id)
  })
  sort(vapply(result, function(x) paste(as.character(serialize(x, NULL)), collapse = ""), ""))
}
stopifnot(identical(cell_key(actual), cell_key(expected)))
root[[helper]] <- get(helper, envir = baseenv())
restored <- dplyr::collect(query)
stopifnot(identical(restored$g, expected$g), identical(restored$set, expected$set))
cat("Restoring only the helper binding recovers the same query's order.\n")
# Explicit user expressions retain ordinary lexical lookup.
user <- dplyr::mutate(source, flag = !!rlang::new_quosure(
  if (helper == "is.na") quote(is.na(g)) else quote(g %in% "a"), root
))
root[[helper]] <- if (helper == "is.na") function(x) !base::is.na(x) else function(x, table) !base::`%in%`(x, table)
user <- dplyr::collect(user)
stopifnot(identical(user$flag, if (helper == "is.na") !base::is.na(root$source$g) else !base::`%in%`(root$source$g, "a")))
if (assert_correct) {
  stopifnot(identical(actual$g, expected$g), identical(actual$set, expected$set))
}
