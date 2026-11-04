Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(Sys.getenv("MARGINPLYR_HUNT_SOURCE", getwd()), quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
as.character.independent_factor <- function(x, ...) {
  result <- as.character.factor(x)
  present <- !is.na(result)
  result[present] <- paste0("[", result[present], "]")
  result
}
vctrs::s3_register("vctrs::vec_ptype2", "independent_factor.independent_factor",
                  function(x, y, ...) vctrs::vec_slice(x, integer()))
vctrs::s3_register("vctrs::vec_cast", "independent_factor.independent_factor",
                  function(x, to, ...) x)
g <- factor("a")
class(g) <- c("independent_factor", class(g))
input <- tibble(g, row = 1L)
codes <- function(x) as.integer(unclass(x))
stopifnot(identical(as.character(vctrs::vec_slice(g, integer())), character()),
          identical(levels(g), "a"), identical(codes(g), 1L))
records <- list()
for (backend in c("local", "dtplyr")) {
  source <- if (backend == "local") input else dtplyr::lazy_dt(input)
  snapshot <- serialize(if (backend == "local") source else source[["parent"]], NULL)
  native <- if (backend == "local") source else collect(source)
  stopifnot(identical(codes(native$g), 1L))
  grouped <- summarize(group_by(source, g), count = n(), .groups = "drop")
  upstream <- if (backend == "local") tidyr::nest(source, .by = g) else
    tidyr::nest(group_by(source, g), data = row)
  if (backend == "dtplyr") { grouped <- collect(grouped); upstream <- collect(upstream) }
  stopifnot(identical(codes(grouped$g), 1L), identical(codes(upstream$g), 1L))
  for (verb in c("nest_with_margins", "nest_by_with_margins", "expand_with_margins", "summarize_with_margins")) {
    run <- function(label) {
      ans <- if (verb == "summarize_with_margins") {
        summarize_with_margins(source, .grouping = grouping_set(g),
                               .margin_label = label, count = n())
      } else get(verb)(source, .grouping = grouping_set(g), .margin_label = label)
      if (inherits(ans, "dtplyr_step")) ans <- as_tibble(ans)
      ans
    }
    actual <- run("Total")
    typed <- run(NULL)
    stopifnot(identical(codes(typed$g), 1L),
              identical(snapshot, serialize(if (backend == "local") source else source[["parent"]], NULL)))
    records[[length(records) + 1L]] <- data.frame(backend, verb,
      native_code = 1L, actual_code = codes(actual$g), typed_missing_code = codes(typed$g),
      actual_levels = paste(levels(actual$g), collapse = ","))
  }
}
result <- do.call(rbind, records)
print(result, row.names = FALSE)
cat("Source values and native grouped/nesting controls passed on both backends.\n")
if ("--assert" %in% commandArgs(trailingOnly = TRUE)) {
  stopifnot(all(result$actual_code == result$native_code))
}
