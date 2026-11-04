# Reproduce a factor's display method changing the stored outer source value.
# Run from the checkout root. --assert requires the intended source values.
pkgload::load_all(".", quiet = TRUE)
Sys.setenv(TESTTHAT_PARALLEL = "false")
assert_mode <- "--assert" %in% commandArgs(trailingOnly = TRUE)

plain_factor <- function(x) {
  structure(unclass(x), levels = levels(x),
            class = if (is.ordered(x)) c("ordered", "factor") else "factor")
}
as.character.hunt_display_factor <- function(x, ...) {
  display <- as.character(plain_factor(x))
  display[!is.na(display)] <- paste0(display[!is.na(display)], "!")
  display
}
registerS3method("vec_ptype2", "hunt_display_factor.hunt_display_factor",
                 function(x, y, ...) vctrs::vec_slice(x, integer()),
                 envir = asNamespace("vctrs"))
registerS3method("vec_cast", "hunt_display_factor.hunt_display_factor",
                 function(x, to, ...) x, envir = asNamespace("vctrs"))
registerS3method("as.character", "hunt_display_factor",
                 as.character.hunt_display_factor, envir = asNamespace("base"))

g <- structure(1L, levels = "A", class = c("hunt_display_factor", "factor"))
input <- tibble::tibble(g = g, row_id = 1L)
source_before <- serialize(input, NULL)
native_union <- dplyr::union_all(input, input)
native_nest <- dplyr::nest_by(input, g, .keep = TRUE)
stopifnot(identical(as.integer(native_union$g), c(1L, 1L)),
          identical(as.integer(native_nest$g), 1L),
          identical(as.integer(native_nest$data[[1L]]$g), 1L))

observed <- list()
for (name in c("nest_with_margins", "nest_by_with_margins")) {
  verb <- get(name, asNamespace("marginplyr"))
  result <- verb(input, .grouping = grouping_set(g), .keep = TRUE)
  missing_label <- verb(input, .grouping = grouping_set(g),
                        .margin_label = NULL, .keep = TRUE)
  fixed <- verb(input, .by = g, .keep = TRUE)
  ordinary <- input
  ordinary$g <- plain_factor(ordinary$g)
  plain <- verb(ordinary, .grouping = grouping_set(g), .keep = TRUE)
  stopifnot(identical(as.integer(missing_label$g), 1L),
            identical(as.integer(fixed$g), 1L),
            identical(as.integer(plain$g), 1L),
            identical(as.integer(result$data[[1L]]$g), 1L),
            identical(result$data[[1L]]$row_id, 1L),
            identical(levels(result$g), c("A", "Total")))
  observed[[name]] <- as.integer(result$g)
  cat(name, "outer source code:", as.integer(result$g),
      "; source/cell code:", as.integer(input$g), "/",
      as.integer(result$data[[1L]]$g), "\n")
}
stopifnot(identical(serialize(input, NULL), source_before))
cat("Native union/nesting, plain-factor, NULL-label, fixed-key, cell and source controls passed.\n")
cat("R:", R.version.string, "; platform:", R.version$platform, "\n")
for (pkg in c("marginplyr", "dplyr", "vctrs", "tibble")) {
  cat(pkg, as.character(utils::packageVersion(pkg)), "\n")
}
if (assert_mode) {
  stopifnot("A detail factor must retain its original source value." =
              all(vapply(observed, identical, logical(1), 1L)))
} else {
  stopifnot(all(vapply(observed, identical, logical(1), NA_integer_)))
  cat("Confirmed dated defect; this is an observation, not a repaired regression pass.\n")
}
