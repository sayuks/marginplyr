# Reproduce finite nonmissing remaps through the same factor encoding boundary.
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(".", quiet = TRUE)
output_dir <- Sys.getenv("HUNT_OUTPUT_DIR", tempdir())
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
make_method <- function(mapping) {
  force(mapping)
  function(x, ...) {
    value <- as.character.factor(x)
    present <- !is.na(value)
    value[present] <- unname(mapping[value[present]])
    value
  }
}
mappings <- list(swap = c(a = "b", b = "a"),
                 collapse = c(a = "a", b = "a"),
                 margin = c(a = "Total", b = "Total"))
rows <- list(); i <- 0L
for (mode in names(mappings)) {
  class_name <- paste0("hunt_remap_", mode)
  registerS3method("as.character", class_name, make_method(mappings[[mode]]),
                  envir = asNamespace("base"))
  registerS3method("vec_ptype2", paste(class_name, class_name, sep = "."),
                  function(x, y, ...) vctrs::vec_slice(x, integer()),
                  envir = asNamespace("vctrs"))
  registerS3method("vec_cast", paste(class_name, class_name, sep = "."),
                  function(x, to, ...) x, envir = asNamespace("vctrs"))
  g <- factor(c("a", "b"))
  class(g) <- c(class_name, "factor")
  input <- tibble::tibble(g = g, row = 1:2)
  stopifnot(identical(as.integer(tidyr::nest(input, .by = g)$g), 1:2))
  stopifnot(identical(as.integer(dplyr::union_all(input, input)$g), rep(1:2, 2)))
  for (backend in c("local", "dtplyr")) for (checked in c(FALSE, TRUE))
  for (verb_name in c("nest", "nest_by")) {
    source <- if (backend == "local") input else dtplyr::lazy_dt(input)
    verb <- if (verb_name == "nest") nest_with_margins else nest_by_with_margins
    result <- dplyr::collect(verb(source, .grouping = rollup(g), .id = "set",
                                 .keep = TRUE, .check_margin_label = checked))
    detail <- result[result$set == 1L, ]
    observed <- as.character.factor(detail$g)
    expected <- vapply(detail$data, function(cell) {
      as.character.factor(input$g)[cell$row[[1L]]]
    }, character(1))
    stopifnot(identical(sort(vapply(detail$data, function(cell) cell$row,
                                   integer(1))), 1:2))
    i <- i + 1L
    rows[[i]] <- data.frame(mode, backend, checked, verb = verb_name,
                            observed = paste(observed, collapse = "/"),
                            expected = paste(expected, collapse = "/"),
                            matched = identical(observed, expected))
  }
}
result <- do.call(rbind, rows)
print(result)
write.csv(result, file.path(output_dir, "factor-remapping.csv"), row.names = FALSE)
cat("Native local nesting/union controls passed; all calls completed.\n")
if ("--assert" %in% commandArgs(trailingOnly = TRUE) && !all(result$matched)) {
  stop("Desired behavior failed: factor displays silently remapped source keys.")
}
