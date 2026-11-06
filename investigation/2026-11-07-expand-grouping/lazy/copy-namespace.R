args <- commandArgs(trailingOnly = TRUE)
source_arg <- grep("^--source=", args, value = TRUE)
source <- if (length(source_arg)) sub("^--source=", "", source_arg[[1L]]) else "."
pkgload::load_all(source, quiet = TRUE)
Sys.setenv(TESTTHAT_PARALLEL = "false")
script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE))
out <- dirname(normalizePath(script))
rows <- list()

capture <- function(expr) {
  tryCatch(list(value = force(expr), error = ""),
           error = function(e) list(value = NULL, error = conditionMessage(e)))
}
canonical <- function(x) {
  x <- as.data.frame(x)
  if (nrow(x)) x <- x[order(x$sid, x$rid), , drop = FALSE]
  rownames(x) <- NULL
  x
}

for (shape in c("root", "filter", "copy_cache", "renamed_cache")) {
  for (family in c("one", "rollup", "repeated")) {
    for (sort in c("none", "first", "last")) {
      for (timing in c("early", "late")) {
        for (mode in c("count", "empty", "error")) {
          root <- new.env(parent = globalenv())
          root$source <- data.table::data.table(g = "a", rid = 1L)
          before <- serialize(root$source, NULL)
          input <- evalq(dtplyr::lazy_dt(source), root)
          if (shape == "filter") input <- dplyr::filter(input, TRUE)
          if (shape == "copy_cache") input <- dplyr::compute(input, name = "copy")
          if (shape == "renamed_cache") {
            input <- dplyr::rename(dplyr::compute(input, name = "cache"), key = g)
          }
          key <- if (shape == "renamed_cache") "key" else "g"
          plan <- switch(family,
            one = grouping_set(!!rlang::sym(key)),
            rollup = rollup(!!rlang::sym(key)),
            repeated = grouping_sets(grouping_set(!!rlang::sym(key)),
                                     grouping_set(!!rlang::sym(key)))
          )
          original <- as.data.frame(input)
          expected <- original
          expected$sid <- 1L
          if (family != "one") {
            second <- original
            if (family == "rollup") second[[key]] <- NA_character_
            second$sid <- 2L
            expected <- rbind(expected, second)
          }
          expected <- expected[, c(key, "sid", "rid"), drop = FALSE]
          callbacks <- 0L
          root$mode <- mode
          dispatcher <- function(pkg, name) {
            p <- as.character(substitute(pkg)); n <- as.character(substitute(name))
            if (identical(c(p, n), c("data.table", "copy"))) {
              callbacks <<- callbacks + 1L
              if (mode == "error") stop("COPY_NAMESPACE_INTERCEPT")
              if (mode == "empty") return(function(x) utils::head(x, 0L))
            }
            getExportedValue(p, n)
          }
          if (timing == "early") root$`::` <- dispatcher
          query <- capture(expand_with_margins(
            input, .grouping = plan, .margin_label = NULL, .id = "sid",
            .duplicates = "keep", .sort = sort
          ))
          construction_callbacks <- callbacks
          if (timing == "late") root$`::` <- dispatcher
          callbacks <- 0L
          ordinary <- capture(as.data.frame(input))
          ordinary_callbacks <- callbacks
          callbacks <- 0L
          mutated <- capture(as.data.frame(dplyr::mutate(input, !!key := !!rlang::sym(key))))
          mutate_callbacks <- callbacks
          callbacks <- 0L
          unioned <- capture(as.data.frame(dplyr::union_all(input, input)))
          union_callbacks <- callbacks
          callbacks <- 0L
          actual <- if (nzchar(query$error)) query else capture(dplyr::collect(query$value))
          expansion_callbacks <- callbacks
          rm("::", envir = root)
          restored <- if (nzchar(query$error)) query else capture(dplyr::collect(query$value))
          row <- data.frame(shape, family, sort, timing, mode,
            construction_callbacks, ordinary_callbacks, mutate_callbacks, union_callbacks,
            expansion_callbacks, actual_rows = if (is.null(actual$value)) NA_integer_ else nrow(actual$value),
            expected_rows = nrow(expected), error = actual$error,
            desired_values = !nzchar(actual$error) && identical(canonical(actual$value), canonical(expected)),
            ordinary_ok = !nzchar(ordinary$error) && identical(ordinary$value, original),
            mutate_ok = !nzchar(mutated$error) && identical(mutated$value, original),
            union_ok = !nzchar(unioned$error) && identical(unioned$value, rbind(original, original)),
            restored_ok = !nzchar(restored$error) && identical(canonical(restored$value), canonical(expected)),
            source_unchanged = identical(before, serialize(root$source, NULL))
          )
          rows[[length(rows) + 1L]] <- row
        }
      }
    }
  }
}
result <- do.call(rbind, rows)
utils::write.csv(result, file.path(out, "copy-namespace.csv"), row.names = FALSE)
print(with(result, table(mode, desired_values, useNA = "ifany")))
print(colSums(result[, c("construction_callbacks", "ordinary_callbacks", "mutate_callbacks", "union_callbacks", "expansion_callbacks")]))
stopifnot(all(result$construction_callbacks == 0L), all(result$ordinary_callbacks == 0L),
          all(result$mutate_callbacks == 0L), all(result$union_callbacks == 0L),
          all(result$ordinary_ok), all(result$mutate_ok), all(result$union_ok),
          all(result$restored_ok), all(result$source_unchanged),
          all(result$expansion_callbacks > 0L))
cat(nrow(result), "namespace-copy observations; ordinary, restored and immutability controls passed\n")
writeLines(capture.output(sessionInfo()), file.path(out, "environment.txt"))
if ("--assert" %in% args) {
  stopifnot(all(result$desired_values), all(result$expansion_callbacks == 0L))
}
