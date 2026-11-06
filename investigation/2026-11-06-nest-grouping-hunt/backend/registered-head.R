# Dated probe. Run from the investigated checkout; --assert requires safety.
Sys.setenv(TESTTHAT_PARALLEL = "false")
args <- commandArgs(trailingOnly = TRUE)
source_dir <- Sys.getenv("MARGINPLYR_HUNT_SOURCE", unset = ".")
pkgload::load_all(source_dir, quiet = TRUE)
requireNamespace("data.table", quietly = TRUE)
original <- utils::getS3method("head", "data.table")
run_case <- function(mode = "retype", graph = "root", role = "dimension",
                     consumer = "inspect", immutable = TRUE) {
  source <- data.table::data.table(g = c("a", "b"), row_id = 1:2)
  before <- serialize(source, NULL)
  root <- dtplyr::lazy_dt(source, immutable = immutable)
  input <- switch(graph, root = root,
                  arithmetic = dplyr::mutate(root, derived = row_id + 1L),
                  filter = dplyr::filter(root, row_id > 0L))
  native <- dplyr::collect(input)
  stopifnot(identical(native$g, c("a", "b")))
  calls <- integer()
  method <- function(x, n = 6L, ...) {
    calls <<- c(calls, nrow(x))
    if (mode == "mutate" && nrow(x) > 0L) {
      data.table::set(x, j = "g", value = rep("changed", nrow(x)))
    }
    out <- original(x, n = n, ...)
    if (mode == "retype" && nrow(out) == 0L) {
      data.table::set(out, j = "g", value = integer())
    }
    out
  }
  registerS3method("head", "data.table", method, envir = asNamespace("utils"))
  on.exit(registerS3method("head", "data.table", original,
                          envir = asNamespace("utils")), add = TRUE)
  named <- inspect_grouping(input, .grouping = grouping_set(g))
  stopifnot(identical(named$included, "(g)"), length(calls) == 0L,
            identical(serialize(source, NULL), before))
  native_masked <- dplyr::collect(input)
  stopifnot(identical(native_masked$g, native$g), length(calls) == 0L)
  invoke <- function() {
    if (consumer == "inspect") {
      if (role == "dimension") {
        inspect_grouping(input, .grouping = grouping_set(where(is.character)))
      } else {
        inspect_grouping(input, .by = where(is.character))
      }
    } else {
      verb <- get(paste0(consumer, "_with_margins"))
      if (role == "dimension") {
        verb(input, .grouping = grouping_set(where(is.character)))
      } else {
        verb(input, .by = where(is.character))
      }
    }
  }
  result <- tryCatch(invoke(), error = identity)
  calls_before_collection <- calls
  if (inherits(result, "dtplyr_step")) result <- dplyr::collect(result)
  refusal <- inherits(result, "marginplyr_error")
  correct <- if (inherits(result, "error")) FALSE else if (consumer == "inspect") {
    identical(result[[if (role == "dimension") "included" else "fixed"]], "(g)")
  } else {
    "g" %in% names(result) && identical(sort(result$g), c("a", "b")) &&
      identical(sort(vapply(result$data, nrow, integer(1))), c(1L, 1L))
  }
  unchanged <- identical(serialize(source, NULL), before)
  registered_head_safe <- !any(calls_before_collection > 0L) && unchanged &&
    (correct || refusal)
  record <- data.frame(mode, graph, role, consumer, immutable, correct, refusal,
                       source_unchanged = unchanged,
                       preparation_row_counts = paste(calls_before_collection,
                                                       collapse = ","),
                       safe = registered_head_safe)
  registerS3method("head", "data.table", original, envir = asNamespace("utils"))
  # Restoring only the registration repairs the same input unless the mutation
  # witness deliberately changed the source; fresh retyping inputs need no repair.
  if (mode == "retype" && immutable) {
    restored <- invoke()
    if (inherits(restored, "dtplyr_step")) restored <- dplyr::collect(restored)
    stopifnot(if (consumer == "inspect") {
      identical(restored[[if (role == "dimension") "included" else "fixed"]],
                "(g)")
    } else {
      "g" %in% names(restored) && nrow(restored) == 2L
    })
  }
  record
}
records <- list()
for (graph in c("root", "arithmetic", "filter")) {
  for (role in c("dimension", "fixed")) {
    for (consumer in c("inspect", "nest", "nest_by")) {
      records[[length(records) + 1L]] <- run_case(graph = graph, role = role,
                                                consumer = consumer)
    }
  }
}
for (immutable in c(TRUE, FALSE)) {
  records[[length(records) + 1L]] <- run_case("mutate", immutable = immutable)
  if (!immutable) {
    for (consumer in c("nest", "nest_by")) {
      records[[length(records) + 1L]] <- run_case("mutate", consumer = consumer,
                                                immutable = FALSE)
    }
  }
}
out <- do.call(rbind, records)
print(out, row.names = FALSE)
if ("--assert" %in% args) {
  stopifnot(all(out$safe))
} else {
  stopifnot(sum(!out$safe) == 20L, sum(out$safe) == 2L,
            !any(out$correct[seq_len(18L)]),
            all(out$source_unchanged[seq_len(18L)]),
            !any(out$source_unchanged[19:20]),
            all(out$refusal[21:22]))
}
cat("R:", R.version.string, "\n")
for (p in c("dplyr", "dtplyr", "data.table", "tibble", "rlang", "vctrs")) {
  cat(p, as.character(utils::packageVersion(p)), "\n")
}
