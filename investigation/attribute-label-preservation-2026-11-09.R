# Reproduce the dated attribute audit; dependencies are research-only.
# Run from the repository root, with labelled available in R_LIBS_USER.
args <- commandArgs(trailingOnly = TRUE)
output <- if (length(args)) args[[1L]] else tempfile("margin-attributes-")
dir.create(output, recursive = TRUE, showWarnings = FALSE)
pkgload::load_all(".", quiet = TRUE)
stopifnot(requireNamespace("labelled", quietly = TRUE))

fixture <- function(kind, shape, empty = FALSE) {
  key <- c(1, 1, 2, 2)
  value <- c(10, 20, 30, 40)
  if (kind == "haven") {
    key <- haven::labelled(key, c(One = 1, Two = 2))
    value <- haven::labelled(value, c(Low = 10, High = 40))
  } else if (kind == "spss") {
    key <- haven::labelled_spss(
      key, c(One = 1, Two = 2, Missing = 99), na_values = 99
    )
    value <- haven::labelled_spss(
      value, c(Low = 10, High = 40, Missing = 99), na_range = c(98, 99)
    )
  } else if (kind == "factor") {
    key <- ordered(key, levels = 1:3)
    value <- factor(value, levels = c(10, 20, 30, 40, 50))
  } else if (kind == "date") {
    key <- as.Date("2026-01-01") + key
    value <- as.Date("2026-02-01") + value
  }
  fixed <- key[rep(1L, 4L)]
  d <- data.frame(f = fixed, g = key, x = value)
  if (shape == "tibble") d <- tibble::as_tibble(d)
  if (shape == "data.table") d <- data.table::as.data.table(d)
  if (shape == "subclass") class(d) <- c("attribute_audit_df", class(d))
  for (nm in names(d)) {
    labelled::var_label(d[[nm]]) <- paste("Variable", nm)
    attr(d[[nm]], "audit_note") <- paste("Column", nm)
  }
  if (empty) d <- d[0, , drop = FALSE]
  attr(d, "label") <- "Source dataset"
  attr(d, "audit_note") <- "Source rows 1:4"
  d
}

records <- list()
record <- function(x, input, context, location) {
  columns <- if (is.data.frame(x)) names(x) else character()
  objects <- c(list(.table = x), lapply(columns, function(nm) x[[nm]]))
  names(objects) <- c(".table", columns)
  for (nm in names(objects)) {
    object <- objects[[nm]]
    original <- if (nm == ".table") input else input[[nm]]
    if (nm == "first_value") original <- input$x
    fields <- c("label", "audit_note", "labels", "na_values", "na_range")
    row <- c(context, list(location = location, column = nm,
                          class = paste(class(object), collapse = "/")))
    for (field in fields) {
      expected <- attr(original, field, exact = TRUE)
      actual <- attr(object, field, exact = TRUE)
      row[[field]] <- if (is.null(expected)) {
        if (is.null(actual)) "absent" else "new"
      } else if (identical(expected, actual)) {
        "kept"
      } else if (is.null(actual)) {
        "lost"
      } else {
        "changed"
      }
    }
    records[[length(records) + 1L]] <<- as.data.frame(row)
  }
}

run_verb <- function(input, verb, plan, margin_label, keep = FALSE) {
  if (verb == "summarize") {
    if (!is.data.frame(input) && !inherits(input, "dtplyr_step")) {
      return(summarize_with_margins(
        input, total = sum(as.numeric(x)), rows = dplyr::n(),
        .by = f, .grouping = plan, .margin_label = margin_label, .id = "set"
      ))
    }
    summarize_with_margins(
      input, first_value = dplyr::first(x),
      total = sum(as.numeric(x)), rows = dplyr::n(),
      .by = f, .grouping = plan, .margin_label = margin_label, .id = "set"
    )
  } else if (verb == "expand") {
    expand_with_margins(
      input, .by = f, .grouping = plan,
      .margin_label = margin_label, .id = "set"
    )
  } else {
    fun <- if (verb == "nest") nest_with_margins else nest_by_with_margins
    fun(input, .by = f, .grouping = plan, .margin_label = margin_label,
        .id = "set", .keep = keep)
  }
}

errors <- list()
for (kind in c("bare", "haven", "spss", "factor", "date")) {
  for (shape in c("data.frame", "tibble", "subclass", "data.table")) {
    for (empty in c(FALSE, TRUE)) {
      input <- fixture(kind, shape, empty)
      for (sets in c("one", "rollup")) {
        plan <- if (sets == "one") grouping_set(g) else rollup(g)
        for (display in c("Total", "NULL")) {
          margin_label <- if (display == "NULL") NULL else "Total"
          for (verb in c("summarize", "expand", "nest", "nest_by")) {
            for (keep in if (verb %in% c("nest", "nest_by")) c(FALSE, TRUE) else FALSE) {
              context <- list(kind = kind, shape = shape, empty = empty,
                              sets = sets, display = display, verb = verb,
                              keep = keep, backend = "local")
              result <- tryCatch(
                run_verb(input, verb, plan, margin_label, keep),
                error = identity
              )
              if (inherits(result, "error")) {
                errors[[length(errors) + 1L]] <- as.data.frame(c(
                  context, list(message = conditionMessage(result))
                ))
                next
              }
              record(result, input, context, "outer")
              if ("data" %in% names(result)) {
                for (i in seq_along(result$data)) {
                  record(result$data[[i]], input, context, paste0("cell", i))
                }
              }
            }
          }
        }
      }
    }
  }
}

# Backend probes distinguish conversion losses from Margin-operation losses.
for (kind in c("bare", "haven", "spss", "factor")) {
  input <- fixture(kind, "tibble")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  duck_con <- DBI::dbConnect(
    duckdb::duckdb(shared_home = FALSE), dbdir = ":memory:"
  )
  sources <- list(
    dtplyr = dtplyr::lazy_dt(input),
    arrow = arrow::Table$create(input),
    sqlite = dplyr::copy_to(con, input, "source", temporary = TRUE),
    duckdb = dplyr::copy_to(duck_con, input, "source", temporary = TRUE)
  )
  for (backend in names(sources)) {
    source <- sources[[backend]]
    roundtrip <- dplyr::collect(source)
    context <- list(kind = kind, shape = "tibble", empty = FALSE,
                    sets = "none", display = "none", verb = "roundtrip",
                    keep = FALSE, backend = backend)
    record(roundtrip, input, context, "outer")
    for (display in c("Total", "NULL")) {
      for (verb in c("summarize", "expand", "nest", "nest_by")) {
        if (backend != "dtplyr" && verb %in% c("nest", "nest_by")) next
        margin_label <- if (display == "NULL") NULL else "Total"
        context$sets <- "rollup"
        context$display <- display
        context$verb <- verb
        context$keep <- verb %in% c("nest", "nest_by")
        result <- tryCatch({
          value <- run_verb(source, verb, rollup(g), margin_label, TRUE)
          if (inherits(value, "rowwise_df")) value else dplyr::collect(value)
        }, error = identity)
        if (inherits(result, "error")) {
          errors[[length(errors) + 1L]] <- as.data.frame(c(
            context, list(message = conditionMessage(result))
          ))
          next
        }
        record(result, input, context, "outer")
        if ("data" %in% names(result)) {
          for (i in seq_along(result$data)) {
            record(result$data[[i]], input, context, paste0("cell", i))
          }
        }
      }
    }
  }
  DBI::dbDisconnect(con)
  DBI::dbDisconnect(duck_con, shutdown = TRUE)
}

# Native arithmetic is distinct from selecting an existing labelled scalar.
arithmetic <- list()
for (kind in c("bare", "haven", "spss")) {
  input <- fixture(kind, "tibble")
  value <- summarize_with_margins(
    input, sum_value = sum(x), mean_value = mean(x),
    .grouping = rollup(g), .margin_label = NULL
  )
  for (nm in c("sum_value", "mean_value")) {
    arithmetic[[length(arithmetic) + 1L]] <- data.frame(
      kind = kind, summary = nm, class = paste(class(value[[nm]]), collapse = "/"),
      label = !is.null(attr(value[[nm]], "label", exact = TRUE)),
      labels = !is.null(attr(value[[nm]], "labels", exact = TRUE))
    )
  }
}
write.csv(do.call(rbind, arithmetic), file.path(output, "arithmetic.csv"),
          row.names = FALSE)

# Inspection constructs a new plan table; contextual helpers construct values.
input <- fixture("haven", "tibble")
inspection <- inspect_grouping(input, .by = f, .grouping = rollup(g))
helpers <- summarize_with_margins(
  input, amount = sum(x), bit = grouping_bit(g), mask = grouping_id(g),
  parent = share_of_parent(amount), total = share_of_total(amount),
  .by = f, .grouping = rollup(g), .margin_label = NULL
)
saveRDS(list(inspection = inspection, helpers = helpers),
        file.path(output, "inspection-helpers.rds"))

upstream <- list()
for (kind in c("haven", "spss")) {
  source <- arrow::Table$create(fixture(kind, "tibble"))
  value <- tryCatch(
    dplyr::collect(dplyr::summarise(source, rows = dplyr::n(), .by = f)),
    error = identity
  )
  upstream[[length(upstream) + 1L]] <- data.frame(
    kind = kind, error = inherits(value, "error"),
    message = if (inherits(value, "error")) conditionMessage(value) else ""
  )
}
write.csv(do.call(rbind, upstream), file.path(output, "arrow-upstream.csv"),
          row.names = FALSE)

# The missing tag is part of the values, unlike the SPSS missing dictionary.
tagged <- data.frame(g = c("a", "b"))
tagged$x <- haven::labelled(c(10, haven::tagged_na("a")), label = "Measure")
tagged_expanded <- expand_with_margins(tagged, .grouping = rollup(g))
stopifnot(
  nrow(tagged_expanded) == 4L,
  sum(haven::is_tagged_na(tagged_expanded$x, "a")) == 2L
)
saveRDS(tagged_expanded, file.path(output, "tagged-expanded.rds"))
results <- do.call(rbind, records)
write.csv(results, file.path(output, "attributes.csv"), row.names = FALSE)
write.csv(if (length(errors)) do.call(rbind, errors) else data.frame(),
          file.path(output, "errors.csv"), row.names = FALSE)
capture.output(sessionInfo(), file = file.path(output, "session-info.txt"))
cat("Results:", output, "\n")
cat("Attribute observations:", nrow(results), "\n")
cat("Errors:", length(errors), "\n")
