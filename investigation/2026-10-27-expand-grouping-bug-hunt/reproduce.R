# Dated investigation replay; run from the repository root.
# Failures are deliberate contract assertions, not installed package tests.
pkgload::load_all(".", quiet = TRUE)

cat("R: ", as.character(getRversion()), "\n", sep = "")
for (package in c("marginplyr", "dplyr", "dbplyr", "rlang", "vctrs",
                  "tibble", "data.table", "DBI", "RSQLite")) {
  cat(package, ": ", as.character(utils::packageVersion(package)), "\n", sep = "")
}

verdicts <- logical()
record <- function(label, actual, accepts_error = function(error) FALSE) {
  value <- tryCatch(actual(), error = identity)
  ok <- if (inherits(value, "error")) accepts_error(value) else isTRUE(value)
  verdicts[[label]] <<- ok
  cat(if (ok) "PASS " else "FAIL ", label, "\n", sep = "")
  if (inherits(value, "error") && !ok) {
    cat("  ", conditionMessage(value), "\n", sep = "")
  }
  invisible(ok)
}

same_columns <- function(x, y) {
  identical(names(x), names(y)) &&
    identical(unname(as.list(x)), unname(as.list(y)))
}

sqlite_replay <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  cat("\nF1: Boolean-word source name beside .env\n")
  for (word in c("TRUE", "FALSE", "TrUe", "FaLsE")) {
    data <- tibble::as_tibble(stats::setNames(list("a", 1L), c(word, ".env")))
    source <- dplyr::copy_to(con, data, paste0("source_", length(verdicts)))
    record(paste("F1 source collection", word), function() {
      same_columns(dplyr::collect(source), data)
    })
    record(paste("F1 inspection", word), function() {
      plan <- inspect_grouping(source, .format = "list")
      identical(plan$set_id, 1L) && identical(plan$included, list(character()))
    })
    record(paste("F1 expansion identity", word), function() {
      same_columns(dplyr::collect(expand_with_margins(source)), data)
    })
  }
  record("F1 limited summary", function() {
    result <- summarize_with_margins(utils::head(source, 1L), n = dplyr::n())
    identical(dplyr::collect(result)$n, 1L)
  })
  for (names in list(c("g", ".env"), c("TRUE", ".data"))) {
    data <- tibble::as_tibble(stats::setNames(list("a", 1L), names))
    source <- dplyr::copy_to(con, data, paste0("control_", length(verdicts)))
    record(paste("F1 name control", paste(names, collapse = "/")), function() {
      same_columns(dplyr::collect(expand_with_margins(source)), data)
    })
  }
}

raw_verb <- function(verb, data, specification, label, check = TRUE,
                     sort = "none", by = NULL) {
  arguments <- list(
    .data = data, .by = by, .grouping = specification,
    .margin_label = label, .check_margin_label = check,
    .sort = sort, .id = "sid"
  )
  if (identical(verb, "summarize_with_margins")) {
    arguments <- c(arguments, list(total = rlang::expr(sum(p))))
  }
  rlang::inject((get(verb, envir = asNamespace("marginplyr")))(!!!arguments))
}

raw_replay <- function() {
  cat("\nF2: raw-containing omitted dimensions\n")
  fixtures <- list(
    raw = tibble::tibble(g = as.raw(c(0L, 1L)), p = 1:2),
    matrix = tibble::tibble(g = matrix(as.raw(0:3), 2L), p = 1:2),
    array = tibble::tibble(g = array(as.raw(0:7), c(2L, 2L, 2L)), p = 1:2),
    packed = tibble::tibble(
      g = tibble::tibble(raw = as.raw(c(0L, 1L)), number = 1:2), p = 1:2
    ),
    data_table = data.table::data.table(g = as.raw(c(0L, 1L)), p = 1:2)
  )
  verbs <- c("expand_with_margins", "summarize_with_margins",
             "nest_with_margins", "nest_by_with_margins")
  safe_refusal <- function(error) {
    inherits(error, "marginplyr_error") &&
      grepl("raw", conditionMessage(error), ignore.case = TRUE) &&
      grepl("missing|label", conditionMessage(error), ignore.case = TRUE)
  }
  raw_passed <- 0L
  raw_failed <- 0L
  for (fixture in names(fixtures)) {
    for (verb in verbs) {
      passed <- 0L
      failed <- 0L
      for (label in list(NULL, NA_character_)) {
        for (check in c(TRUE, FALSE)) {
          for (sort in c("none", "first", "last")) {
            result <- tryCatch(
              raw_verb(verb, fixtures[[fixture]], rollup(g), label, check, sort),
              error = identity
            )
            ok <- if (inherits(result, "error")) {
              safe_refusal(result)
            } else {
              positions <- which(result$sid == 2L)
              expected <- if (identical(verb, "expand_with_margins")) 2L else 1L
              omitted <- vctrs::vec_slice(result[["g"]], positions)
              length(positions) == expected &&
                all(vctrs::vec_detect_missing(omitted))
            }
            if (ok) passed <- passed + 1L else failed <- failed + 1L
          }
        }
      }
      name <- paste("F2", fixture, verb)
      verdicts[[name]] <<- failed == 0L
      cat(name, ": ", passed, " passed, ", failed, " failed\n", sep = "")
      raw_passed <- raw_passed + passed
      raw_failed <- raw_failed + failed
    }
  }
  cat("F2 matrix total: ", raw_passed, " passed, ", raw_failed, " failed\n",
      sep = "")
  for (verb in verbs) {
    for (control in c("fixed", "payload", "included", "string")) {
      record(paste("F2 unaffected control", verb, control), function() {
        result <- switch(control,
          fixed = raw_verb(verb, fixtures$raw, NULL, NULL, by = rlang::sym("g")),
          payload = raw_verb(verb, fixtures$raw, rollup(p), "All"),
          included = raw_verb(verb, fixtures$raw, grouping_set(g), NULL),
          string = raw_verb(verb, fixtures$raw, rollup(g), "All")
        )
        nrow(result) > 0L
      })
    }
  }
  cat("\nF2 minimal actual result:\n")
  minimal <- tryCatch(
    expand_with_margins(fixtures$raw, .grouping = rollup(g),
                        .margin_label = NULL, .id = "sid"),
    error = identity
  )
  if (inherits(minimal, "error")) {
    cat(conditionMessage(minimal), "\n", sep = "")
  } else {
    print(minimal)
  }
}

sqlite_replay()
raw_replay()
cat("\nAssertion groups: ", sum(verdicts), " passed, ", sum(!verdicts),
    " failed\n", sep = "")
if (!all(verdicts)) {
  stop("Public-contract assertions failed; see the dated investigation.",
       call. = FALSE)
}
