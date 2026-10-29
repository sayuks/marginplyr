# Dated investigation replay; no package or test-suite implementation is changed.
# Run from the repository root. Report mode exits zero; --assert requires the
# desired public behavior and therefore exits nonzero at the investigated SHA.
pkgload::load_all(".", quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
args <- commandArgs(TRUE)
assert_mode <- "--assert" %in% args
output_dir <- "investigation/2026-10-29-summary-spec-share-exhaustive/backend"
conns <- list(
  sqlite = DBI::dbConnect(RSQLite::SQLite(), ":memory:"),
  duckdb = DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
)
invisible(DBI::dbExecute(conns$duckdb, "SET threads = 1"))
inputs <- lapply(conns, function(con) {
  list(
    populated = copy_to(con, tibble(g = c("a", "b"), v = c(1, 3)),
                        "signed_populated", overwrite = TRUE),
    empty = copy_to(con, tibble(g = character(), v = double()),
                    "signed_empty", overwrite = TRUE)
  )
})
plans <- list(none = NULL, single_empty = grouping_set(), rollup = rollup(g))
roles <- c("summary", "source", "share", "identifier")
spellings <- c("..+0", "..+1", ".. 1", "..-1", ".env")
engines <- c("sqlite", "duckdb_native", "duckdb_portable")
contents_to_check <- c("populated", "empty")
sorts <- c("none", "first", "last")
if (assert_mode) {
  # Keep the red-capable feedback loop small; report mode owns the full matrix.
  plans <- plans["rollup"]
  roles <- "summary"
  spellings <- "..+1"
  engines <- "sqlite"
  contents_to_check <- "populated"
  sorts <- "none"
}
records <- list()
controls <- list()
record <- function(engine, contents, plan, role, name, sort, stage, ok,
                   diagnostic = "") {
  records[[length(records) + 1L]] <<- data.frame(
    engine, contents, plan, role, name, sort, stage, ok,
    diagnostic = gsub("[\r\n]+", " | ", diagnostic), stringsAsFactors = FALSE
  )
}
canonical <- function(x) {
  x <- as.data.frame(x)
  x <- x[do.call(order, c(unname(x), list(na.last = TRUE))), , drop = FALSE]
  rownames(x) <- NULL
  x
}
equal_frame <- function(actual, expected) {
  isTRUE(all.equal(canonical(actual), canonical(expected),
                  check.attributes = FALSE))
}
ordinary <- function(input, plan) {
  if (plan == "rollup") {
    detail <- collect(summarise(group_by(input, g), amount = sum(v, na.rm = TRUE),
                                .groups = "drop"))
    detail$sid <- rep.int(1L, nrow(detail))
    total <- collect(summarise(input, amount = sum(v, na.rm = TRUE)))
    total$g <- "Total"
    total$sid <- 2L
    bind_rows(detail, total)[c("g", "sid", "amount")]
  } else {
    total <- collect(summarise(input, amount = sum(v, na.rm = TRUE)))
    total$sid <- 1L
    total[c("sid", "amount")]
  }
}
build <- function(input, plan, role, name, sort, portable) {
  amount <- if (role %in% c("summary", "source")) name else "amount"
  fraction <- if (role == "share") name else "fraction"
  dots <- setNames(list(rlang::quo(sum(v, na.rm = TRUE))), amount)
  if (role %in% c("source", "share")) {
    dots[[fraction]] <- rlang::new_quosure(
      rlang::call2("share_of_total", rlang::sym(amount)), globalenv()
    )
  }
  summarize_with_margins(
    input, !!!dots, .grouping = plans[[plan]], .sort = sort,
    .duplicates = if (portable) "keep" else "drop",
    .id = if (assert_mode) NULL else if (role == "identifier") name else "sid",
    .check_share_source = FALSE
  )
}
for (engine in engines) {
  backend <- if (engine == "sqlite") "sqlite" else "duckdb"
  portable <- engine == "duckdb_portable"
  for (contents in contents_to_check) {
    input <- inputs[[backend]][[contents]]
    for (plan in names(plans)) {
      expected_base <- ordinary(input, plan)
      cat("GRID", engine, contents, plan, "\n")
      flush.console()
      for (name in spellings) {
        dots <- setNames(list(rlang::quo(sum(v, na.rm = TRUE))), name)
        ordinary_named <- collect(summarise(group_by(input, g), !!!dots,
                                            .groups = "drop"))
        controls[[length(controls) + 1L]] <- data.frame(
          engine, contents, plan, name, control = "ordinary_grouped_output",
          ok = identical(names(ordinary_named), c("g", name)) &&
            if (contents == "empty") nrow(ordinary_named) == 0L else
              isTRUE(all.equal(ordinary_named[[name]], c(1, 3),
                              check.attributes = FALSE))
        )
        for (role in roles) {
          expected <- expected_base
          if (role %in% c("source", "share")) {
            denominator <- tail(expected$amount, 1L)
            expected$fraction <- ifelse(
              is.na(expected$amount) | is.na(denominator) | denominator == 0,
              NA_real_, expected$amount / denominator
            )
            expected$fraction[expected$sid == if (plan == "rollup") 2L else 1L] <- 1
          }
          if (role %in% c("summary", "source")) {
            names(expected)[names(expected) == "amount"] <- name
          }
          if (role == "share") names(expected)[names(expected) == "fraction"] <- name
          if (role == "identifier") names(expected)[names(expected) == "sid"] <- name
          if (assert_mode) expected$sid <- NULL
          for (sort in sorts) {
            stage <- "construct"
            got <- tryCatch({
              query <- build(input, plan, role, name, sort, portable)
              stage <- "render"
              sql <- dbplyr::sql_render(query)
              stage <- "collect"
              collected <- collect(query)
              if (!equal_frame(collected, expected)) {
                stop("Collected public names/values differed from the ordinary-grain oracle")
              }
              if (sort != "none" && plan == "rollup" && nrow(collected) > 1L) {
                wanted <- if (sort == "first") c("Total", "a", "b") else c("a", "b", "Total")
                if (!identical(collected$g, wanted)) stop("Requested Margin order differed")
              }
              stage <- "compute"
              materialized <- compute(query, name = "margin_signed_output", temporary = TRUE,
                                      overwrite = TRUE)
              materialized <- collect(materialized)
              if (!equal_frame(materialized, expected)) stop("Computed public names/values differed")
              stage <- "complete"
              TRUE
            }, error = identity)
            ok <- identical(got, TRUE)
            record(engine, contents, plan, role, name, sort, stage, ok,
                   if (inherits(got, "error")) conditionMessage(got) else "")
          }
        }
      }
    }
  }
}
# The caller's explicit lexical read must survive public-name protection.
for (engine in engines) {
  backend <- if (engine == "sqlite") "sqlite" else "duckdb"
  input <- inputs[[backend]]$populated
  scale <- 2
  got <- collect(summarize_with_margins(
    input, .env = sum(v, na.rm = TRUE), lexical = sum(v, na.rm = TRUE) * .env$scale,
    .grouping = rollup(g), .duplicates = if (engine == "duckdb_portable") "keep" else "drop",
    .sort = "last", .id = "sid"
  ))
  controls[[length(controls) + 1L]] <- data.frame(
    engine, contents = "populated", plan = "rollup", name = ".env",
    control = "caller_lexical_env", ok = identical(got$.env, c(1, 3, 4)) &&
      identical(got$lexical, c(2, 6, 8))
  )
}
results <- bind_rows(records)
control_results <- bind_rows(controls)
if (!assert_mode) {
  write.csv(results, file.path(output_dir, "sql-signed-output-results.csv"), row.names = FALSE)
  write.csv(control_results, file.path(output_dir, "sql-signed-output-controls.csv"), row.names = FALSE)
}
cat("BASELINE", system("git rev-parse HEAD", intern = TRUE), "\n")
for (package in c("marginplyr", "dplyr", "dbplyr", "rlang", "RSQLite", "duckdb")) {
  cat("VERSION", package, as.character(packageVersion(package)), "\n")
}
cat("CHECKS", nrow(results), "FAILURES", sum(!results$ok), "CONTROLS",
    nrow(control_results), "CONTROL_FAILURES", sum(!control_results$ok), "\n")
print(with(results, table(engine, ok)))
print(with(results[!results$ok, ], table(name, stage)))
print(with(results, table(name, ok)))
for (i in which(!results$ok)[seq_len(min(12L, sum(!results$ok)))]) {
  cat("FAIL", paste(results[i, c("engine", "contents", "plan", "role", "name", "sort", "stage")], collapse = "/"),
      results$diagnostic[[i]], "\n")
}
for (con in conns) DBI::dbDisconnect(con)
if (!all(control_results$ok)) stop("An ordinary or lexical control failed", call. = FALSE)
if (assert_mode && !all(results$ok)) {
  stop("Accepted signed/whitespace SQL output names failed desired behavior", call. = FALSE)
}
