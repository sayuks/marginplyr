source_dir <- Sys.getenv("MARGINPLYR_HUNT_SOURCE", ".")
output_dir <- Sys.getenv("MARGINPLYR_HUNT_OUTPUT", tempfile("dots-boundaries-"))
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(source_dir, quiet = TRUE)
capture <- function(expr) tryCatch(list(ok = TRUE, value = expr),
                                  error = function(cnd) list(ok = FALSE, value = cnd))
describe <- function(result) {
  if (result$ok) return(paste(vapply(result$value$included, paste,
                                   collapse = ",", character(1)), collapse = ";"))
  paste(class(result$value)[[1L]], conditionMessage(result$value), sep = ": ")
}

dat <- tibble::tibble(a = c("x", "y"), value = 1:2)
child <- grouping_set(a)
plain <- function(...) inspect_grouping(dat, .grouping = grouping_sets(..1), .format = "list")
wrapped <- function(...) inspect_grouping(dat, .grouping = grouping_sets((..1)), .format = "list")
named_plain <- function(x = child) inspect_grouping(dat, .grouping = grouping_sets(x), .format = "list")
named_wrapped <- function(x = child) inspect_grouping(dat, .grouping = grouping_sets((x)), .format = "list")
default_plain <- function(x = child) plain(x)
default_wrapped <- function(x = child) wrapped(x)
second_plain <- function(...) inspect_grouping(dat, .grouping = grouping_sets(..2), .format = "list")
second_wrapped <- function(...) inspect_grouping(dat, .grouping = grouping_sets((..2)), .format = "list")
top_plain <- function(...) inspect_grouping(dat, .grouping = ..1, .format = "list")
top_wrapped <- function(...) inspect_grouping(dat, .grouping = (..1), .format = "list")

# jarl-ignore missing_argument: Empty dots payloads are intentional boundary inputs.
cases <- list(
  named_default_plain = capture(named_plain()),
  named_default_wrapped = capture(named_wrapped()),
  forwarded_default_plain = capture(default_plain()),
  forwarded_default_wrapped = capture(default_wrapped()),
  missing_first_plain = capture(plain(, child)),
  missing_first_wrapped = capture(wrapped(, child)),
  absent_first_plain = capture(plain()),
  absent_first_wrapped = capture(wrapped()),
  second_named_reordered_plain = capture(second_plain(unused = NULL, chosen = child)),
  second_named_reordered_wrapped = capture(second_wrapped(unused = NULL, chosen = child)),
  second_missing_plain = capture(second_plain(NULL, , child)),
  second_missing_wrapped = capture(second_wrapped(NULL, , child)),
  top_plain = capture(top_plain(child)),
  top_wrapped = capture(top_wrapped(child))
)
stopifnot(cases$named_default_plain$ok, cases$named_default_wrapped$ok,
          cases$forwarded_default_plain$ok, !cases$forwarded_default_wrapped$ok,
          cases$second_named_reordered_plain$ok,
          !cases$second_named_reordered_wrapped$ok,
          cases$top_plain$ok, cases$top_wrapped$ok,
          inherits(cases$missing_first_plain$value, "marginplyr_error"),
          inherits(cases$second_missing_plain$value, "marginplyr_error"))
records <- lapply(names(cases), function(name) {
  data.frame(case = name, ok = cases[[name]]$ok, observation = describe(cases[[name]]))
})

for (dot in 1:2) {
  for (prefix in c("", "0", "00", "+", " ", " +")) {
    spelling <- paste0("..", prefix, dot)
    payload <- as.name(spelling)
    for (parens in 0:2) {
      dat_alias <- dat
      allowed <- prefix != ""
      if (allowed) dat_alias[[spelling]] <- c("wrong-1", "wrong-2")
      env <- rlang::env(globalenv(), dat_alias = dat_alias)
      arg <- payload
      for (i in seq_len(parens)) arg <- rlang::call2("(", arg)
      body <- rlang::call2("inspect_grouping", quote(dat_alias),
                          .grouping = rlang::call2("grouping_sets", arg),
                          .format = "list")
      wrapper <- rlang::new_function(alist(... = ), body, env)
      result <- capture(do.call(wrapper, c(rep(list(NULL), dot - 1L), list(child))))
      expected <- if (parens == 0L) "a" else if (allowed) spelling else NULL
      if (is.null(expected)) {
        stopifnot(!result$ok)
      } else {
        stopifnot(result$ok, identical(result$value$included, list(expected)))
      }
      records[[length(records) + 1L]] <- data.frame(
        case = paste0("alias=", spelling, ";parens=", parens),
        ok = result$ok, observation = describe(result)
      )
    }
  }
}
results <- do.call(rbind, records)
write.csv(results, file.path(output_dir, "numbered-dots-boundaries.csv"), row.names = FALSE)
print(results[seq_len(14L), ], row.names = FALSE)
cat("Additional alias controls:", nrow(results) - 14L, "\n")
print(sessionInfo())
