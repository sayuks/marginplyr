Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(quiet = TRUE)
dir <- "investigation/2026-11-03-summary-grouping-share-bug-hunt/share"
setters <- alist(
  block = base::list2env({list(outside = environment())}, envir = environment()),
  identity = base::list2env(identity(list(outside = environment())), envir = environment()),
  keys = base::list2env(setNames(list(environment()), "outside"), envir = environment()),
  literal = base::list2env(list(outside = environment()), envir = environment()),
  assign = base::assign("outside", environment(), envir = environment()),
  external = NULL,
  unrelated = base::list2env(list(other = environment()), envir = environment())
)
run_case <- function(type, fixed, helper, direction, setter, shares = TRUE) {
  input <- data.frame(g = c("a", "b", "a", "b"), x = c(1, 3, 2, 6),
                      prior = c(100, 300, 200, 600), f = c("F", "F", NA, NA))
  data <- switch(type, tibble = tibble::as_tibble(input),
                 data.table = data.table::as.data.table(input), input)
  outside <- new.env(parent = baseenv())
  outside$prior <- 7
  previous <- if (shares && direction == "later") rlang::call2(helper, quote(value)) else quote(sum(x))
  body <- rlang::expr({ !!setters[[setter]]; sum(get("prior", envir = outside)) })
  args <- if (direction == "source") {
    list(prior = quote(sum(x)), value = body)
  } else {
    list(value = quote(sum(x)), prior = previous, derived = body)
  }
  if (shares && direction == "source") args$fraction <- rlang::call2(helper, quote(value))
  args$.grouping <- quote(rollup(g))
  if (fixed) args$.by <- quote(f)
  out <- tryCatch(eval(rlang::call2("summarize_with_margins", quote(data), !!!args)), error = identity)
  unchanged <- identical(outside$prior, 7) && identical(parent.env(outside), baseenv())
  data.frame(type, fixed, helper, direction, setter, shares,
             error = inherits(out, "error"), package_error = inherits(out, "marginplyr_error"),
             values = if (inherits(out, "error")) "" else paste(out[[if (direction == "source") "value" else "derived"]], collapse = ","),
             unchanged, message = if (inherits(out, "error")) conditionMessage(out) else "")
}
rows <- list()
for (type in c("data.frame", "tibble", "data.table")) {
  for (fixed in c(FALSE, TRUE)) {
    for (helper in c("share_of_total", "share_of_parent")) {
      for (direction in c("source", "later")) {
        for (setter in names(setters)) {
          rows[[length(rows) + 1L]] <- run_case(type, fixed, helper, direction, setter)
          if (setter %in% c("block", "identity", "keys")) {
            rows[[length(rows) + 1L]] <- run_case(type, fixed, helper, direction, setter, FALSE)
          }
        }
      }
    }
  }
}
out <- do.call(rbind, rows)
finding <- out$shares & out$setter %in% c("block", "identity", "keys")
refusal <- out$shares & out$setter %in% c("literal", "assign")
accepted <- !finding & !refusal
stopifnot(all(!out$error[finding]), all(out$package_error[refusal]),
          all(!out$error[accepted]), all(out$unchanged))
write.csv(out, file.path(dir, "list2env-unresolved-matrix.csv"), row.names = FALSE)
cat("SHA:", system2("git", c("rev-parse", "HEAD"), stdout = TRUE), "\n")
cat("Missing refusals:", sum(finding), "Controls:", sum(!finding), "\n")
print(with(out, table(setter, shares, error)))
