# Bounded scope-rebinding investigation, 2026-11-02.
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(".", quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))

setters <- list(
  direct = quote(list2env(list(e = environment()), envir = environment())),
  qualified = quote(base::list2env(list(e = environment()), envir = environment())),
  get = quote((get("list2env"))(list(e = environment()), envir = environment())),
  match_fun = quote((match.fun("list2env"))(list(e = environment()), envir = environment())),
  do_call = quote(do.call("list2env", list(list(e = environment()), envir = environment())))
)
results <- list()
evaluate <- function(input_kind, helper, direction, fixed, mutation, share = TRUE,
                     external = FALSE, shadow = FALSE) {
  e <- new.env(parent = baseenv())
  e$prior <- 7
  input <- if (fixed) {
    tibble(k = c("P", "P", NA, NA), g = c("a", "b", "a", "b"),
           x = c(1, 3, 2, 6), prior = c(100, 300, 200, 600))
  } else {
    tibble(g = c("a", "b"), x = c(1, 3), prior = c(100, 300))
  }
  input <- switch(input_kind, tibble = input, data.frame = as.data.frame(input),
                  data.table = data.table::as.data.table(input))
  if (shadow) list2env <- function(...) invisible(NULL)
  value <- if (external) quote(sum(get("prior", envir = e))) else {
    rlang::expr({ !!mutation; sum(get("prior", envir = e)) })
  }
  helper_call <- rlang::call2(helper, quote(value))
  dots <- if (direction == "source") {
    list(prior = quote(sum(x)), value = value)
  } else {
    list(value = quote(sum(x)), prior = if (share) helper_call else quote(sum(x)),
         derived = value)
  }
  if (direction == "source" && share) dots$fraction <- helper_call
  call <- rlang::call2("summarize_with_margins", quote(input), !!!dots,
                       .grouping = quote(rollup(g)),
                       .by = if (fixed) quote(k) else NULL)
  tryCatch(eval(call), error = identity)
}

for (input_kind in c("tibble", "data.frame", "data.table")) {
  for (helper in c("share_of_total", "share_of_parent")) {
    for (direction in c("source", "later")) for (fixed in c(FALSE, TRUE)) {
      canonical <- evaluate(input_kind, helper, direction, fixed,
                            quote(assign("e", environment(), envir = environment())))
      external <- evaluate(input_kind, helper, direction, fixed, NULL, external = TRUE)
      unrelated <- evaluate(input_kind, helper, direction, fixed,
                            quote(list2env(list(other = environment()), envir = environment())))
      shadow <- evaluate(input_kind, helper, direction, fixed, setters$direct, shadow = TRUE)
      stopifnot(inherits(canonical, "marginplyr_error"),
                !inherits(external, "error"), !inherits(unrelated, "error"),
                !inherits(shadow, "error"))
      for (spelling in names(setters)) {
        result <- evaluate(input_kind, helper, direction, fixed, setters[[spelling]])
        ordinary <- evaluate(input_kind, helper, direction, fixed, setters[[spelling]], share = FALSE)
        stopifnot(!inherits(ordinary, "error"))
        results[[length(results) + 1L]] <- data.frame(
          input = input_kind, helper = helper, direction = direction,
          fixed = fixed, spelling = spelling,
          refused = inherits(result, "marginplyr_error"),
          ordinary_succeeded = !inherits(ordinary, "error")
        )
      }
    }
  }
}
result <- bind_rows(results)
write.csv(result, "investigation/2026-11-02-summary-grouping-share-bug-hunt/share/list2env-matrix.csv",
          row.names = FALSE)
cat("Investigated calls:", nrow(result), "\n")
cat("Missing dependency refusals:", sum(!result$refused), "\n")
cat("Successful no-share controls:", sum(result$ordinary_succeeded), "\n")
cat("Successful canonical/external/unrelated/shadow controls:", 24L * 4L, "\n")
stopifnot(!any(result$refused), all(result$ordinary_succeeded))
