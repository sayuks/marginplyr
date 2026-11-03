Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
dir <- "investigation/2026-11-03-summary-grouping-share-bug-hunt/share"
cat("SHA:", system2("git", c("rev-parse", "HEAD"), stdout = TRUE), "\n")
cat(R.version.string, "\n")
for (p in c("dplyr", "rlang", "dtplyr", "data.table", "pkgload")) {
  cat(p, as.character(packageVersion(p)), "\n")
}
data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(100, 300))
setters <- alist(
  direct = base::assign("outside", environment(), envir = environment()),
  literal = base::list2env(list(outside = environment()), envir = environment()),
  dynamic_setnames = base::list2env(setNames(list(environment()), "outside"), envir = environment()),
  dynamic_structure = base::list2env(structure(list(environment()), names = "outside"), envir = environment()),
  block_list = base::list2env({list(outside = environment())}, envir = environment()),
  identity_list = base::list2env(identity(list(outside = environment())), envir = environment()),
  c_list = base::list2env(c(list(outside = environment())), envir = environment()),
  local_list = base::list2env(local(list(outside = parent.frame())), envir = environment()),
  eval_list = base::list2env(eval(quote(list(outside = environment()))), envir = environment()),
  assigned_list = { values <- list(outside = environment()); base::list2env(values, envir = environment()) },
  bracket_setter = environment()[["outside"]] <- environment(),
  dollar_setter = environment()$outside <- environment(),
  env_bind = rlang::env_bind(environment(), outside = environment()),
  env_poke = rlang::env_poke(environment(), "outside", environment()),
  assign_inherits = base::assign("outside", environment(), inherits = FALSE),
  within_literal = base::list2env(list(outside = environment()), envir = parent.frame())
)
results <- list()
for (helper in c("share_of_total", "share_of_parent")) {
  for (direction in c("source", "later")) {
    for (setter in names(setters)) {
      outside <- new.env(parent = baseenv())
      outside$prior <- 7
      body <- rlang::expr({ !!setters[[setter]]; sum(get("prior", envir = outside)) })
      args <- if (direction == "source") {
        list(prior = quote(sum(x)), value = body,
             fraction = rlang::call2(helper, quote(value)))
      } else {
        list(value = quote(sum(x)), prior = rlang::call2(helper, quote(value)),
             derived = body)
      }
      out <- tryCatch(eval(rlang::call2("summarize_with_margins", quote(data),
                                       !!!args, .grouping = quote(rollup(g)))),
                      error = identity)
      message <- if (inherits(out, "error")) conditionMessage(out) else ""
      values <- if (inherits(out, "error")) "" else paste(out[[if (direction == "source") "value" else "derived"]], collapse = ",")
      row <- data.frame(helper, direction, setter,
                        refused = inherits(out, "marginplyr_error"),
                        error = inherits(out, "error"), values, message)
      results[[length(results) + 1L]] <- row
      cat(helper, direction, setter, if (row$error) class(out)[1L] else values, "\n")
    }
  }
}
results <- do.call(rbind, results)
write.csv(results, file.path(dir, "lexical-discovery.csv"), row.names = FALSE)
print(results[, c("helper", "direction", "setter", "refused", "error", "values")], row.names = FALSE)
