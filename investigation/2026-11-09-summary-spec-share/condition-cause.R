args <- commandArgs(trailingOnly = TRUE)
root <- if (length(args)) args[[1L]] else getwd()
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(root, quiet = TRUE)
d <- data.frame(g = c("a", "b"), x = c(1, 3))
cause <- tryCatch(marginplyr::share_of_total(x), error = identity)
failure <- rlang::error_cnd(
  "caller_summary_failure", message = "User summary failed",
  parent = cause, token = new.env(parent = emptyenv())
)
fail <- function(x) stop(failure)
native <- tryCatch(dplyr::summarise(d, value = fail(x), .by = g),
                   error = identity)
margin <- tryCatch(marginplyr::summarize_with_margins(
  d, value = fail(x), .grouping = marginplyr::rollup(g)
), error = identity)
chain <- function(e) {
  if (!inherits(e, "condition")) return(list())
  c(list(e), chain(e$parent))
}
stopifnot(identical(native$parent, failure))
stopifnot(inherits(margin, "marginplyr_error"))
stopifnot(!any(vapply(chain(margin), identical, logical(1), failure)))
stopifnot(!grepl("User summary failed", conditionMessage(margin), fixed = TRUE))
cat("Confirmed: ordinary dplyr retains the caller error; marginplyr erases it.\n")
print(vapply(chain(native), function(e) class(e)[[1L]], character(1)))
print(vapply(chain(margin), function(e) class(e)[[1L]], character(1)))
