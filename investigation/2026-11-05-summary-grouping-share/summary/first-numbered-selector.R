pkgload::load_all(".", quiet = TRUE)

compare <- function(input, spelling, helper, grain, ...) {
  selector <- rlang::call2(helper, as.name(spelling), quote(sum))
  direct <- rlang::new_quosure(selector)
  nested <- rlang::new_quosure(rlang::call2("identity", selector))
  ordinary <- switch(
    grain,
    whole = dplyr::summarise(input, packed = !!direct)$packed$x,
    fixed = dplyr::summarise(input, packed = !!direct, .by = g)$packed$x,
    rollup = c(
      dplyr::summarise(input, packed = !!direct, .by = g)$packed$x,
      dplyr::summarise(input, packed = !!direct)$packed$x
    )
  )

  run <- function(expr, before = FALSE) {
    prefix <- if (before) list(unused = 1L) else list()
    switch(
      grain,
      whole = summarize_with_margins(input, !!!prefix, packed = !!expr),
      fixed = summarize_with_margins(
        input, !!!prefix, packed = !!expr, .by = g
      ),
      rollup = summarize_with_margins(
        input, !!!prefix, packed = !!expr, .grouping = rollup(g)
      )
    )$packed$x
  }

  actual <- tryCatch(run(direct), error = identity)
  stopifnot(
    identical(run(direct, before = TRUE), ordinary),
    identical(run(nested), ordinary)
  )
  list(ordinary = ordinary, actual = actual)
}

data <- tibble::tibble(g = c("a", "a", "b"), x = 1:3)
wrap <- function(verb, ...) verb(data, packed = dplyr::across(..1, sum))
ordinary <- wrap(dplyr::summarise, x)
actual <- tryCatch(wrap(summarize_with_margins, x), error = identity)
cat("Minimal ordinary packed x =", ordinary$packed$x, "\n")
assert <- "--assert" %in% commandArgs(TRUE)
if (assert) {
  stopifnot(identical(actual, ordinary))
} else {
  cat("Minimal Margin:", conditionMessage(actual), "\n")
}

inputs <- list(as.data.frame(data), data, data.table::as.data.table(data))
forms <- c("..1", "..2", "..01", "..+1", ".. 1")
helpers <- list(quote(across), quote(dplyr::across))
across <- dplyr::across
cases <- 0L
for (input in inputs) {
  for (spelling in forms) {
    for (helper in helpers) {
      for (grain in c("whole", "fixed", "rollup")) {
        trial <- if (identical(spelling, "..2")) {
          compare(input, spelling, helper, grain, unused, x)
        } else {
          compare(input, spelling, helper, grain, x)
        }
        if (assert) {
          stopifnot(identical(trial$actual, trial$ordinary))
        } else {
          stopifnot(
            inherits(trial$actual, "vctrs_error_subscript_oob"),
            grepl(spelling, conditionMessage(trial$actual), fixed = TRUE)
          )
        }
        cases <- cases + 1L
      }
    }
  }
}
cat(cases, if (assert) "agreements and" else "Margin-only refusals and",
    2L * cases, "passing controls\n")
