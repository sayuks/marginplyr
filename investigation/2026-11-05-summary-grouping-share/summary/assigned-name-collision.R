pkgload::load_all(".", quiet = TRUE)

output <- "frame(dplyr::pick(x))"
frame <- function(columns) tibble::tibble(out = sum(columns))
scalar <- function(columns) sum(columns)
inner <- function(columns) tibble::tibble(!!!stats::setNames(
  list(sum(columns)), output
))
data <- tibble::tibble(g = c("a", "a", "b"), x = 1:3)

plain <- summarize_with_margins(data, frame(dplyr::pick(x)))
stopifnot(identical(plain, dplyr::summarise(data, frame(dplyr::pick(x)))))
actual <- tryCatch(
  summarize_with_margins(data, frame(dplyr::pick(x)), .id = output),
  error = identity
)
cat("Minimal ordinary output and Margin refusal\n")
print(plain)
assert <- "--assert" %in% commandArgs(TRUE)
if (assert) {
  stopifnot(identical(actual[[output]], 1L), identical(actual$out, 6L))
} else {
  cat(conditionMessage(actual), "\n")
}

base_inputs <- list(as.data.frame(data), data, data.table::as.data.table(data))
cases <- 0L
for (input in base_inputs) {
  input[[output]] <- input$g
  expr <- quote(frame(dplyr::pick(x)))
  for (mode in c("fixed", "single", "rollup")) {
    ordinary <- dplyr::summarise(input, !!expr, .by = dplyr::all_of(output))
    stopifnot(
      identical(names(ordinary), c(output, "out")),
      identical(ordinary$out, c(3L, 3L))
    )
    result <- tryCatch(switch(
      mode,
      fixed = summarize_with_margins(
        input, !!expr, .by = dplyr::all_of(output)
      ),
      single = summarize_with_margins(
        input, !!expr, .grouping = grouping_set(dplyr::all_of(output))
      ),
      rollup = summarize_with_margins(
        input, !!expr, .grouping = rollup(dplyr::all_of(output))
      )
    ), error = identity)
    if (assert) {
      if (identical(mode, "rollup")) {
        stopifnot(identical(sort(result$out), c(3L, 3L, 6L)))
      } else {
        stopifnot(identical(result, ordinary))
      }
    } else {
      stopifnot(
        inherits(result, "marginplyr_error"),
        grepl("cannot overwrite grouping column", conditionMessage(result))
      )
    }
    cases <- cases + 1L
  }

  input[[output]] <- NULL
  for (sort in c("none", "first", "last")) {
    intended <- summarize_with_margins(
      input, !!expr, .grouping = rollup(g), .id = "control", .sort = sort
    )
    stopifnot(
      identical(names(intended), c("g", "control", "out")),
      identical(sort(intended$out), c(3L, 3L, 6L))
    )
    result <- tryCatch(summarize_with_margins(
      input, !!expr, .grouping = rollup(g), .id = output, .sort = sort
    ), error = identity)
    if (assert) {
      names(intended)[2L] <- output
      stopifnot(identical(result, intended))
    } else {
      stopifnot(
        inherits(result, "marginplyr_error"),
        grepl("conflicts with a summary output", conditionMessage(result))
      )
    }
    cases <- cases + 1L
  }
  real_scalar <- tryCatch(summarize_with_margins(
    input, scalar(dplyr::pick(x)), .id = "scalar(dplyr::pick(x))"
  ), error = identity)
  real_frame <- tryCatch(summarize_with_margins(
    input, inner(dplyr::pick(x)), .id = output
  ), error = identity)
  packed <- summarize_with_margins(input, packed = frame(dplyr::pick(x)))
  stopifnot(
    inherits(real_scalar, "marginplyr_error"),
    inherits(real_frame, "marginplyr_error"),
    identical(packed$packed$out, 6L)
  )
}
cat(cases, if (assert) "successful calls;" else "false collisions;",
    "real scalar and expanded output collisions refused\n")
