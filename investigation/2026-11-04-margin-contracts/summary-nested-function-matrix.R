suppressPackageStartupMessages(library(dplyr))
pkgload::load_all(quiet = TRUE)
values <- data.frame(g = c("a", "b", "a"), x = 1:3)
inputs <- list(frame = values, tibble = tibble::as_tibble(values),
               datatable = data.table::as.data.table(values))
inner <- tibble::tibble(z = c(1L, 2L), h = c("c", "d"))
operators <- alist(
  mutate(inner, z = z + 1L),
  filter(inner, z > 1L),
  select(inner, z),
  summarise(inner, z = sum(z)),
  reframe(inner, z = sum(z)),
  slice_head(inner, n = 1L),
  rowwise(inner),
  count(inner, wt = z, name = "z"),
  summarize_with_margins(inner, z = sum(z)),
  expand_with_margins(inner, .grouping = rollup(h))
)
operator_names <- vapply(operators, function(x) as.character(x[[1L]]), "")
grid <- expand.grid(input = names(inputs), grain = c("none","fixed","rollup"),
                    operator = seq_along(operators), qualified = c(FALSE,TRUE),
                    preceded = c(FALSE,TRUE), stringsAsFactors = FALSE)
grid$unshadowed_matches <- grid$shadowed_matches <- NA
grid$generated_reads <- NA_integer_
for (i in seq_len(nrow(grid))) {
  cse <- grid[i, ]
  data <- inputs[[cse$input]]
  nested <- operators[[cse$operator]]
  if (cse$qualified) {
    operator <- operator_names[[cse$operator]]
    namespace <- if (grepl("_with_margins$", operator)) "marginplyr" else "dplyr"
    nested[[1L]] <- rlang::call2("::", rlang::sym(namespace), rlang::sym(operator))
  }
  code <- list(value = rlang::expr(sum((!!nested)$z)))
  if (cse$preceded) code <- c(list(rows = quote(dplyr::n())), code)
  by <- list(.by = quote(g))
  shadow_calls <- 0L
  replacement <- function(...) {
    shadow_calls <<- shadow_calls + 1L
    function(...) function(...) data.frame(z = 99L)
  }
  run <- function(call, shadowed) {
    if (shadowed) assign("function", replacement, envir = environment())
    eval(call)
  }
  ordinary <- function(shadowed) {
    verb <- quote(dplyr::summarise)
    if (cse$grain == "rollup") {
      detail <- run(rlang::call2(verb, data, !!!code, !!!by), shadowed)
      total <- run(rlang::call2(verb, data, !!!code), shadowed)
      total$g <- NA_character_
      return(dplyr::bind_rows(detail, total))
    }
    args <- if (cse$grain == "fixed") by else list()
    run(rlang::call2(verb, data, !!!code, !!!args), shadowed)
  }
  margin <- function(shadowed) {
    args <- if (cse$grain == "rollup") {
      list(.grouping = quote(rollup(g)), .margin_label = NA_character_,
           .sort = "last")
    } else if (cse$grain == "fixed") by else list()
    run(rlang::call2(quote(marginplyr::summarize_with_margins), data,
                     !!!code, !!!args), shadowed)
  }
  control <- ordinary(FALSE)
  stopifnot(identical(control, ordinary(TRUE)), shadow_calls == 0L)
  grid$unshadowed_matches[[i]] <- identical(control, margin(FALSE))
  grid$shadowed_matches[[i]] <- identical(control, margin(TRUE))
  grid$generated_reads[[i]] <- shadow_calls
}
grid$operation <- operator_names[grid$operator]
print(with(grid, table(operation, unshadowed_matches, shadowed_matches)))
print(with(grid, table(grain, generated_reads)))
wrapped <- grid$operation != "rowwise"
stopifnot(all(grid$unshadowed_matches),
          !any(grid$shadowed_matches[wrapped]),
          all(grid$shadowed_matches[!wrapped]),
          all(grid$generated_reads[wrapped] > 0L),
          all(grid$generated_reads[!wrapped] == 0L))
