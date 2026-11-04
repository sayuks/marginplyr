suppressPackageStartupMessages(library(dplyr))
pkgload::load_all(quiet = TRUE)
values <- data.frame(g = c("a", "b", "a"), x = 1:3, y = 10:12)
inputs <- list(frame = values, tibble = tibble::as_tibble(values),
               datatable = data.table::as.data.table(values))
plain <- list(across = quote(across(x, sum)),
              pick = quote(sum(pick(x))),
              if_any = quote(sum(if_any(x, ~ .x > 9L))),
              if_all = quote(sum(if_all(x, ~ .x > 9L))))
qualified <- list(across = quote(dplyr::across(x, sum)),
                  pick = quote(sum(dplyr::pick(x))),
                  if_any = quote(sum(dplyr::if_any(x, ~ .x > 9L))),
                  if_all = quote(sum(dplyr::if_all(x, ~ .x > 9L))))
grid <- expand.grid(input = names(inputs), grain = c("none","fixed","rollup"),
                    helper = names(plain), qualified = c(FALSE,TRUE),
                    preceded = c(FALSE,TRUE), stringsAsFactors = FALSE)
grid$unshadowed_matches <- grid$shadowed_matches <- NA
grid$generated_reads <- NA_integer_
for (i in seq_len(nrow(grid))) {
  cse <- grid[i, ]
  data <- inputs[[cse$input]]
  code <- (if (cse$qualified) qualified else plain)[cse$helper]
  if (cse$helper != "across") names(code) <- "value" else names(code) <- NULL
  if (cse$preceded) code <- c(list(rows = quote(dplyr::n())), code)
  by <- list(.by = quote(g))
  shadow_calls <- 0L
  run <- function(call, shadowed) {
    if (shadowed) quote <- function(...) {
      shadow_calls <<- shadow_calls + 1L
      base::quote(y)
    }
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
print(with(grid, table(helper, unshadowed_matches, shadowed_matches)))
print(with(grid, table(grain, generated_reads)))
stopifnot(all(grid$unshadowed_matches), !any(grid$shadowed_matches),
          all(grid$generated_reads > 0L))
