suppressPackageStartupMessages(library(dplyr))
pkgload::load_all(quiet = TRUE)
data <- data.frame(g = c("a", "b"), x = c(1L, 3L), y = c(2L, 3L))
named_share <- function(shadowed, helper) {
  if (shadowed) quote <- function(...) base::quote(y)
  summarize_with_margins(
    data, total = sum(dplyr::pick(x)), share = !!helper,
    .grouping = rollup(g), .sort = "last"
  )
}
for (helper in alist(share_of_total(total), share_of_parent(total))) {
  correct <- named_share(FALSE, helper)
  incorrect <- named_share(TRUE, helper)
  stopifnot(identical(correct$total, c(1L, 3L, 4L)),
            identical(correct$share, c(0.25, 0.75, 1)),
            identical(incorrect$total, c(2L, 3L, 5L)),
            identical(incorrect$share, c(0.4, 0.6, 1)))
  print(correct)
  print(incorrect)
}

caller_quote <- function(verb) {
  quote <- function(...) 7L
  verb(data, answer = quote(unbound))
}
replacement <- function(...) function() 7L
caller_function <- function(verb) {
  assign("function", replacement, envir = environment())
  verb(data, answer = (function() 11L)())
}
for (run in list(caller_quote, caller_function)) {
  ordinary <- run(dplyr::summarise)
  margin <- run(marginplyr::summarize_with_margins)
  stopifnot(identical(ordinary$answer, 7L), identical(ordinary, margin))
}

nested_control <- function(verb) {
  verb(
    data,
    returned = (function() {
      dplyr::mutate(return(sum(x)), z = 1L)
      99L
    })(),
    assigned = (function() {
      value <- 99L
      nested <- dplyr::mutate({
        value <- sum(x)
        data.frame(z = value)
      }, extra = rowSums(dplyr::pick(z)))
      value + nested$extra
    })()
  )
}
ordinary <- nested_control(dplyr::summarise)
margin <- nested_control(marginplyr::summarize_with_margins)
stopifnot(identical(ordinary$returned, 4L),
          identical(ordinary$assigned, 8),
          identical(ordinary, margin))
print(ordinary)
