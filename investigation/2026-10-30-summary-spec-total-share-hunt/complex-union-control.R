# Ordinary dtplyr set operations reproduce this multi-grain limitation.
pkgload::load_all(".", quiet = TRUE)
source <- dtplyr::lazy_dt(tibble::tibble(
  f = complex(real = 1:2), g = c("a", "b"), x = c(1, 3)
))
detail <- dplyr::summarise(source, v = sum(x), .by = c(f, g))
grand <- dplyr::mutate(dplyr::summarise(source, v = sum(x), .by = f),
                       g = "Total")
grand <- dplyr::select(grand, f, g, v)
print(dplyr::collect(detail))
print(dplyr::collect(grand))
capture <- function(value) tryCatch(force(value), error = identity)
ordinary <- capture(dplyr::collect(dplyr::union_all(detail, grand)))
margin <- capture(dplyr::collect(summarize_with_margins(
  source, v = sum(x), .by = f, .grouping = rollup(g)
)))
stopifnot(inherits(ordinary, "error"), inherits(margin, "error"),
          identical(class(ordinary), class(margin)),
          identical(conditionMessage(ordinary), conditionMessage(margin)))
cat("ORDINARY UNION:", conditionMessage(ordinary), "\n")
cat("MARGIN UNION:", conditionMessage(margin), "\n")
