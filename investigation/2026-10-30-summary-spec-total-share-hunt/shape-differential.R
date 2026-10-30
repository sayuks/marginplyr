# Dated investigation evidence. Run from the checkout root.
pkgload::load_all(".", quiet = TRUE)

options(dplyr.summarise.inform = FALSE)

cap <- function(x) tryCatch(suppressWarnings(force(x)), error = identity)

show <- function(x) if (inherits(x, "error")) cat(class(x)[1], conditionMessage(x),
  "\n") else print(x)

run <- function(input, dots, margin = FALSE) {
  verb <- if (margin)
    summarize_with_margins
  else dplyr::summarise
  rlang::eval_tidy(rlang::expr(verb(input, !!!dots, .by = g)))
}

values <- list(i = 1L, d = 1, i0 = integer(), nil = NULL, li = list(1L), li0 = list(),
  df = data.frame(z = 1L), df0 = data.frame(z = integer()), zerocol = vctrs::new_data_frame(list(),
    n = 1L), zerozero = data.frame(), mat = matrix(1L, nrow = 1L), mat0 = matrix(integer(),
    nrow = 0L, ncol = 1L), array = array(1L, c(1, 1, 1)), packed = tibble::tibble(z = tibble::tibble(k = 1L)),
  named = structure(1L, names = "abc"), table = structure(1L, dim = 1L, class = "table"),
  date = as.Date("2026-01-01"), raw = as.raw(1L), complex = 1 + (0+1i))

data <- tibble::tibble(g = c("a", "a", "b"), x = 1:3)

count <- 0L

differences <- list()

for (a in names(values)) for (b in names(values)) {
  f <- local({
    aa <- values[[a]]
    bb <- values[[b]]
    function(x) if (length(x) > 1L)
      aa
    else bb
  })
  for (named in c(FALSE, TRUE)) {
    dots <- list(rlang::expr(f(x)))
    if (named)
      names(dots) <- "out"
    expected <- cap(run(data, dots))
    actual <- cap(run(data, dots, TRUE))
    count <- count + 1L
    if (inherits(expected, "error") && inherits(actual, "error"))
      next
    if (!isTRUE(all.equal(expected, actual))) {
      key <- paste(a, b, named)
      differences[[key]] <- list(expected = expected, actual = actual)
      cat("DIFF", key, "\n")
      show(expected)
      show(actual)
    }
  }
}

cat("TOTAL", count, "DIFFERENCES", length(differences), "\n")

stopifnot(length(differences) == 0L)
