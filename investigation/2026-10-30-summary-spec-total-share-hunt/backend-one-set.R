# Dated investigation evidence. Run from the checkout root.
pkgload::load_all(".", quiet = TRUE)

options(dplyr.summarise.inform = FALSE)

cap <- function(x) tryCatch(suppressWarnings(force(x)), error = identity)

collect_or_error <- function(x) if (inherits(x, "error")) x else cap(dplyr::collect(x))

show <- function(x) if (inherits(x, "error")) cat(class(x)[1], conditionMessage(x),
  "\n") else print(x)

con1 <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

con2 <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE), dbdir = ":memory:")

exprs <- list(rlang::exprs(n = dplyr::n()), rlang::exprs(s = sum(x)), rlang::exprs(s = sum(x,
  na.rm = TRUE)), rlang::exprs(m = mean(x)), rlang::exprs(m = median(x)),
  rlang::exprs(m = min(x)), rlang::exprs(m = max(x)), rlang::exprs(m = sd(x)),
  rlang::exprs(m = var(x)), rlang::exprs(m = length(x)), rlang::exprs(m = dplyr::n_distinct(x)),
  rlang::exprs(m = dplyr::first(x)), rlang::exprs(m = dplyr::last(x)), rlang::exprs(m = list(x)),
  rlang::exprs(m = list(dplyr::pick(x, y))), rlang::exprs(dplyr::across(c(x,
    y), sum, .names = "sum_{.col}")), rlang::exprs(dplyr::across(c(x, y),
    list(mean = mean, sum = sum))), rlang::exprs(dplyr::across(c(x, y),
    ~sum(.x))), rlang::exprs(dplyr::across(c(x, y), ~tibble::tibble(s = sum(.x),
    m = mean(.x)), .unpack = TRUE)), rlang::exprs(m = ifelse(sum(x) > 0,
    sum(y), sum(x))), rlang::exprs(m = dplyr::if_else(sum(x) > 0, sum(y),
    sum(x))), rlang::exprs(m = sum(x + y)), rlang::exprs(m = sum(x * y)),
  rlang::exprs(m = sum(x)/sum(y)), rlang::exprs(m = sum(x, na.rm = TRUE),
    n = m * 2), rlang::exprs(tibble::tibble(m = sum(x), n = sum(y))), rlang::exprs(m = tibble::tibble(s = sum(x))),
  rlang::exprs(x = sum(x), y = sum(y)), rlang::exprs(m = quantile(x, na.rm = TRUE)),
  rlang::exprs(m = unique(x)), rlang::exprs(m = sort(x)), rlang::exprs(m = NULL),
  rlang::exprs(m = NA), rlang::exprs(m = NA_real_), rlang::exprs(m = "literal"),
  rlang::exprs(m = 1L), rlang::exprs(m = as.Date("2026-01-01")), rlang::exprs(m = as.character(sum(x))),
  rlang::exprs(m = as.numeric(sum(x))))

count <- 0L

success <- 0L

paired <- 0L

absorptions <- 0L

diffs <- list()

for (empty in c(FALSE, TRUE)) for (layout in c("normal", "special", "unicode")) {
  dat <- data.frame(g = c("a", "a", "b", NA), x = c(1, NA, 3, -1), y = c(4,
    5, 6, 7))
  if (empty)
    dat <- dat[FALSE, ]
  extras <- switch(layout, normal = c("extra1", "extra2", "extra3"), special = c(".BY",
    ".SD", ".N"), unicode = c("日本語", "é", "é"))
  for (n in extras) dat[[n]] <- seq_len(nrow(dat))
  inputs <- list(local = dat, dt = dtplyr::lazy_dt(dat), arrow = arrow::arrow_table(dat),
    sqlite = dplyr::copy_to(con1, dat, paste0("s", empty, layout)), duck = dplyr::copy_to(con2,
      dat, paste0("s", empty, layout)))
  for (nm in names(inputs)) for (i in seq_along(exprs)) {
    inp <- inputs[[nm]]
    dots <- exprs[[i]]
    ordinary <- cap(rlang::eval_tidy(rlang::expr(dplyr::summarise(inp, !!!dots,
      .by = g))))
    got <- cap(rlang::eval_tidy(rlang::expr(summarize_with_margins(inp,
      !!!dots, .by = g))))
    expected <- collect_or_error(ordinary)
    actual <- collect_or_error(got)
    count <- count + 1L
    if (inherits(expected, "error") && inherits(actual, "error")) {
      paired <- paired + 1L
      next
    }
    if (!inherits(expected, "error"))
      success <- success + 1L
    if (nm == "arrow" && inherits(actual, "marginplyr_error") && grepl("Arrow cannot evaluate this summary",
      conditionMessage(actual), fixed = TRUE)) {
      absorptions <- absorptions + 1L
      next
    }
    if (!inherits(expected, "error")) {
      expected <- as.data.frame(expected)
      expected <- expected[order(expected$g), , drop = FALSE]
      rownames(expected) <- NULL
    }
    if (!inherits(actual, "error")) {
      actual <- as.data.frame(actual)
      actual <- actual[order(actual$g), , drop = FALSE]
      rownames(actual) <- NULL
    }
    if (!isTRUE(all.equal(expected, actual))) {
      key <- paste(empty, layout, nm, i)
      diffs[[key]] <- list(dots = dots, expected = expected, actual = actual)
      cat("DIFF", key, "\n")
      print(dots)
      cat("EXPECTED\n")
      show(expected)
      cat("ACTUAL\n")
      show(actual)
    }
  }
}

cat("TOTAL", count, "ORDINARY SUCCESS", success, "PAIRED ERROR", paired, "ABSORPTION REFUSALS",
  absorptions, "DIFFERENCES", length(diffs), "\n")

stopifnot(length(diffs) == 0L)

DBI::dbDisconnect(con1)

DBI::dbDisconnect(con2, shutdown = TRUE)
