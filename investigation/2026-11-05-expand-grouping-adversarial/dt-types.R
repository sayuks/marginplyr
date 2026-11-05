Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(".", quiet = TRUE)
sample_types <- list(
  integer = c(2L, 1L, NA), double = c(2, 1, NaN), raw = as.raw(1:3),
  complex = c(1i, NA_complex_, 2+3i), logical = c(NA, TRUE, FALSE),
  character = c("b", "a", NA),
  factor = factor(c("b", "a", NA), levels = c("b", "a", NA), exclude = NULL),
  ordered = ordered(c("b", "a", NA), levels = c("b", "a", "c")),
  date = as.Date(c("2020-01-03", NA, "2020-01-01")),
  datetime = as.POSIXct(c("2020-01-03", NA, "2020-01-01"), tz = "America/New_York"),
  difftime = as.difftime(c(2, 1, NA), units = "hours"),
  hms = hms::hms(c(2, 1, NA)),
  int64 = bit64::as.integer64(c("9007199254740994", "9007199254740993", NA)),
  list = list(2L, 1L, NULL),
  list_of = vctrs::list_of(2L, 1L, NULL, .ptype = integer()),
  matrix = I(matrix(c(2L, 1L, NA, 1:3), ncol = 2L)),
  POSIXlt = I(as.POSIXlt(c("2020-01-03", NA, "2020-01-01"), tz = "America/New_York"))
)
capture <- function(expr) tryCatch(suppressMessages(suppressWarnings(expr)), error = identity)
flag <- function(x) if (inherits(x, "error")) class(x)[1L] else "ok"
res <- list()
for (nm in names(sample_types)) for (size in c(0L, 1L, 3L)) {
  key <- vctrs::vec_slice(sample_types[[nm]], seq_len(size))
  d <- tibble::tibble(g = rep("a", size), row_id = seq_len(size), k = key)
  dt <- capture(dtplyr::lazy_dt(d))
  if (inherits(dt, "error")) next
  for (role in c("fixed", "dimension", "payload")) for (omitted_first in c(FALSE, TRUE)) {
    included <- if (omitted_first) c(FALSE, TRUE) else c(TRUE, FALSE)
    dim <- if (role == "dimension") "k" else "g"
    by <- if (role == "fixed") "k" else character()
    spec <- grouping_sets(!!!lapply(included, function(i) {
      if (i) grouping_set(tidyselect::all_of(dim)) else grouping_set()
    }))
    replacement <- vctrs::vec_init(d[[dim]], size)
    ordinary <- function(x) {
      branches <- lapply(seq_along(included), function(i) {
        sid <- as.integer(i)
        out <- dplyr::mutate(x, sid = !!sid)
        if (included[[i]]) return(out)
        dplyr::mutate(out, !!dim := .env$replacement)
      })
      dplyr::collect(dplyr::union_all(branches[[1L]], branches[[2L]]))
    }
    margin <- function(x) {
      dplyr::collect(expand_with_margins(x, .by = tidyselect::all_of(by),
        .grouping = spec, .margin_label = NULL, .id = "sid"))
    }
    canon <- function(x) {
      if (inherits(x, "error")) return(flag(x))
      x <- x[order(x$sid, x$row_id), , drop = FALSE]
      as.list(x[sort(names(x))])
    }
    ordinary_lazy <- capture(ordinary(dt))
    eager <- capture(margin(d))
    lazy <- capture(margin(dt))
    verdict <- if (identical(canon(eager), canon(lazy))) "eager-lazy match"
        else if (identical(canon(lazy), canon(ordinary_lazy))) "lazy ordinary match"
        else if (inherits(ordinary_lazy, "error")) "no lazy oracle"
        else "mismatch vs lazy ordinary"
    res[[length(res)+1L]] <- data.frame(nm, size, role, omitted_first, verdict,
      eager = flag(eager), lazy = flag(lazy), ordinary_lazy = flag(ordinary_lazy))
  }
}
df <- do.call(rbind, res)
print(table(df$verdict))
print(df[df$verdict == "mismatch vs lazy ordinary", ], row.names = FALSE)
print(unique(df[df$verdict == "no lazy oracle", c("nm", "role", "lazy", "ordinary_lazy")]), row.names=FALSE)
