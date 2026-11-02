# Dated exploratory evidence; ordinary branch composition is the value oracle.
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(quiet = TRUE)
library(dplyr)
out_dir <- Sys.getenv("MARGINPLYR_HUNT_OUTPUT", tempdir())
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
capture <- function(expr) {
  tryCatch(suppressWarnings(suppressMessages(force(expr))), error = identity)
}
families <- list(
  ts = ts(c(1, NA, 3), frequency = 12),
  mts = ts(matrix(c(1, NA, 3, 4, 5, 6), 3), frequency = 4),
  zoo = zoo::zoo(c(1, NA, 3), as.Date("2026-01-01") + 0:2),
  yearmon = zoo::as.yearmon(c("2026-01", "2026-02", NA)),
  yearqtr = zoo::as.yearqtr(c("2026 Q1", "2026 Q2", NA)),
  labelled_int = haven::labelled(c(1L, NA_integer_, 3L), c(yes = 1L)),
  labelled_spss = haven::labelled_spss(c(1, 99, NA), c(yes = 1), na_values = 99),
  labelled_chr = haven::labelled(c("a", NA, "c"), c(alpha = "a")),
  glue = glue::as_glue(c("a", NA, "c")),
  hms = hms::hms(c(1, NA, 3)),
  blob = blob::blob(as.raw(1), NULL, as.raw(3)),
  bit = bit::as.bit(c(TRUE, NA, FALSE)),
  bitwhich = bit::as.bitwhich(c(TRUE, NA, FALSE)),
  hexmode = as.hexmode(c(1L, NA_integer_, 3L)),
  octmode = as.octmode(c(1L, NA_integer_, 3L)),
  roman = as.roman(c(1L, NA_integer_, 3L)),
  difftime_hours = as.difftime(c(1, NA, 3), units = "hours"),
  Surv_multi = survival::Surv(c(1, 2, 3), factor(c("censor", "death", "other"))),
  Surv_interval = survival::Surv(c(1, NA, 3), c(2, NA, 4), type = "interval2"),
  sfc = sf::st_sfc(sf::st_point(c(1, 2)), sf::st_point(), sf::st_point(c(3, 4))),
  s2 = s2::as_s2_geography(c("POINT (1 2)", NA, "POINT (3 4)")),
  xml_nodeset = xml2::xml_find_all(xml2::read_xml("<x><a/><b/><c/></x>"), "*"),
  vctrs_unspecified = vctrs::unspecified(3),
  asis_int = I(c(1L, NA_integer_, 3L)),
  asis_char = I(c("a", NA, "c")),
  list_language = list(quote(a), NULL, quote(b + c)),
  list_environment = list(new.env(parent = emptyenv()), NULL, new.env(parent = emptyenv()))
)
rows <- list()
add <- function(family, n, role, packed, sort, status, detail = "") {
  rows[[length(rows) + 1L]] <<- data.frame(
    family, n, role, packed, sort, status, detail, stringsAsFactors = FALSE
  )
}
for (family in names(families)) {
  for (n in c(0L, 1L, 3L)) {
    x <- capture(vctrs::vec_slice(families[[family]], seq_len(n)))
    if (inherits(x, "error")) {
      add(family, n, "fixture", FALSE, "none", "ordinary_fixture_error", conditionMessage(x))
      next
    }
    for (packed in c(FALSE, TRUE)) {
      key <- if (packed) tibble(inner = x) else x
      for (role in c("dimension", "fixed", "payload")) {
        dat <- capture(tibble(g = rep("a", n), key = key, row = seq_len(n)))
        if (inherits(dat, "error")) {
          add(family, n, role, packed, "none", "ordinary_fixture_error", conditionMessage(dat))
          next
        }
        original <- serialize(dat, NULL)
        grouping <- if (role == "dimension") rollup(key) else rollup(g)
        by <- if (role == "fixed") rlang::quo(key) else rlang::quo(NULL)
        dimension <- if (role == "dimension") "key" else "g"
        missing <- capture(vctrs::vec_init(dat[[dimension]], n))
        control <- capture({
          omitted <- mutate(dat, !!dimension := !!missing)
          bind_rows(mutate(dat, occurrence = 1L), mutate(omitted, occurrence = 2L))
        })
        for (sort in c("none", "first", "last")) {
          got <- capture(expand_with_margins(
            dat, .by = !!by, .grouping = grouping, .margin_label = NULL,
            .id = "occurrence", .sort = sort
          ))
          if (inherits(control, "error")) {
            add(family, n, role, packed, sort, "ordinary_control_error", conditionMessage(control))
            next
          }
          if (inherits(got, "error")) {
            ordering <- capture(arrange(dat, across(all_of(c(if (role == "fixed") "key", dimension)))))
            classification <- if (sort != "none" && inherits(ordering, "error")) {
              "ordinary_order_error"
            } else "package_error"
            add(family, n, role, packed, sort, classification, conditionMessage(got))
            next
          }
          got <- arrange(got, occurrence, row)
          control <- arrange(control, occurrence, row)
          comparison <- capture(all.equal(as.list(got)[names(control)], as.list(control), check.attributes = TRUE))
          if (isTRUE(comparison)) {
            add(family, n, role, packed, sort, "match")
          } else {
            add(family, n, role, packed, sort, "difference", paste(comparison, collapse = "; "))
          }
          stopifnot(identical(serialize(dat, NULL), original))
        }
      }
    }
  }
}
results <- bind_rows(rows)
write.csv(results, file.path(out_dir, "additional-types.csv"), row.names = FALSE)
print(table(results$status))
print(results[!results$status %in% c("match", "ordinary_order_error"), ], row.names = FALSE)
print(sessionInfo())
