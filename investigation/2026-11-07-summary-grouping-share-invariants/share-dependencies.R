Sys.setenv(TESTTHAT_PARALLEL = "false")
args <- commandArgs(trailingOnly = TRUE)
mode <- if (length(args)) args[[1L]] else "observation"
family_filter <- if (length(args) > 1L) args[[2L]] else "all"
stopifnot(mode %in% c("observation", "control", "contract"))
pkgload::load_all(".", quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))

fixtures <- list(
  data.frame = data.frame(g = c("a", "b"), x = c(1, 3), prior = c(10, 20)),
  tibble = tibble::tibble(g = c("a", "b"), x = c(1, 3), prior = c(10, 20)),
  data.table = data.table::data.table(
    g = c("a", "b"), x = c(1, 3), prior = c(10, 20)
  )
)

case <- function(family, body, expected = "refused", outcome = expected) {
  list(family = family, body = body, expected = expected, outcome = outcome)
}

cases <- list(
  nested_member = case("nested", '{
    a <- list(outer = list(k = function() 7))
    a$outer$k <- function() sum(prior)
    rm(prior)
    a$outer$k()
  }', outcome = "bypass"),
  nested_member_reverse = case("nested", '{
    a <- list(outer = list(k = function() sum(prior)))
    a$outer$k <- function() 7
    rm(prior)
    a$outer$k()
  }', "seven", "refused"),
  nested_recursive = case("nested", '{
    a <- list(outer = list(k = function() 7))
    a[[c("outer", "k")]] <- function() sum(prior)
    rm(prior)
    a$outer$k()
  }', outcome = "bypass"),
  nested_delete = case("nested", '{
    a <- list(outer = list(function() 7, function() sum(prior)))
    a$outer[[1L]] <- NULL
    rm(prior)
    a$outer[[1L]]()
  }', outcome = "bypass"),
  nested_names = case("nested", '{
    a <- list(outer = list(k = function() sum(prior)))
    names(a$outer) <- "other"
    rm(prior)
    a$outer$other()
  }', outcome = "bypass"),
  nested_names_reverse = case("nested", '{
    a <- list(outer = list(k = function() sum(prior), other = function() 7))
    names(a$outer) <- c("other", "k")
    rm(prior)
    a$outer$k()
  }', "seven", "refused"),
  nested_direct = case("nested", '{
    a <- list(outer = list(k = function() sum(prior)))
    rm(prior)
    a$outer$k()
  }'),
  flat_write = case("nested", '{
    a <- list(k = function() 7)
    a$k <- function() sum(prior)
    rm(prior)
    a$k()
  }'),
  flat_names = case("nested", '{
    a <- list(k = function() sum(prior))
    names(a) <- "other"
    rm(prior)
    a$other()
  }'),
  nested_retained = case("nested", '{
    a <- list(outer = list(k = function() 7))
    a$outer$k <- function() sum(prior)
    a$outer$k()
  }', "one"),
  dispatch_safe = case("dispatch", '{
    `$<-` <- function(x, name, value) x
    a <- list(k = function() 7)
    a$k <- function() sum(prior)
    rm(prior)
    a$k()
  }', "seven", "refused"),
  dispatch_unsafe = case("dispatch", '{
    `$<-` <- function(x, name, value) x
    a <- list(k = function() sum(prior))
    a$k <- function() 7
    rm(prior)
    a$k()
  }', outcome = "bypass"),
  dispatch_bracket = case("dispatch", '{
    `[<-` <- function(x, i, value) x
    a <- list(k = function() sum(prior))
    a[1L] <- list(function() 7)
    rm(prior)
    a$k()
  }', outcome = "bypass"),
  dispatch_double = case("dispatch", '{
    `[[<-` <- function(x, i, value) x
    a <- list(k = function() 7)
    a[[1L]] <- function() sum(prior)
    rm(prior)
    a$k()
  }', "seven", "refused"),
  base_dependent = case("dispatch", '{
    a <- list(k = function() 7)
    a$k <- function() sum(prior)
    rm(prior)
    a$k()
  }'),
  base_independent = case("dispatch", '{
    a <- list(k = function() sum(prior))
    a$k <- function() 7
    rm(prior)
    a$k()
  }', "seven"),
  formal_actual = case("formal", '{
    a <- list(k = function() sum(prior))
    rm(prior)
    (function(y) y$k())(a)
  }', outcome = "bypass"),
  formal_forwarded = case("formal", '{
    a <- list(k = function() sum(prior))
    rm(prior)
    (function(y) (function(z) z$k())(y))(a)
  }', outcome = "bypass"),
  formal_copy = case("formal", '{
    a <- list(k = function() sum(prior))
    rm(prior)
    (function(y) { copy <- y; copy$k() })(a)
  }', outcome = "bypass"),
  formal_direct = case("formal", '{
    a <- list(k = function() sum(prior))
    rm(prior)
    a$k()
  }'),
  formal_default = case("formal", '{
    rm(prior)
    (function(y = list(k = function() sum(prior))) y$k())()
  }'),
  formal_retained = case("formal", '{
    a <- list(k = function() sum(prior))
    (function(y) y$k())(a)
  }', "one"),
  factory_list = case("factory", '{
    a <- (function() list(k = function() sum(prior)))()
    rm(prior)
    a$k()
  }', outcome = "bypass"),
  factory_return = case("factory", '{
    a <- (function() {
      k <- function() sum(prior)
      return(list(k = k))
    })()
    rm(prior)
    a$k()
  }', outcome = "bypass"),
  factory_direct = case("factory", '{
    a <- list(k = (function() function() sum(prior))())
    rm(prior)
    a$k()
  }'),
  factory_retained = case("factory", '{
    a <- (function() list(k = function() sum(prior)))()
    a$k()
  }', "one"),
  subset_negative = case("indices", '{
    a <- list(k = function() sum(prior), safe = function() 7)
    a <- a[-2L]
    rm(prior)
    a$k()
  }', outcome = "bypass"),
  subset_concat = case("indices", '{
    a <- list(k = function() sum(prior))
    a <- a[c(1L)]
    rm(prior)
    a$k()
  }', outcome = "bypass"),
  subset_drop = case("indices", '{
    a <- list(k = function() sum(prior))
    a <- a[1L, drop = FALSE]
    rm(prior)
    a$k()
  }', outcome = "bypass"),
  member_true = case("indices", '{
    a <- list(k = function() sum(prior))
    rm(prior)
    a[[TRUE]]()
  }', outcome = "bypass"),
  member_minus = case("indices", '{
    a <- list(k = function() sum(prior), safe = function() 7)
    rm(prior)
    a[[-2L]]()
  }', outcome = "bypass"),
  subset_literal = case("indices", '{
    a <- list(k = function() sum(prior), safe = function() 7)
    a <- a[1L]
    rm(prior)
    a$k()
  }'),
  member_literal = case("indices", '{
    a <- list(k = function() sum(prior), safe = function() 7)
    rm(prior)
    a[[1L]]()
  }'),
  subset_independent = case("indices", '{
    a <- list(k = function() sum(prior), safe = function() 7)
    a <- a[-1L]
    rm(prior)
    a$safe()
  }', "seven")
)

collect_case <- function(data, read, direction, kind) {
  source <- if (direction == "forward") quote(value) else quote(observed)
  helper <- rlang::call2(paste0("share_of_", kind), source)
  if (direction == "forward") {
    summarize_with_margins(
      data, value = sum(x), prior = !!helper, observed = !!read,
      .grouping = rollup(g), .sort = "last"
    )
  } else {
    summarize_with_margins(
      data, prior = sum(x), observed = !!read, ratio = !!helper,
      .grouping = rollup(g), .sort = "last"
    )
  }
}

classify <- function(outcome, direction, kind) {
  if (inherits(outcome, "error")) {
    fragment <- if (direction == "forward") {
      paste("Ordinary summaries cannot use an earlier",
            tools::toTitleCase(kind), "share")
    } else {
      "depends on earlier summary alias"
    }
    if (inherits(outcome, "marginplyr_error") &&
          grepl(fragment, conditionMessage(outcome), fixed = TRUE)) {
      return("refused")
    }
    return(paste(class(outcome)[[1L]], conditionMessage(outcome)))
  }
  observed <- outcome$observed
  if (length(observed) != 3L) return("unexpected cardinality")
  if (all(observed == 1)) return("one")
  if (all(observed == 7)) return("seven")
  if (direction == "forward" &&
        identical(observed, c(10, 20, 30)) &&
        identical(outcome$prior, c(0.25, 0.75, 1))) return("bypass")
  if (direction == "reverse" &&
        identical(observed, c(1, 3, 4)) &&
        identical(outcome$ratio, c(0.25, 0.75, 1))) return("bypass")
  paste("unexpected", paste(observed, collapse = ","))
}

rows <- list()
for (name in names(cases)) {
  entry <- cases[[name]]
  if (family_filter != "all" && family_filter != entry$family) next
  if (mode == "control" && entry$outcome != entry$expected) next
  read <- rlang::parse_expr(paste0("(function(prior) ", entry$body, ")(1)"))
  for (class in names(fixtures)) {
    data <- fixtures[[class]]
    before <- serialize(data, NULL)
    for (kind in c("parent", "total")) for (direction in c("forward", "reverse")) {
      observed <- classify(tryCatch(
        collect_case(data, read, direction, kind), error = identity
      ), direction, kind)
      stopifnot(identical(serialize(data, NULL), before))
      expected <- if (mode == "observation") entry$outcome else entry$expected
      rows[[length(rows) + 1L]] <- data.frame(
        case = name, family = entry$family, class = class, kind = kind,
        direction = direction, observed = observed, expected = expected,
        match = observed == expected
      )
    }
  }
}
results <- do.call(rbind, rows)
print(with(results, table(family, observed)))
mismatches <- unique(results[!results$match, c("case", "observed", "expected")])
if (nrow(mismatches)) print(mismatches, row.names = FALSE)
cat(mode, nrow(results), "comparisons;", sum(!results$match), "mismatches\n")
stopifnot(nrow(mismatches) == 0L)
