# Dated exploration of derived metadata and lexical filter bindings.
args <- commandArgs(trailingOnly = TRUE)
if (length(args)) setwd(args[[1L]])
pkgload::load_all('.', quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
counts <- c(backend_control_refusals = 0L, typed_comparisons = 0L, matched = 0L, documented_refusals = 0L,
            mismatches = 0L, lexical_nesting = 0L, premature_calls = 0L)
observe <- function(x) tryCatch(force(x), error = identity)
vectors <- list(
  integer = c(1L, 2L, NA_integer_), double = c(1, 2, NA_real_),
  factor = addNA(factor(c('a', 'b', NA), levels = c('z', 'b', 'a'))),
  ordered = ordered(c('a', 'b', NA), levels = c('b', 'a', 'z')),
  date = as.Date(c('2026-10-27', '2026-10-28', NA_character_)),
  zoned_time = as.POSIXct(c('2026-10-27', '2026-10-28', NA_character_), tz = 'America/New_York'),
  difftime = as.difftime(c(1, 2, NA), units = 'hours'),
  integer64 = bit64::as.integer64(c('9007199254740992', '9007199254740993', NA_character_)),
  hms = hms::as_hms(c(1, 2, NA)), AsIs = I(c(1, 2, NA_real_)),
  list = list(1L, 'a', NULL), raw = as.raw(c(0, 1, 2)),
  complex = c(1i, 2i, NA_complex_)
)
# Compare proxy prototypes with explicit source collection after safe graph
# composition. Typed refusals for classed join roots are intentional; they are
# recorded separately from genuine ptype mismatches.
for (type in names(vectors)) for (size in c(0L, 3L)) {
  input <- tibble(g = rep(c('a', 'b', 'a'), length.out = size), value = seq_len(size))
  input$payload <- vectors[[type]][seq_len(size)]
  root <- dtplyr::lazy_dt(input)
  cache <- compute(root)
  derivations <- list(
    root = root,
    rename = rename(root, renamed = payload),
    select = select(root, renamed = payload, g, value),
    relocate_cached = relocate(cache, payload),
    arranged_cache = arrange(compute(filter(root, TRUE)), value),
    copy = mutate(root, copied = payload),
    left_join = left_join(cache, cache, by = c('g', 'value')),
    union_all = union_all(cache, cache)
  )
  for (label in names(derivations)) {
    q <- derivations[[label]]
    actual_input <- observe(collect(q))
    if (inherits(actual_input, 'error')) {
      counts[['backend_control_refusals']] <- counts[['backend_control_refusals']] + 1L
      next
    }
    proxy <- observe(getFromNamespace('dtplyr_selection_proxy', 'marginplyr')(q))
    counts[['typed_comparisons']] <- counts[['typed_comparisons']] + 1L
    if (inherits(proxy, 'marginplyr_error')) {
      counts[['documented_refusals']] <- counts[['documented_refusals']] + 1L
      next
    }
    same <- !inherits(proxy, 'error') && identical(
      lapply(proxy, vctrs::vec_ptype), lapply(actual_input, vctrs::vec_ptype)
    )
    if (same) counts[['matched']] <- counts[['matched']] + 1L else {
      counts[['mismatches']] <- counts[['mismatches']] + 1L
      cat('PTYPE DIFF', type, size, label, '\n')
      if (inherits(proxy, 'error')) cat(conditionMessage(proxy), '\n')
      else print(all.equal(lapply(proxy, vctrs::vec_ptype), lapply(actual_input, vctrs::vec_ptype)))
    }
  }
}
# Lexical .BY is deliberately present without a same-named source column.
# Values come from the caller binding in the upstream predicate, while the
# Margin operation creates aliases only downstream of that predicate.
for (name in c('.BY', '.SD', '.I', '.N', '.GRP', '.ROW', '.xSD', '.iSD')) {
  local({
    calls <- integer()
    assign(name, 1L, envir = environment())
    callback <- eval(rlang::expr(function(x) {
      calls <<- c(calls, length(x))
      x > !!rlang::sym(name)
    }), envir = environment())
    input <- tibble(g = c('a', 'b', 'a'), value = 1:3)
    names(input)[[1L]] <- name
    predicate <- rlang::expr(callback(value))
    source <- filter(dtplyr::lazy_dt(input), !!predicate) |>
      compute() |>
      arrange(value)
    explicit <- collect(source)
    for (verb in list(nest_with_margins, nest_by_with_margins)) for (keep in c(FALSE, TRUE)) {
      run <- function(x) verb(x, .grouping = rollup(all_of(name)), .keep = keep,
                              .margin_label = NULL, .id = 'set', .sort = 'last')
      calls <- integer()
      actual <- run(source)
      if (identical(verb, nest_with_margins)) {
        if (length(calls)) counts[['premature_calls']] <<- counts[['premature_calls']] + 1L
        actual <- collect(actual)
      }
      expected <- run(explicit)
      normalize <- function(x) {
        z <- as.list(ungroup(x)); z$data <- lapply(z$data, as.list); z
      }
      counts[['lexical_nesting']] <<- counts[['lexical_nesting']] + 1L
      if (!identical(normalize(actual), normalize(expected))) {
        counts[['mismatches']] <<- counts[['mismatches']] + 1L
        cat('LEXICAL DIFF', name, keep, '\n')
      }
    }
  })
}
print(counts)
