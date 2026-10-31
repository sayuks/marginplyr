pkgload::load_all(quiet = TRUE)
set.seed(31103197)
env_a <- list2env(list(x = 1L), parent = emptyenv())
env_b <- list2env(list(x = 1L), parent = emptyenv())
closure <- function(x) function(y) x + y
fun_a <- closure(1L)
fun_b <- closure(2L)
formula_a <- stats::as.formula("x ~ y", env = env_a)
formula_b <- stats::as.formula("x ~ y", env = env_b)
fixtures <- list(
  environments = list(env_a, env_a, env_b, NULL),
  closures = list(fun_a, fun_a, fun_b, NULL),
  formulas = list(formula_a, formula_a, formula_b, NULL),
  pairlists = list(pairlist(x=1L,y=2L), pairlist(x=1L,y=2L), pairlist(y=2L,x=1L), NULL),
  expressions = list(expression(x+y), expression(x+y), expression(y+x), NULL),
  recursive = list(list(a=list(env_a, fun_a)), list(a=list(env_a, fun_a)), list(a=list(env_b, fun_b)), NULL)
)
count <- projections <- 0L
for (fixture in names(fixtures)) for (n in c(0L, 4L, 8L)) {
  input <- tibble::tibble(g = rep(fixtures[[fixture]], length.out = n),
                         h = rep(c("x", "x", "y", "y"), length.out = n),
                         row_id = seq_len(n))
  before <- serialize(input, NULL)
  for (verb in list(nest_with_margins, nest_by_with_margins)) for (keep in c(FALSE, TRUE)) {
    for (sort in c("none", "first", "last")) for (role in c("fixed", "dimension")) {
      if (role == "fixed") {
        result <- verb(input, .by = g, .grouping = rollup(h), .margin_label = NULL,
                       .keep = keep, .sort = sort, .id = "set")
        sets <- list(c("g", "h"), "g")
      } else {
        result <- verb(input, .grouping = rollup(g, h), .margin_label = NULL,
                       .keep = keep, .sort = sort, .id = "set")
        sets <- list(c("g", "h"), "g", character())
      }
      for (set in seq_along(sets)) {
        expected <- if (length(sets[[set]])) vctrs::vec_group_loc(input[sets[[set]]])$loc else {
          if (n) list(seq_len(n)) else list()
        }
        observed <- lapply(result$data[result$set == set], function(cell) cell$row_id)
        normalize <- function(groups) groups[order(vapply(groups, function(ids) ids[[1L]], integer(1)))]
        stopifnot(identical(normalize(expected), normalize(observed)))
      }
      if (keep) for (cell in result$data) {
        stopifnot(identical(cell$g, input$g[cell$row_id]), identical(cell$h, input$h[cell$row_id]))
        projections <- projections + 1L
      }
      stopifnot(identical(before, serialize(input, NULL)))
      count <- count + 1L
    }
  }
}
cat("calls", count, "kept_list_key_projections", projections, "source_mutations", 0L, "\n")
