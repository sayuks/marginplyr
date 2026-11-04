# Run from the package root. Exit nonzero on any disagreement with the oracle.
pkgload::load_all(".", quiet = TRUE, export_all = FALSE)

set.seed(94371)
dimensions <- c("z", "a,b", "line\nbreak", "a`b", "é")
data <- tibble::as_tibble(stats::setNames(rep(list(1L), length(dimensions)),
                                         dimensions))
data$fixed <- "partition"
node <- function(kind, children = list()) list(kind = kind, children = children)
columns <- function(cols) node("columns", as.list(cols))

make_case <- function(depth) {
  choose_cols <- function(minimum) {
    sample(dimensions, sample(seq.int(minimum, 3L), 1L))
  }
  kind <- if (depth == 0L) "set" else
    sample(c("set", "sets", "rollup", "cube", "product"), 1L)
  children <- switch(
    kind,
    set = list(columns(choose_cols(0L))),
    sets = replicate(sample(1:3, 1L), make_case(depth - 1L), simplify = FALSE),
    product = replicate(sample(0:2, 1L), make_case(depth - 1L), simplify = FALSE),
    rollup = ,
    cube = replicate(
      sample(1:4, 1L),
      if (sample(c(TRUE, FALSE), 1L)) {
        columns(choose_cols(1L)[1L])
      } else {
        node("set", list(columns(choose_cols(1L))))
      },
      simplify = FALSE
    )
  )
  node(kind, children)
}

encode <- function(case) {
  constructors <- c(set = "grouping_set", sets = "grouping_sets",
                    rollup = "rollup", cube = "cube", product = "grouping_spec")
  if (case$kind == "columns") {
    return(rlang::expr(tidyselect::all_of(!!as.character(case$children))))
  }
  rlang::call2(constructors[[case$kind]], !!!lapply(case$children, encode),
               .ns = "marginplyr")
}

enumerate <- function(case) {
  if (case$kind == "columns") return(list(as.character(case$children)))

  units <- lapply(case$children, enumerate)
  collapse_units <- function(selected) {
    unique(as.character(unlist(units[selected], use.names = FALSE)))
  }
  switch(
    case$kind,
    set = list(collapse_units(seq_along(units))),
    sets = do.call(c, units),
    rollup = lapply(rev(seq.int(0L, length(units))), function(size) {
      collapse_units(seq_len(size))
    }),
    cube = {
      # Enumerate Boolean vectors, then rank by descending subset size and
      # lexicographically by which earlier unit is included.
      choices <- as.matrix(expand.grid(rep(list(c(1L, 0L)), length(units))))
      selected <- do.call(order, c(list(-rowSums(choices)), as.list(-as.data.frame(choices))))
      lapply(selected, function(i) collapse_units(which(choices[i, ] == 1L)))
    },
    product = {
      if (length(units) == 0L) return(list(character()))
      # expand.grid advances its first coordinate fastest. Reverse the axes
      # and reverse the picked indices to obtain left-major order.
      tuples <- expand.grid(rev(lapply(units, seq_along)))
      lapply(seq_len(nrow(tuples)), function(i) {
        indices <- rev(as.integer(tuples[i, ]))
        parts <- Map(function(unit, index) unit[[index]], units, indices)
        unique(as.character(unlist(parts, use.names = FALSE)))
      })
    }
  )
}

expected_plan <- function(sets, drop) {
  dims <- unique(as.character(unlist(sets, use.names = FALSE)))
  bits <- lapply(sets, function(set) stats::setNames(as.integer(!dims %in% set), dims))
  indices <- if (drop) which(!duplicated(bits)) else seq_along(sets)
  bits <- bits[indices]
  if (!length(dims)) bits <- rep(list(integer()), length(indices))
  tibble::tibble(
    set_id = seq_along(indices),
    fixed = rep(list("fixed"), length(indices)),
    included = lapply(sets[indices], function(set) dims[dims %in% set]),
    omitted = lapply(sets[indices], function(set) dims[!dims %in% set]),
    grouping_bits = bits,
    grouping_id = vapply(bits, function(b) {
      if (!length(b)) return(0L)
      as.integer(sum(b * 2 ^ rev(seq_along(b) - 1L)))
    }, integer(1))
  )
}

cases <- c(list(node("product"), node("set"), node("sets", list(node("set"), node("product")))),
           lapply(rep(0:3, each = 150L), make_case))
n_duplicate <- 0L
n_occurrence <- 0L
for (i in seq_along(cases)) {
  case <- cases[[i]]
  spec <- eval(encode(case))
  sets <- enumerate(case)
  keep <- expected_plan(sets, FALSE)
  drop <- expected_plan(sets, TRUE)
  n_duplicate <- n_duplicate + as.integer(nrow(drop) < nrow(keep))
  n_occurrence <- n_occurrence + nrow(keep)
  for (policy in c("keep", "drop", "error")) {
    actual <- tryCatch(inspect_grouping(data, .by = fixed, .grouping = spec,
                                         .duplicates = policy, .format = "list"),
                       error = identity)
    agrees <- if (policy == "error" && nrow(drop) < nrow(keep)) {
      inherits(actual, "marginplyr_error") &&
        grepl("Duplicate grouping sets", conditionMessage(actual), fixed = TRUE)
    } else {
      identical(actual, if (policy == "drop") drop else keep)
    }
    if (!agrees) {
      print(encode(case))
      print(actual)
      print(if (policy == "drop") drop else keep)
      stop("Case ", i, ", policy ", policy, " differed from the oracle.")
    }
  }
}

# Check the documented signed-integer boundary independently.
for (width in c(1L, 30L, 31L, 32L, 65L)) {
  vars <- sprintf("dim_%02d", seq_len(width))
  wide <- tibble::as_tibble(stats::setNames(rep(list(1L), width), vars))
  spec <- grouping_sets(grouping_set(tidyselect::all_of(vars)), grouping_set())
  plan <- inspect_grouping(wide, .grouping = spec, .format = "list")
  ids <- if (width <= 31L) c(0L, as.integer(2^width - 1)) else rep(NA_integer_, 2L)
  stopifnot(
    identical(plan$grouping_id, ids),
    identical(plan$grouping_bits[[1L]], stats::setNames(rep(0L, width), vars)),
    identical(plan$grouping_bits[[2L]], stats::setNames(rep(1L, width), vars))
  )
}

cat(length(cases), "specifications x 3 policies;", n_occurrence,
    "occurrences;", n_duplicate, "specifications with duplicates.\n")
cat("Exact membership, order, identifiers and bits agreed. Widths 1, 30, 31, 32, 65 agreed.\n")
cat(R.version.string, R.version$platform, "\n")
for (package in c("dplyr", "rlang", "tidyselect", "vctrs")) {
  cat(package, as.character(utils::packageVersion(package)), "\n")
}
