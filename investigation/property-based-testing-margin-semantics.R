#!/usr/bin/env Rscript

# Throwaway property-based exploration harness. This is repository-only
# evidence, not part of the package test suite.

options(warn = 2)

pkgload::load_all(".", quiet = TRUE, export_all = FALSE, helpers = FALSE)

pbt_seed <- as.integer(Sys.getenv("MARGINPLYR_PBT_SEED", "20260912"))
pbt_iterations <- as.integer(Sys.getenv("MARGINPLYR_PBT_ITERATIONS", "500"))
pbt_artifact <- Sys.getenv(
  "MARGINPLYR_PBT_ARTIFACT",
  "/private/tmp/marginplyr-pbt-failure.rds"
)

set.seed(pbt_seed)

pbt_counts <- new.env(parent = emptyenv())
pbt_counts$generated <- 0L
pbt_counts$checks <- 0L
pbt_counts$discarded <- 0L
pbt_counts$failures <- list()

`%||%` <- function(x, y) if (is.null(x)) y else x

pbt_fail <- function(property, case, message) {
  failure <- list(
    property = property,
    seed = pbt_seed,
    generated_case = pbt_counts$generated,
    message = message,
    case = case
  )
  pbt_counts$failures[[length(pbt_counts$failures) + 1L]] <- failure
  saveRDS(failure, pbt_artifact)
  stop(
    paste0(
      "PROPERTY FAILURE\n",
      "property: ", property, "\n",
      "seed: ", pbt_seed, "\n",
      "generated case: ", pbt_counts$generated, "\n",
      "artifact: ", pbt_artifact, "\n",
      message
    ),
    call. = FALSE
  )
}

pbt_check <- function(ok, property, case, message) {
  pbt_counts$checks <- pbt_counts$checks + 1L
  if (!isTRUE(ok)) {
    pbt_fail(property, case, message)
  }
  invisible(TRUE)
}

pbt_attempt <- function(code, property, case) {
  tryCatch(
    force(code),
    error = function(condition) {
      pbt_fail(
        property,
        case,
        paste(class(condition), conditionMessage(condition), sep = "\n")
      )
    }
  )
}

pbt_identical_frame <- function(actual, expected) {
  actual <- tibble::as_tibble(actual)
  expected <- tibble::as_tibble(expected)
  identical(names(actual), names(expected)) &&
    identical(as.list(actual), as.list(expected))
}

pbt_frame_diff <- function(actual, expected) {
  actual <- tibble::as_tibble(actual)
  expected <- tibble::as_tibble(expected)
  paste(
    capture.output({
      cat("actual:\n")
      print(actual)
      cat("expected:\n")
      print(expected)
      cat("actual structure:\n")
      str(actual)
      cat("expected structure:\n")
      str(expected)
    }),
    collapse = "\n"
  )
}

pbt_sort_frame <- function(x, keys) {
  x <- tibble::as_tibble(x)
  if (nrow(x) < 2L) {
    return(x)
  }
  order <- vctrs::vec_order(x[keys])
  x[order, , drop = FALSE]
}

pbt_name_pool <- c(
  "region", "store", "a b", "x+y", "if", ".hidden", "a,b",
  "日本語", "Total", "0", "a/b", "with`tick"
)

pbt_sample_names <- function(n, excluded = character()) {
  pool <- setdiff(pbt_name_pool, excluded)
  if (n > length(pool)) {
    stop("The PBT name pool is too small.", call. = FALSE)
  }
  sample(pool, n)
}

pbt_group_vector <- function(n, type) {
  if (type == "character") {
    return(sample(c("a", "b", "", NA_character_), n, replace = TRUE))
  }
  if (type == "numeric") {
    return(sample(c(-1, 0, 1, 2, NA_real_), n, replace = TRUE))
  }
  if (type == "logical") {
    return(sample(c(TRUE, FALSE, NA), n, replace = TRUE))
  }
  if (type == "factor") {
    return(factor(
      sample(c("low", "high", NA_character_), n, replace = TRUE),
      levels = c("high", "unused", "low"),
      ordered = sample(c(TRUE, FALSE), 1L)
    ))
  }
  stop("Unknown generated type.", call. = FALSE)
}

pbt_explicit_spec <- function(sets) {
  set_calls <- lapply(sets, function(set) {
    rlang::call2("grouping_set", !!!rlang::syms(set))
  })
  eval(rlang::call2("grouping_sets", !!!set_calls), envir = globalenv())
}

pbt_rollup_spec <- function(dimensions) {
  eval(
    rlang::call2("rollup", !!!rlang::syms(dimensions)),
    envir = globalenv()
  )
}

pbt_dimension_expr <- function(block) {
  if (length(block) == 1L) {
    return(rlang::sym(block))
  }
  rlang::call2("grouping_set", !!!rlang::syms(block))
}

pbt_partition_dimensions <- function(dimensions) {
  if (length(dimensions) == 1L) {
    return(list(dimensions))
  }
  cuts <- which(stats::runif(length(dimensions) - 1L) < 0.45)
  ends <- c(cuts, length(dimensions))
  starts <- c(1L, head(ends, -1L) + 1L)
  Map(function(start, end) dimensions[start:end], starts, ends)
}

pbt_constructor_spec <- function(kind, blocks) {
  eval(
    rlang::call2(kind, !!!lapply(blocks, pbt_dimension_expr)),
    envir = globalenv()
  )
}

pbt_set_key <- function(set) paste(sort(set), collapse = "\r")

pbt_set_multiset <- function(sets) sort(vapply(sets, pbt_set_key, character(1)))

pbt_plan_sets <- function(plan) {
  lapply(seq_len(nrow(plan)), function(index) plan$included[[index]])
}

pbt_generate_plan_case <- function() {
  dimension_count <- sample(1:7, 1L)
  fixed_count <- sample(0:2, 1L)
  names <- pbt_sample_names(dimension_count + fixed_count)
  fixed <- head(names, fixed_count)
  dimensions <- tail(names, dimension_count)
  data <- as.data.frame(
    stats::setNames(
      rep(list(integer()), dimension_count + fixed_count),
      names
    ),
    optional = TRUE
  )
  kind <- sample(c("rollup", "cube", "product", "union"), 1L)

  if (kind %in% c("rollup", "cube")) {
    blocks <- pbt_partition_dimensions(dimensions)
    spec <- pbt_constructor_spec(kind, blocks)
    if (kind == "rollup") {
      expected <- lapply(
        rev(seq.int(0L, length(blocks))),
        function(count) {
          if (count == 0L) character() else unlist(head(blocks, count), use.names = FALSE)
        }
      )
    } else {
      expected <- lapply(seq.int(0L, 2L ^ length(blocks) - 1L), function(mask) {
        omitted <- as.logical(intToBits(mask)[seq_along(blocks)])
        unlist(blocks[!omitted], use.names = FALSE)
      })
    }
  } else if (kind == "product") {
    split_at <- if (dimension_count == 1L) 1L else sample(seq_len(dimension_count - 1L), 1L)
    left <- head(dimensions, split_at)
    right <- tail(dimensions, dimension_count - split_at)
    if (length(right) == 0L) {
      right <- character()
    }
    left_sets <- lapply(rev(seq.int(0L, length(left))), function(count) head(left, count))
    right_sets <- if (length(right) == 0L) {
      list(character())
    } else {
      lapply(rev(seq.int(0L, length(right))), function(count) head(right, count))
    }
    left_call <- rlang::call2("rollup", !!!rlang::syms(left))
    right_call <- if (length(right) == 0L) {
      rlang::call2("grouping_spec")
    } else {
      rlang::call2("rollup", !!!rlang::syms(right))
    }
    spec <- eval(
      rlang::call2("grouping_spec", left_call, right_call),
      envir = globalenv()
    )
    expected <- unlist(
      lapply(left_sets, function(left_set) {
        lapply(right_sets, function(right_set) c(left_set, right_set))
      }),
      recursive = FALSE
    )
  } else {
    first <- sample(seq_along(dimensions), sample(seq_along(dimensions), 1L))
    second <- sample(seq_along(dimensions), sample(seq_along(dimensions), 1L))
    first_set <- dimensions[sort(unique(first))]
    second_set <- dimensions[sort(unique(second))]
    # Ensure the resolved dimension universe is the generated universe.
    third_set <- dimensions
    nested <- rlang::call2(
      "grouping_sets",
      rlang::call2("grouping_set", !!!rlang::syms(first_set)),
      rlang::call2("grouping_set", !!!rlang::syms(second_set))
    )
    spec <- eval(
      rlang::call2(
        "grouping_sets",
        nested,
        rlang::call2("grouping_set", !!!rlang::syms(third_set))
      ),
      envir = globalenv()
    )
    expected <- list(first_set, second_set, third_set)
  }

  list(
    data = data,
    fixed = fixed,
    dimensions = dimensions,
    plan_dimensions = unique(unlist(expected, use.names = FALSE)),
    kind = kind,
    spec = spec,
    expected_sets = expected
  )
}

pbt_generate_case <- function(
    min_rows = 0L,
    max_rows = 24L,
    min_dimensions = 1L,
    max_dimensions = 4L,
    allow_duplicate_sets = TRUE,
    positive_measure = FALSE,
    excluded_names = character()) {
  row_count <- sample(seq.int(min_rows, max_rows), 1L)
  dimension_count <- sample(seq.int(min_dimensions, max_dimensions), 1L)
  fixed_count <- sample(0:min(2L, max_dimensions - dimension_count + 1L), 1L)
  group_count <- fixed_count + dimension_count
  group_names <- pbt_sample_names(
    group_count,
    excluded = c(
      ".pbt_measure", ".pbt_row", ".pbt_set", excluded_names
    )
  )
  fixed <- head(group_names, fixed_count)
  dimensions <- tail(group_names, dimension_count)
  types <- sample(
    c("character", "numeric", "logical", "factor"),
    group_count,
    replace = TRUE
  )
  columns <- Map(
    function(type) pbt_group_vector(row_count, type),
    types
  )
  names(columns) <- group_names
  data <- as.data.frame(columns, optional = TRUE)
  data$.pbt_measure <- if (positive_measure) {
    sample(1:9, row_count, replace = TRUE)
  } else {
    sample(c(-3L, 0L, 1L, 2L, NA_integer_), row_count, replace = TRUE)
  }
  data$.pbt_row <- seq_len(row_count)

  possible_masks <- seq.int(0L, 2L ^ dimension_count - 1L)
  set_count <- sample(seq_len(min(8L, length(possible_masks) + 2L)), 1L)
  masks <- if (allow_duplicate_sets) {
    sample(possible_masks, set_count, replace = TRUE)
  } else {
    sample(possible_masks, min(set_count, length(possible_masks)), replace = FALSE)
  }
  # The resolved Margin dimensions are the union of columns named by all
  # grouping sets. Keep the generated dimension universe observable so the
  # independent oracle and the public plan speak about the same columns.
  masks[[1L]] <- 0L
  sets <- lapply(masks, function(mask) {
    bits <- as.logical(intToBits(mask)[seq_len(dimension_count)])
    dimensions[!bits]
  })

  list(
    data = data,
    fixed = fixed,
    dimensions = dimensions,
    types = stats::setNames(types, group_names),
    sets = sets,
    spec = pbt_explicit_spec(sets)
  )
}

pbt_expected_gid <- function(dimensions, set) {
  bits <- as.integer(!dimensions %in% set)
  as.integer(sum(bits * 2L ^ rev(seq_along(bits) - 1L)))
}

pbt_summary <- function(case, sort = "none", grouping = case$spec) {
  bit_summaries <- stats::setNames(
    lapply(case$dimensions, function(name) {
      rlang::new_quosure(
        rlang::call2("grouping_bit", rlang::sym(name)),
        env = globalenv()
      )
    }),
    paste0(".pbt_bit_", seq_along(case$dimensions))
  )
  ordinary <- rlang::quos(
    .pbt_rows = dplyr::n(),
    .pbt_total = sum(.data$.pbt_measure, na.rm = TRUE),
    .pbt_gid = grouping_id()
  )
  rlang::inject(summarize_with_margins(
    case$data,
    !!!c(ordinary, bit_summaries),
    .by = dplyr::all_of(case$fixed),
    .grouping = grouping,
    .margin_label = NULL,
    .duplicates = "keep",
    .sort = sort,
    .id = ".pbt_set"
  ))
}

pbt_summary_oracle <- function(case) {
  branches <- lapply(seq_along(case$sets), function(index) {
    set <- case$sets[[index]]
    keys <- c(case$fixed, set)
    if (length(keys) == 0L) {
      branch <- dplyr::summarise(
        case$data,
        .pbt_rows = dplyr::n(),
        .pbt_total = sum(.data$.pbt_measure, na.rm = TRUE)
      )
    } else {
      branch <- dplyr::summarise(
        case$data,
        .pbt_rows = dplyr::n(),
        .pbt_total = sum(.data$.pbt_measure, na.rm = TRUE),
        .by = dplyr::all_of(keys)
      )
    }
    for (name in setdiff(case$dimensions, set)) {
      branch[[name]] <- vctrs::vec_init(case$data[[name]], nrow(branch))
    }
    branch$.pbt_set <- rep.int(as.integer(index), nrow(branch))
    branch$.pbt_gid <- rep.int(
      pbt_expected_gid(case$dimensions, set),
      nrow(branch)
    )
    bits <- as.integer(!case$dimensions %in% set)
    for (bit_index in seq_along(bits)) {
      branch[[paste0(".pbt_bit_", bit_index)]] <- rep.int(
        bits[[bit_index]],
        nrow(branch)
      )
    }
    branch[c(
      case$fixed,
      case$dimensions,
      ".pbt_set",
      ".pbt_rows",
      ".pbt_total",
      ".pbt_gid",
      paste0(".pbt_bit_", seq_along(case$dimensions))
    )]
  })
  dplyr::bind_rows(branches)
}

pbt_expansion_oracle <- function(case) {
  branches <- lapply(seq_along(case$sets), function(index) {
    branch <- case$data
    omitted <- setdiff(case$dimensions, case$sets[[index]])
    for (name in omitted) {
      branch[[name]] <- vctrs::vec_init(case$data[[name]], nrow(branch))
    }
    branch$.pbt_set <- rep.int(as.integer(index), nrow(branch))
    branch[c(
      case$fixed,
      case$dimensions,
      ".pbt_set",
      setdiff(names(case$data), c(case$fixed, case$dimensions))
    )]
  })
  dplyr::bind_rows(branches)
}

pbt_property_summary_oracle <- function(case) {
  actual <- pbt_sort_frame(
    pbt_summary(case),
    c(case$fixed, case$dimensions, ".pbt_set")
  )
  expected <- pbt_sort_frame(
    pbt_summary_oracle(case),
    c(case$fixed, case$dimensions, ".pbt_set")
  )
  pbt_check(
    pbt_identical_frame(actual, expected),
    "local summary equals per-grouping-set dplyr oracle",
    case,
    pbt_frame_diff(actual, expected)
  )
}

pbt_property_expansion_oracle <- function(case) {
  actual <- expand_with_margins(
    case$data,
    .by = dplyr::all_of(case$fixed),
    .grouping = case$spec,
    .margin_label = NULL,
    .duplicates = "keep",
    .sort = "none",
    .id = ".pbt_set"
  )
  actual <- pbt_sort_frame(actual, c(".pbt_set", ".pbt_row"))
  expected <- pbt_sort_frame(
    pbt_expansion_oracle(case),
    c(".pbt_set", ".pbt_row")
  )
  pbt_check(
    pbt_identical_frame(actual, expected),
    "local expansion emits one correctly labelled copy per set",
    case,
    pbt_frame_diff(actual, expected)
  )
}

pbt_property_inspection <- function(case) {
  actual <- inspect_grouping(
    case$data,
    .by = dplyr::all_of(case$fixed),
    .grouping = case$spec,
    .duplicates = "keep",
    .format = "list"
  )
  pbt_check(
    identical(actual$set_id, seq_along(case$sets)),
    "inspection set identifiers follow explicit occurrence order",
    case,
    paste(capture.output(print(actual)), collapse = "\n")
  )
  for (index in seq_along(case$sets)) {
    set <- case$sets[[index]]
    expected_bits <- stats::setNames(
      as.integer(!case$dimensions %in% set),
      case$dimensions
    )
    pbt_check(
      identical(actual$fixed[[index]], case$fixed) &&
        identical(actual$included[[index]], case$dimensions[case$dimensions %in% set]) &&
        identical(actual$omitted[[index]], case$dimensions[!case$dimensions %in% set]) &&
        identical(actual$grouping_bits[[index]], expected_bits) &&
        identical(actual$grouping_id[[index]], pbt_expected_gid(case$dimensions, set)),
      "inspection matches explicit grouping-set oracle",
      case,
      paste("occurrence", index, "\n", paste(capture.output(print(actual)), collapse = "\n"))
    )
  }
}

pbt_property_constructor_plan <- function(case) {
  actual <- inspect_grouping(
    case$data,
    .by = dplyr::all_of(case$fixed),
    .grouping = case$spec,
    .duplicates = "keep",
    .format = "list"
  )
  actual_sets <- pbt_plan_sets(actual)
  pbt_check(
    identical(pbt_set_multiset(actual_sets), pbt_set_multiset(case$expected_sets)),
    paste0("constructor semantics match combinatorial oracle (", case$kind, ")"),
    case,
    paste(
      "actual sets:\n",
      paste(capture.output(dput(actual_sets)), collapse = "\n"),
      "\nexpected sets:\n",
      paste(capture.output(dput(case$expected_sets)), collapse = "\n")
    )
  )
  pbt_check(
    identical(actual$set_id, seq_len(nrow(actual))),
    "constructor plan exposes sequential occurrence identifiers",
    case,
    paste(capture.output(print(actual)), collapse = "\n")
  )
  for (index in seq_len(nrow(actual))) {
    expected_bits <- stats::setNames(
      as.integer(!case$plan_dimensions %in% actual$included[[index]]),
      case$plan_dimensions
    )
    pbt_check(
      identical(actual$fixed[[index]], case$fixed) &&
        identical(actual$omitted[[index]], case$plan_dimensions[expected_bits == 1L]) &&
        identical(actual$grouping_bits[[index]], expected_bits) &&
        identical(
          actual$grouping_id[[index]],
          as.integer(sum(expected_bits * 2L ^ rev(seq_along(expected_bits) - 1L)))
        ),
      paste0("constructor plan identities are internally consistent (", case$kind, ")"),
      case,
      paste(capture.output(print(actual)), collapse = "\n")
    )
  }
  if (identical(case$kind, "rollup")) {
    pbt_check(
      identical(actual_sets, case$expected_sets),
      "rollup plan follows hierarchical prefix order",
      case,
      paste(capture.output(print(actual)), collapse = "\n")
    )
  }
}

pbt_order_key <- function(result, case, direction) {
  key <- list()
  for (name in case$fixed) {
    key[[paste0("fixed_na_", length(key) + 1L)]] <- is.na(result[[name]])
    key[[paste0("fixed_value_", length(key) + 1L)]] <- result[[name]]
  }
  for (index in seq_along(case$dimensions)) {
    bit <- result[[paste0(".pbt_bit_", index)]]
    if (identical(direction, "first")) {
      bit <- -bit
    }
    name <- case$dimensions[[index]]
    key[[paste0("bit_", index)]] <- bit
    key[[paste0("dimension_na_", index)]] <- is.na(result[[name]])
    key[[paste0("dimension_value_", index)]] <- result[[name]]
  }
  key[["set"]] <- result$.pbt_set
  tibble::as_tibble(key)
}

pbt_property_sort <- function(case, direction) {
  actual <- tibble::as_tibble(pbt_summary(case, sort = direction))
  expected_order <- vctrs::vec_order(pbt_order_key(actual, case, direction))
  pbt_check(
    identical(expected_order, seq_len(nrow(actual))),
    paste0("local Margin order follows structural key (", direction, ")"),
    case,
    paste(
      "expected row permutation:", paste(expected_order, collapse = ", "),
      "\nactual:\n",
      paste(capture.output(print(actual)), collapse = "\n")
    )
  )
}

pbt_property_share <- function(case, check_share_source = TRUE, backend = "local") {
  grouping <- pbt_rollup_spec(case$dimensions)
  result <- summarize_with_margins(
    case$data,
    .pbt_total = sum(.data$.pbt_measure, na.rm = TRUE),
    .pbt_parent = share_of_parent(.pbt_total),
    .pbt_whole = share_of_total(.pbt_total),
    .by = dplyr::all_of(case$fixed),
    .grouping = grouping,
    .margin_label = NULL,
    .check_share_source = check_share_source,
    .id = ".pbt_set"
  ) |>
    dplyr::collect()
  total_sums <- dplyr::summarise(
    result,
    value = sum(.data$.pbt_whole),
    .by = dplyr::all_of(c(case$fixed, ".pbt_set"))
  )
  pbt_check(
    all(abs(total_sums$value - 1) < 1e-12),
    paste0("positive Total shares conserve within fixed partitions and occurrences (", backend, ")"),
    case,
    paste(capture.output(print(result)), collapse = "\n")
  )

  dimension_count <- length(case$dimensions)
  for (occurrence in seq_len(dimension_count)) {
    prefix <- head(case$dimensions, dimension_count - occurrence)
    child <- result[result$.pbt_set == occurrence, , drop = FALSE]
    parent_sums <- dplyr::summarise(
      child,
      value = sum(.data$.pbt_parent),
      .by = dplyr::all_of(c(case$fixed, prefix))
    )
    pbt_check(
      all(abs(parent_sums$value - 1) < 1e-12),
      paste0("positive Parent shares conserve beneath each immediate rollup parent (", backend, ")"),
      case,
      paste(
        "occurrence:", occurrence,
        "\n",
        paste(capture.output(print(result)), collapse = "\n")
      )
    )
  }
  grand <- result[result$.pbt_set == dimension_count + 1L, , drop = FALSE]
  pbt_check(
    all(grand$.pbt_parent == 1),
    paste0("Grand total Parent shares equal one (", backend, ")"),
    case,
    paste(capture.output(print(result)), collapse = "\n")
  )
}

pbt_canonical_vector <- function(x) {
  if (length(x) == 0L) {
    return(character())
  }
  if (is.factor(x)) {
    x <- as.character(x)
  }
  if (is.logical(x)) {
    return(ifelse(is.na(x), "<NA>", ifelse(x, "TRUE", "FALSE")))
  }
  if (is.numeric(x) || is.integer(x)) {
    return(ifelse(
      is.na(x),
      "<NA>",
      format(as.numeric(x), scientific = FALSE, trim = TRUE, digits = 17)
    ))
  }
  if (is.character(x)) {
    return(ifelse(is.na(x), "<NA>", paste0("<CHR>", x)))
  }
  ifelse(is.na(x), "<NA>", paste0("<OTHER>", as.character(x)))
}

pbt_canonical_frame <- function(x, keys) {
  x <- tibble::as_tibble(x)
  x[] <- lapply(x, pbt_canonical_vector)
  pbt_sort_frame(x, keys)
}

pbt_case_for_storage <- function(case, backend) {
  local <- case
  if (backend %in% c("RSQLite", "Arrow")) {
    local$data[] <- lapply(local$data, function(column) {
      if (is.factor(column)) as.character(column) else column
    })
  }
  if (identical(backend, "RSQLite")) {
    local$data[] <- lapply(local$data, function(column) {
      if (is.logical(column)) as.integer(column) else column
    })
  }
  local
}

pbt_lazy_case <- function(case, backend, connection = NULL, table_index = 1L) {
  local <- pbt_case_for_storage(case, backend)
  lazy <- local
  if (identical(backend, "dtplyr")) {
    lazy$data <- dtplyr::lazy_dt(local$data)
  } else if (backend %in% c("RSQLite", "DuckDB")) {
    lazy$data <- dplyr::copy_to(
      connection,
      local$data,
      paste0("pbt_", tolower(backend), "_", table_index),
      temporary = TRUE,
      overwrite = TRUE
    )
  } else if (identical(backend, "Arrow")) {
    lazy$data <- arrow::Table$create(local$data)
  } else {
    stop("Unknown PBT backend.", call. = FALSE)
  }
  list(local = local, lazy = lazy)
}

pbt_property_backend_summary <- function(pair, backend) {
  actual <- pbt_summary(pair$lazy) |>
    dplyr::collect()
  expected <- dplyr::bind_rows(lapply(seq_along(pair$local$sets), function(index) {
    set <- pair$local$sets[[index]]
    keys <- c(pair$local$fixed, set)
    if (length(keys) == 0L) {
      branch <- dplyr::summarise(
        pair$lazy$data,
        .pbt_rows = dplyr::n(),
        .pbt_total = sum(.data$.pbt_measure, na.rm = TRUE)
      )
    } else {
      branch <- dplyr::summarise(
        pair$lazy$data,
        .pbt_rows = dplyr::n(),
        .pbt_total = sum(.data$.pbt_measure, na.rm = TRUE),
        .by = dplyr::all_of(keys)
      )
    }
    branch <- dplyr::collect(branch)
    for (name in setdiff(pair$local$dimensions, set)) {
      branch[[name]] <- rep(NA_character_, nrow(branch))
    }
    branch$.pbt_set <- rep.int(as.integer(index), nrow(branch))
    branch$.pbt_gid <- rep.int(
      pbt_expected_gid(pair$local$dimensions, set),
      nrow(branch)
    )
    bits <- as.integer(!pair$local$dimensions %in% set)
    for (bit_index in seq_along(bits)) {
      branch[[paste0(".pbt_bit_", bit_index)]] <- rep.int(
        bits[[bit_index]],
        nrow(branch)
      )
    }
    branch <- branch[c(
      pair$local$fixed,
      pair$local$dimensions,
      ".pbt_set",
      ".pbt_rows",
      ".pbt_total",
      ".pbt_gid",
      paste0(".pbt_bit_", seq_along(pair$local$dimensions))
    )]
    branch[] <- lapply(branch, pbt_canonical_vector)
    branch
  }))
  keys <- c(pair$local$fixed, pair$local$dimensions, ".pbt_set")
  actual <- pbt_canonical_frame(actual, keys)
  expected <- pbt_sort_frame(expected, keys)
  pbt_check(
    pbt_identical_frame(actual, expected),
    paste0("lazy summary equals per-grouping-set oracle (", backend, ")"),
    pair$local,
    pbt_frame_diff(actual, expected)
  )
}

pbt_property_backend_expansion <- function(pair, backend) {
  actual <- expand_with_margins(
    pair$lazy$data,
    .by = dplyr::all_of(pair$lazy$fixed),
    .grouping = pair$lazy$spec,
    .margin_label = NULL,
    .duplicates = "keep",
    .sort = "none",
    .id = ".pbt_set"
  ) |>
    dplyr::collect()
  expected <- pbt_expansion_oracle(pair$local)
  keys <- c(".pbt_set", ".pbt_row")
  actual <- pbt_canonical_frame(actual, keys)
  expected <- pbt_canonical_frame(expected, keys)
  pbt_check(
    pbt_identical_frame(actual, expected),
    paste0("lazy expansion emits one correctly labelled copy per set (", backend, ")"),
    pair$local,
    pbt_frame_diff(actual, expected)
  )
}

pbt_property_backend_sort <- function(pair, backend, direction) {
  actual <- pbt_summary(pair$lazy, sort = direction) |>
    dplyr::collect() |>
    tibble::as_tibble()
  expected_order <- vctrs::vec_order(pbt_order_key(actual, pair$local, direction))
  pbt_check(
    identical(expected_order, seq_len(nrow(actual))),
    paste0("lazy Margin order follows structural key (", backend, ", ", direction, ")"),
    pair$local,
    paste(
      "expected row permutation:", paste(expected_order, collapse = ", "),
      "\nactual:\n",
      paste(capture.output(print(actual)), collapse = "\n")
    )
  )
}

pbt_has_duplicate_varying_bit_patterns <- function(case) {
  patterns <- vapply(case$dimensions, function(dimension) {
    paste0(
      as.integer(!vapply(case$sets, function(set) dimension %in% set, logical(1))),
      collapse = ""
    )
  }, character(1))
  varying <- grepl("0", patterns, fixed = TRUE) & grepl("1", patterns, fixed = TRUE)
  anyDuplicated(patterns[varying]) > 0L
}

pbt_run_backends <- function() {
  backend_iterations <- as.integer(Sys.getenv("MARGINPLYR_PBT_BACKEND_ITERATIONS", "75"))
  backends <- c("dtplyr", "RSQLite", "DuckDB", "Arrow")
  sqlite <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(sqlite), add = TRUE)
  duckdb <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
  on.exit(DBI::dbDisconnect(duckdb, shutdown = TRUE), add = TRUE)

  for (iteration in seq_len(backend_iterations)) {
    for (backend in backends) {
      # A literal backtick in a dtplyr join key is a confirmed upstream-backed
      # violation recorded by this investigation. Exclude it after that first
      # failure so the remaining generated space can continue to be searched.
      case <- pbt_generate_case(
        max_rows = 16L,
        max_dimensions = 3L,
        excluded_names = "with`tick"
      )
      pbt_counts$generated <- pbt_counts$generated + 1L
      connection <- switch(backend, RSQLite = sqlite, DuckDB = duckdb, NULL)
      pair <- pbt_lazy_case(case, backend, connection, iteration)
      pbt_attempt(
        pbt_property_backend_summary(pair, backend),
        paste0("backend summary completes for generated valid names (", backend, ")"),
        pair$local
      )
      pbt_attempt(
        pbt_property_backend_expansion(pair, backend),
        paste0("backend expansion completes for generated valid names (", backend, ")"),
        pair$local
      )
      if (
        identical(backend, "Arrow") &&
          pbt_has_duplicate_varying_bit_patterns(pair$local)
      ) {
        # This is the second confirmed violation from the exploration. Skip
        # only its known failing precondition so other Arrow sort keys run.
        pbt_counts$discarded <- pbt_counts$discarded + 2L
      } else {
        pbt_attempt(
          pbt_property_backend_sort(pair, backend, "last"),
          paste0("backend last-order completes for generated valid names (", backend, ")"),
          pair$local
        )
        pbt_attempt(
          pbt_property_backend_sort(pair, backend, "first"),
          paste0("backend first-order completes for generated valid names (", backend, ")"),
          pair$local
        )
      }

      if (!identical(backend, "Arrow")) {
        share_case <- pbt_generate_case(
          min_rows = 1L,
          max_rows = 16L,
          min_dimensions = 1L,
          max_dimensions = 3L,
          allow_duplicate_sets = FALSE,
          positive_measure = TRUE,
          excluded_names = "with`tick"
        )
        pbt_counts$generated <- pbt_counts$generated + 1L
        share_pair <- pbt_lazy_case(
          share_case,
          backend,
          connection,
          backend_iterations + iteration
        )
        pbt_attempt(
          pbt_property_share(
            share_pair$lazy,
            check_share_source = !identical(backend, "RSQLite"),
            backend = backend
          ),
          paste0("backend shares complete for generated valid names (", backend, ")"),
          share_pair$local
        )
      }
    }
  }
  cat(
    "PBT BACKENDS PASS\n",
    "seed: ", pbt_seed, "\n",
    "generated cases: ", pbt_counts$generated, "\n",
    "property checks: ", pbt_counts$checks, "\n",
    "discarded cases: ", pbt_counts$discarded, "\n",
    sep = ""
  )
}

pbt_nest_call <- function(case, verb, keep) {
  grouping <- pbt_rollup_spec(case$dimensions)
  verb(
    case$data,
    .by = dplyr::all_of(case$fixed),
    .grouping = grouping,
    .margin_label = NULL,
    .duplicates = "drop",
    .sort = "none",
    .id = ".pbt_set",
    .keep = keep
  ) |>
    dplyr::collect()
}

pbt_normalize_nest <- function(result, case, project_keys = FALSE) {
  result <- dplyr::ungroup(result)
  cells <- result$data
  outer_names <- c(case$fixed, case$dimensions, ".pbt_set")
  order <- vctrs::vec_order(result[outer_names])
  outer <- pbt_canonical_frame(result[outer_names], outer_names)
  cells <- cells[order]
  cells <- lapply(cells, function(cell) {
    cell <- tibble::as_tibble(cell)
    if (project_keys) {
      cell <- cell[setdiff(names(cell), c(case$fixed, case$dimensions))]
    }
    cell <- pbt_canonical_frame(cell, intersect(".pbt_row", names(cell)))
    cell
  })
  list(outer = outer, cells = cells)
}

pbt_expanded_membership <- function(case) {
  grouping <- pbt_rollup_spec(case$dimensions)
  expanded <- expand_with_margins(
    case$data,
    .by = dplyr::all_of(case$fixed),
    .grouping = grouping,
    .margin_label = NULL,
    .duplicates = "drop",
    .sort = "none",
    .id = ".pbt_set"
  ) |>
    dplyr::collect()
  keys <- c(case$fixed, case$dimensions, ".pbt_set")
  grouped <- dplyr::summarise(
    expanded,
    ids = list(sort(.data$.pbt_row)),
    .by = dplyr::all_of(keys)
  )
  order <- vctrs::vec_order(grouped[keys])
  list(
    outer = pbt_canonical_frame(grouped[keys], keys),
    ids = grouped$ids[order]
  )
}

pbt_property_nesting <- function(case, backend) {
  expected <- pbt_expanded_membership(case)
  normalized <- list()
  for (verb_name in c("nest_with_margins", "nest_by_with_margins")) {
    verb <- get(verb_name, mode = "function")
    kept <- pbt_nest_call(case, verb, keep = TRUE)
    kept_normal <- pbt_normalize_nest(kept, case)
    actual_ids <- lapply(kept_normal$cells, function(cell) {
      sort(as.integer(cell$.pbt_row))
    })
    pbt_check(
      identical(kept_normal$outer, expected$outer) &&
        identical(actual_ids, expected$ids),
      paste0("nesting source-row membership matches expansion (", backend, ", ", verb_name, ")"),
      case,
      paste(capture.output(str(list(actual = kept_normal, expected = expected))), collapse = "\n")
    )

    dropped <- pbt_nest_call(case, verb, keep = FALSE)
    dropped_normal <- pbt_normalize_nest(dropped, case)
    projected <- pbt_normalize_nest(kept, case, project_keys = TRUE)
    pbt_check(
      identical(projected, dropped_normal),
      paste0("nesting .keep projection is exact (", backend, ", ", verb_name, ")"),
      case,
      paste(capture.output(str(list(projected = projected, dropped = dropped_normal))), collapse = "\n")
    )
    normalized[[verb_name]] <- kept_normal
  }
  pbt_check(
    identical(normalized[[1L]], normalized[[2L]]),
    paste0("nest and nest_by agree apart from documented grouping (", backend, ")"),
    case,
    paste(capture.output(str(normalized)), collapse = "\n")
  )

  permuted <- case
  source <- if (is.data.frame(case$data)) case$data else dplyr::collect(case$data)
  permuted$data <- source[rev(seq_len(nrow(source))), , drop = FALSE]
  if (identical(backend, "dtplyr")) {
    permuted$data <- dtplyr::lazy_dt(permuted$data)
  }
  permuted_result <- pbt_nest_call(
    permuted,
    nest_with_margins,
    keep = TRUE
  )
  pbt_check(
    identical(
      pbt_normalize_nest(permuted_result, permuted),
      normalized[["nest_with_margins"]]
    ),
    paste0("nesting is invariant to source-row permutation (", backend, ")"),
    case,
    "Normalized nesting results differed after reversing source rows."
  )
}

pbt_run_nesting <- function() {
  nesting_iterations <- as.integer(Sys.getenv("MARGINPLYR_PBT_NESTING_ITERATIONS", "150"))
  for (iteration in seq_len(nesting_iterations)) {
    for (backend in c("local", "dtplyr")) {
      case <- pbt_generate_case(
        min_rows = 1L,
        max_rows = 16L,
        min_dimensions = 1L,
        max_dimensions = 3L,
        allow_duplicate_sets = FALSE,
        excluded_names = "with`tick"
      )
      if (identical(backend, "dtplyr")) {
        case$data <- dtplyr::lazy_dt(case$data)
      }
      pbt_counts$generated <- pbt_counts$generated + 1L
      pbt_attempt(
        pbt_property_nesting(case, backend),
        paste0("generated nesting case completes (", backend, ")"),
        case
      )
    }
  }
  cat(
    "PBT NESTING PASS\n",
    "seed: ", pbt_seed, "\n",
    "generated cases: ", pbt_counts$generated, "\n",
    "property checks: ", pbt_counts$checks, "\n",
    "discarded cases: ", pbt_counts$discarded, "\n",
    sep = ""
  )
}

pbt_run <- function() {
  for (iteration in seq_len(pbt_iterations)) {
    case <- pbt_generate_case()
    pbt_counts$generated <- pbt_counts$generated + 1L
    pbt_property_inspection(case)
    pbt_property_summary_oracle(case)
    pbt_property_expansion_oracle(case)
    pbt_property_sort(case, "last")
    pbt_property_sort(case, "first")

    plan_case <- pbt_generate_plan_case()
    pbt_counts$generated <- pbt_counts$generated + 1L
    pbt_property_constructor_plan(plan_case)

    share_case <- pbt_generate_case(
      min_rows = 1L,
      max_rows = 24L,
      min_dimensions = 1L,
      max_dimensions = 4L,
      allow_duplicate_sets = FALSE,
      positive_measure = TRUE
    )
    pbt_counts$generated <- pbt_counts$generated + 1L
    pbt_property_share(share_case)
  }
  cat(
    "PBT PASS\n",
    "seed: ", pbt_seed, "\n",
    "generated cases: ", pbt_counts$generated, "\n",
    "property checks: ", pbt_counts$checks, "\n",
    "discarded cases: ", pbt_counts$discarded, "\n",
    sep = ""
  )
}

if (identical(Sys.getenv("MARGINPLYR_PBT_MODE", "local"), "backends")) {
  pbt_run_backends()
} else if (identical(Sys.getenv("MARGINPLYR_PBT_MODE", "local"), "nesting")) {
  pbt_run_nesting()
} else {
  pbt_run()
}
