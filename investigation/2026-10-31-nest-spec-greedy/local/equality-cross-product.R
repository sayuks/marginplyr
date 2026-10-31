source("investigation/2026-10-31-nest-spec-greedy/local/factor-equality.R")
set.seed(31103142)
options <- expand.grid(na_level = c(FALSE, TRUE), n = c(0L, 4L, 8L),
                       role = c("dimension", "fixed"), keep = c(FALSE, TRUE),
                       verb = c("nest", "nest_by"), sort = c("none", "first", "last"),
                       label = c("string", "missing"), plan = c("set", "rollup"),
                       stringsAsFactors = FALSE)
ledger <- vector("list", nrow(options))
for (i in seq_len(nrow(options))) {
  option <- options[i, ]
  source <- tibble::tibble(
    a = new_casefold_factor(rep(c("A", "a", "B", "b"), length.out = option$n), option$na_level),
    b = package_version(rep(c("1.0", "1.0.0", "2.0", "2.0.0"), length.out = option$n)),
    row_id = seq_len(option$n),
    packed = tibble::tibble(x = rep(c(NA_real_, NaN), length.out = option$n),
                            y = matrix(seq_len(option$n * 2L), nrow = option$n, ncol = 2L))
  )
  before <- serialize(source, NULL)
  if (option$role == "fixed") {
    by <- rlang::quo(a)
    specification <- if (option$plan == "set") grouping_set(b) else rollup(b)
    sets <- if (option$plan == "set") list(c("a", "b")) else list(c("a", "b"), "a")
  } else {
    by <- rlang::quo(NULL)
    specification <- if (option$plan == "set") grouping_set(a, b) else rollup(a, b)
    sets <- if (option$plan == "set") list(c("a", "b")) else list(c("a", "b"), "a", character())
  }
  verb <- if (option$verb == "nest") nest_with_margins else nest_by_with_margins
  result <- rlang::inject(verb(source, .by = !!by, .grouping = specification,
                                .id = "set", .keep = option$keep,
                                .sort = option$sort,
                                .margin_label = if (option$label == "string") "Total" else NULL))
  expected <- lapply(sets, function(keys) {
    if (length(keys)) vctrs::vec_group_loc(source[keys])$loc else {
      if (nrow(source)) list(seq_len(nrow(source))) else list()
    }
  })
  observed <- lapply(seq_along(sets), function(set) lapply(result$data[result$set == set], function(cell) cell$row_id))
  normalize <- function(groups) groups[order(vapply(groups, function(ids) ids[[1L]], integer(1)))]
  faithful <- all(vapply(seq_along(sets), function(set) identical(normalize(expected[[set]]), normalize(observed[[set]])), logical(1)))
  stopifnot(identical(before, serialize(source, NULL)))
  projection_ok <- all(vapply(result$data, function(cell) {
    identical(cell$packed, vctrs::vec_slice(source$packed, cell$row_id)) &&
      (!option$keep || (identical(as.character(cell$a), as.character(source$a[cell$row_id])) &&
                         identical(cell$b, source$b[cell$row_id])))
  }, logical(1)))
  stopifnot(projection_ok)
  predicted_bad <- option$na_level && option$n > 0 && option$role == "dimension"
  if (faithful == predicted_bad) stop("Unexpected equality outcome in case ", i)
  ledger[[i]] <- cbind(option, faithful = faithful, cells = length(result$data))
}
ledger <- do.call(rbind, ledger)
write.csv(ledger, "investigation/2026-10-31-nest-spec-greedy/local/equality-cross-product.csv", row.names = FALSE)
cat("calls", nrow(ledger), "faithful", sum(ledger$faithful), "wrong_membership", sum(!ledger$faithful),
    "verified_cell_projections", sum(ledger$cells), "source_mutations", 0L, "\n")
