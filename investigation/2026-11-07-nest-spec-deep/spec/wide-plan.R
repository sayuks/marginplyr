Sys.setenv(TESTTHAT_PARALLEL = "false")
source_root <- Sys.getenv("MARGINPLYR_HUNT_SOURCE")
if (!nzchar(source_root)) stop("Set MARGINPLYR_HUNT_SOURCE.")
pkgload::load_all(source_root, quiet = TRUE)
out <- Sys.getenv("MARGINPLYR_HUNT_OUTPUT")
if (!nzchar(out) || !dir.exists(out)) stop("Set an existing MARGINPLYR_HUNT_OUTPUT.")
set.seed(7110731)
special <- c("a,b", "a=0", "(a)", "a`b", "a\\b", "a\nb", "a\rb",
             "日本語", "🙂", "a b", " a", "a ", ".env", ".data", "..01")
make_set <- function(columns) grouping_set(tidyselect::all_of(!!columns))
make_family <- function(sets) do.call(grouping_sets, sets)
membership <- function(family, dimensions) {
  vapply(family, function(x) paste(as.integer(dimensions %in% x), collapse = ""), "")
}
expected_plan <- function(family, fixed, policy, text = FALSE) {
  dimensions <- unique(unlist(family, use.names = FALSE))
  keys <- membership(family, dimensions)
  if (policy == "drop") family <- family[!duplicated(keys)]
  included <- lapply(family, function(s) dimensions[dimensions %in% s])
  omitted <- lapply(family, function(s) dimensions[!dimensions %in% s])
  bits <- lapply(omitted, function(s) stats::setNames(as.integer(dimensions %in% s), dimensions))
  if (!length(dimensions)) bits <- rep(list(integer()), length(family))
  ids <- vapply(bits, function(b) {
    if (length(b) > 31L) return(NA_integer_)
    if (!length(b)) return(0L)
    # Accumulating from the first bit models its SQL significance without a
    # matrix product or bitwise integer shifts at the signed boundary.
    as.integer(Reduce(function(acc, bit) acc * 2 + bit, b, init = 0))
  }, integer(1))
  fixed <- rep(list(fixed), length(family))
  if (text) {
    display <- function(x) paste0("(", paste(x, collapse = ", "), ")")
    fixed <- vapply(fixed, display, "")
    included <- vapply(included, display, "")
    omitted <- vapply(omitted, display, "")
    bits <- vapply(bits, function(x) {
      if (!length(x)) return("()")
      paste0(names(x), "=", x, collapse = ", ")
    }, "")
  }
  tibble::tibble(set_id = seq_along(family), fixed = fixed,
                 included = included, omitted = omitted,
                 grouping_bits = bits, grouping_id = ids)
}
records <- list()
failures <- list()
for (dimensions_n in c(0L, 1L, 2L, 15L, 30L, 31L, 32L, 33L, 64L, 96L)) {
  columns <- if (dimensions_n) c(special, paste0("dimension-", seq_len(96)))[seq_len(dimensions_n)] else character()
  for (repeat_id in seq_len(8L)) {
    columns <- sample(columns)
    units <- if (length(columns)) split(columns, rep(seq_len(min(6L, length(columns))), length.out = length(columns))) else list()
    units <- unname(units)
    random_sets <- c(list(columns), lapply(seq_len(5L), function(i) sample(columns, sample(0:length(columns), 1L))), list(character(), columns))
    families <- list(union = list(spec = make_family(lapply(random_sets, make_set)), model = random_sets))
    if (length(columns)) {
      rollup_family <- lapply(rev(seq.int(0L, length(columns))), function(i) columns[seq_len(i)])
      families$rollup <- list(spec = rollup(tidyselect::all_of(!!columns)), model = rollup_family)
      cube_family <- unlist(lapply(rev(seq.int(0L, length(units))), function(k) {
        lapply(utils::combn(seq_along(units), k, simplify = FALSE), function(i) unique(unlist(units[i], use.names = FALSE)))
      }), recursive = FALSE)
      families$cube <- list(spec = do.call(cube, lapply(units, make_set)), model = cube_family)
      left <- random_sets[c(1, 3, 7)]
      right <- random_sets[c(2, 4, 6)]
      product <- unlist(lapply(left, function(a) lapply(right, function(b) unique(c(a, b)))), recursive = FALSE)
      families$product <- list(spec = grouping_spec(!!make_family(lapply(left, make_set)), !!make_family(lapply(right, make_set))), model = product)
    }
    for (family_name in names(families)) for (fixed_mode in c("none", "by", "grouped")) {
      x <- families[[family_name]]
      fixed <- if (fixed_mode == "none") character() else "fixed,name"
      dat <- stats::setNames(rep(list("value"), length(c(columns, fixed))), c(columns, fixed))
      dat <- tibble::as_tibble(dat, .name_repair = "minimal")
      if (fixed_mode == "grouped") dat <- dplyr::group_by(dat, !!!rlang::syms(fixed))
      for (policy in c("keep", "drop", "error")) {
        for (format in c("list", "text")) {
          actual <- tryCatch({
            if (fixed_mode == "by") inspect_grouping(dat, .by = tidyselect::all_of(fixed), .grouping = x$spec, .duplicates = policy, .format = format)
            else inspect_grouping(dat, .grouping = x$spec, .duplicates = policy, .format = format)
          }, error = identity)
          dims <- unique(unlist(x$model, use.names = FALSE))
          duplicate <- anyDuplicated(membership(x$model, dims)) > 0L
          should_error <- policy == "error" && duplicate
          same <- if (should_error) {
            inherits(actual, "marginplyr_error") && grepl("Duplicate grouping sets", conditionMessage(actual), fixed = TRUE)
          } else !inherits(actual, "error") && identical(actual, expected_plan(x$model, fixed, policy, format == "text"))
          records[[length(records) + 1L]] <- data.frame(dimensions = dimensions_n, repeat_id, family = family_name, fixed_mode, policy, format, expected_error = should_error, same)
          if (!same) failures[[length(failures) + 1L]] <- list(record = records[[length(records)]], family = x$model, actual = if (inherits(actual, "error")) conditionMessage(actual) else actual)
        }
      }
    }
  }
}
results <- do.call(rbind, records)
utils::write.csv(results, file.path(out, "wide-plan.csv"), row.names = FALSE)
dput(failures, file = file.path(out, "wide-plan-failures.R"))
cat("Wide-plan comparisons:", nrow(results), "failures:", length(failures), "\n")
print(table(results$expected_error, results$same))
if (length(failures)) stop("Wide-plan comparison failed.")
