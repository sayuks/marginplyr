args <- commandArgs(TRUE)
stopifnot(length(args) == 2L)
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(args[[1L]], quiet = TRUE)
library(dplyr)
types <- list(
  integer = c(2L, NA_integer_, 1L),
  double = c(NaN, -Inf, Inf),
  character = c("z", NA, "a"),
  logical = c(TRUE, NA, FALSE),
  complex = c(2+1i, NA_complex_, 1-2i),
  factor = factor(c("b", NA, "a"), levels = c("b", "a", "unused")),
  factor_na = structure(c(2L, NA_integer_, 1L), levels = c("b", NA, "a"), class = "factor"),
  ordered = ordered(c("b", NA, "a"), levels = c("b", "a")),
  date = as.Date(c("2026-01-02", NA, "2026-01-01")),
  time = as.POSIXct(c("2026-01-02", NA, "2026-01-01"), tz = "Asia/Tokyo"),
  list = list(NULL, NA, list(1))
)
plans <- list(
  rollup = list(expr = quote(rollup(a, b)), sets = list(c("a", "b"), "a", character())),
  cube = list(expr = quote(cube(a, b)), sets = list(c("a", "b"), "a", "b", character())),
  reversed = list(expr = quote(grouping_sets(grouping_set(), grouping_set(b), grouping_set(a,b))), sets = list(character(), "b", c("a", "b"))),
  duplicate = list(expr = quote(grouping_sets(grouping_set(a,b), grouping_set(a), grouping_set(a,b), grouping_set())), sets = list(c("a", "b"), "a", c("a", "b"), character()))
)
label_pairs <- list(list(a = "Everything A", b = "Everything B"), list(a = NULL, b = "Everything B"), list(a = "Everything A", b = NULL), list(a = NA_character_, b = "Everything B"))
display <- function(x, label, n, omitted, position) {
  missing <- is.null(label) || is.na(label)
  if (is.factor(x)) {
    lv <- levels(x)
    if (!missing) lv <- if (position == "first") c(label, lv) else c(lv, label)
    codes <- if (omitted) rep(if (missing) NA_integer_ else match(label, lv), n) else match(as.character.factor(x), lv)
    if (!omitted) codes[is.na(unclass(x))] <- NA_integer_
    return(structure(codes, levels = lv, class = if (is.ordered(x)) c("ordered", "factor") else "factor"))
  }
  if (missing) return(if (omitted) vctrs::vec_init(x,n) else x)
  if (omitted) rep(label,n) else as.vector(as.character(x), mode = "character")
}
rows <- list()
smoke <- identical(Sys.getenv("MARGINPLYR_HUNT_SMOKE"), "true")
for (pair in if (smoke) 1L else seq_len(11L)) for (n in if (smoke) 1L else c(0L, 1L, 3L)) for (labels in if (smoke) 1L else seq_along(label_pairs)) for (plan_name in names(plans)) for (position in if (smoke) "last" else c("first", "last")) for (container in c("tibble", "data.frame", "data.table", "grouped")) {
  left <- names(types)[[pair]]
  right <- names(types)[[(pair * 3L) %% length(types) + 1L]]
  x <- tibble(fixed = c("g", "g", NA)[seq_len(n)], a = vctrs::vec_slice(types[[left]],seq_len(n)), b = vctrs::vec_slice(types[[right]],seq_len(n)), payload = seq_len(n))
  if (container == "data.frame") x <- as.data.frame(x)
  if (container == "data.table") x <- data.table::as.data.table(x)
  if (container == "grouped") x <- group_by(x, fixed)
  before <- serialize(x,NULL)
  label <- label_pairs[[labels]]
  pl <- plans[[plan_name]]
  dimensions <- unique(unlist(pl$sets, use.names = FALSE))
  refusal <- (is.factor(x$a) && anyNA(levels(x$a)) && !is.null(label$a) && is.na(label$a)) || (is.factor(x$b) && anyNA(levels(x$b)) && !is.null(label$b) && is.na(label$b))
  base <- tryCatch({
    branches <- lapply(seq_along(pl$sets), function(s) {
      y <- as_tibble(ungroup(x))
      for (k in c("a", "b")) y[[k]] <- display(x[[k]], label[[k]], n, !k %in% pl$sets[[s]], position)
      y$sid <- rep(s, n)
      y
    })
    y <- select(Reduce(union_all, branches), fixed, all_of(dimensions), sid, payload)
    y
  }, error = identity)
  by_expr <- if (container == "grouped") NULL else quote(fixed)
  for (sorting in c("none", "first", "last")) {
    expected <- tryCatch({
      if (inherits(base,"error")) stop(base)
      y <- base
      if (sorting != "none") {
        keys <- list(vctrs::vec_detect_missing(y$fixed), y$fixed)
        for (k in dimensions) {
          bits <- vapply(y$sid, function(s) as.integer(!k %in% pl$sets[[s]]), integer(1))
          if (sorting == "first") bits <- -bits
          keys <- c(keys, list(bits, vctrs::vec_detect_missing(y[[k]]), y[[k]]))
        }
        keys <- c(keys,list(y$sid))
        y <- vctrs::vec_slice(y,vctrs::vec_order(tibble::new_tibble(setNames(keys,paste0("v",seq_along(keys))),nrow=nrow(y))))
      }
      y
    }, error = identity)
    actual <- tryCatch(rlang::inject(expand_with_margins(x, .by = !!by_expr, .grouping = !!pl$expr, .margin_label = label, .margin_label_position = position, .duplicates = "keep", .id = "sid", .sort = sorting)), error = identity)
    if (!inherits(actual,"error")) actual <- as_tibble(actual)
    status <- if (refusal && inherits(actual,"marginplyr_error")) "required_refusal" else if (refusal) "missing_refusal" else if (inherits(expected,"error")) "ordinary_failure" else if (inherits(actual,"error")) "package_error" else if (!identical(actual,expected)) "difference" else "match"
    detail <- if (inherits(actual,"error")) conditionMessage(actual) else if (status == "difference") paste(all.equal(actual, expected), collapse="; ") else if (inherits(expected,"error")) conditionMessage(expected) else ""
    stopifnot(identical(serialize(x,NULL), before))
    rows[[length(rows)+1L]] <- data.frame(left=left,right=right,n=n,labels=labels,plan=plan_name,position=position,container=container,sort=sorting,status=status,detail=detail)
  }
}
result <- bind_rows(rows)
write.csv(result,args[[2L]],row.names=FALSE)
print(table(result$status))
print(head(result[result$status %in% c("difference","package_error","missing_refusal"),], 20))
cat("comparisons",nrow(result),"\n")
print(sessionInfo())
