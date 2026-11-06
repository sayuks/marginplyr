args <- commandArgs(TRUE)
source_dir <- if (length(args)) args[[1]] else Sys.getenv("MARGINPLYR_HUNT_SOURCE", ".")
out <- if (length(args) > 1) args[[2]] else tempfile("mixed-algebra-")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(source_dir, quiet = TRUE)
set.seed(6110626)
columns <- c("a", "b", "c", "d")
dat <- tibble::tibble(a = "x", b = 1L, c = "y", d = FALSE)
leaf <- function(nonempty = FALSE) {
  if (sample.int(3, 1) == 1L) return(list(kind = "leaf", cols = c("a", "c"), typed = TRUE))
  n <- sample(seq.int(as.integer(nonempty), 3L), 1)
  list(kind = "leaf", cols = if (n) sample(columns, n) else character(), typed = FALSE)
}
node <- function(depth = 0L) {
  kind <- sample(c("set", "sets", "cube", "rollup", "product"), 1)
  count <- sample(if (kind %in% c("set", "product")) 0:3 else 1:3, 1)
  children <- lapply(seq_len(count), function(i) {
    if (kind %in% c("rollup", "cube")) {
      ch <- leaf(TRUE)
      if (sample(c(TRUE, FALSE), 1)) list(kind = "set", children = list(ch)) else ch
    } else if (kind == "set" || depth >= 5L || sample.int(3, 1) <= 2L) leaf() else node(depth + 1L)
  })
  list(kind = kind, children = children)
}
family <- function(x) {
  if (x$kind == "leaf") return(list(x$cols))
  ch <- x$children
  if (x$kind == "set") return(list(unique(unlist(lapply(ch, `[[`, "cols"), use.names = FALSE))))
  if (x$kind == "sets") return(unlist(lapply(ch, family), recursive = FALSE))
  if (x$kind == "product") {
    ans <- list(character())
    for (c in ch) {
      f <- family(c)
      if (length(f) * length(ans) > 256) stop("oracle-cap")
      ans <- unlist(lapply(ans, function(a) lapply(f, function(b) unique(c(a, b)))), recursive = FALSE)
    }
    return(ans)
  }
  units <- unlist(lapply(ch, function(c) if (c$kind == "set") family(c) else lapply(c$cols, identity)), recursive = FALSE)
  n <- length(units)
  if (n > 7L) stop("oracle-cap")
  indices <- if (x$kind == "rollup") lapply(n:0L, seq_len) else unlist(lapply(n:0L, function(k) utils::combn(seq_len(n), k, simplify = FALSE)), recursive = FALSE)
  lapply(indices, function(i) unique(unlist(units[i], use.names = FALSE)))
}
expr <- function(x) {
  if (x$kind == "leaf") return(if (x$typed) quote(tidyselect::where(is.character)) else rlang::call2("all_of", x$cols, .ns = "tidyselect"))
  ctor <- c(set = "grouping_set", sets = "grouping_sets", cube = "cube", rollup = "rollup", product = "grouping_spec")[[x$kind]]
  rlang::call2(ctor, !!!lapply(x$children, expr))
}
expected <- function(fam, policy) {
  dims <- unique(unlist(fam, use.names = FALSE))
  if (is.null(dims)) dims <- character()
  fam <- lapply(fam, function(x) dims[dims %in% x])
  if (policy == "drop") fam <- fam[!duplicated(fam)]
  bits <- lapply(fam, function(x) if (length(dims)) stats::setNames(as.integer(!dims %in% x), dims) else integer())
  tibble::tibble(set_id = seq_along(fam), fixed = rep(list(character()), length(fam)), included = fam,
    omitted = lapply(fam, function(x) dims[!dims %in% x]), grouping_bits = bits,
    grouping_id = vapply(bits, function(b) as.integer(sum(b * 2^rev(seq_along(b) - 1))), integer(1)))
}
records <- list(); cases <- list(); generated <- 0L
while (length(cases) < 750L) {
  generated <- generated + 1L
  x <- node()
  fam <- tryCatch(family(x), error = function(e) NULL)
  if (is.null(fam) || length(fam) > 128L) next
  cases[[length(cases) + 1L]] <- list(x = x, family = fam)
}
for (i in seq_along(cases)) {
  e <- expr(cases[[i]]$x); fam <- cases[[i]]$family
  s <- eval(e)
  keep <- expected(fam, "keep")
  for (policy in c("error", "drop", "keep")) {
    actual <- tryCatch(inspect_grouping(dat, .grouping = s, .duplicates = policy, .format = "list"), error = identity)
    want_error <- policy == "error" && any(duplicated(keep$included))
    match <- if (want_error) inherits(actual, "marginplyr_error") && grepl("Duplicate grouping sets", conditionMessage(actual), fixed=TRUE) else identical(actual, expected(fam, policy))
    positions_valid <- TRUE
    if (want_error && inherits(actual, "condition") && match) {
      positions <- strsplit(conditionMessage(actual), "\n", fixed = TRUE)[[1L]][[2L]]
      positions <- gsub("[^0-9,; ]", "", positions)
      groups <- strsplit(positions, ";", fixed=TRUE)[[1]]
      ids <- lapply(groups, function(g) as.integer(trimws(strsplit(g, ",", fixed=TRUE)[[1]])))
      positions_valid <- !anyNA(unlist(ids)) && all(vapply(ids, function(ix) length(ix) > 1L && all(ix %in% seq_len(nrow(keep))) && length(unique(keep$included[ix])) == 1L, logical(1)))
    }
    records[[length(records)+1L]] <- data.frame(case=i, policy, matches=match, positions_valid, family_size=length(fam),
      expression=paste(deparse(e, width.cutoff=500L), collapse=" "), observation=if (inherits(actual,"condition")) conditionMessage(actual) else "plan")
  }
  if (i %% 100L == 0L) cat("Completed", i, "trees\n")
}
r <- do.call(rbind, records)
write.csv(r, file.path(out,"mixed-algebra.csv"), row.names=FALSE)
saveRDS(cases, file.path(out,"mixed-trees.rds"))
cat("Generated:",generated,"Retained trees:",length(cases),"Comparisons:",nrow(r),"Plan matches:",sum(r$matches),"Position matches:",sum(r$positions_valid),"\n")
print(r[!r$matches | !r$positions_valid,], row.names=FALSE)
print(sessionInfo())
stopifnot(all(r$matches), all(r$positions_valid))
