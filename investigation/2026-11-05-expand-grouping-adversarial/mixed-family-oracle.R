pkgload::load_all(quiet = TRUE)
set.seed(41248)
input <- data.frame(a = 1L, b = "b", c = FALSE, d = 1.5, check.names = FALSE)
leaves <- list(a = quote(a), b = quote(b), c = quote(c), d = quote(d),
               typed_b = quote(dplyr::where(is.character)), typed_c = quote(dplyr::where(is.logical)))
node <- function(depth) {
  kind <- if (depth < 1L) "col" else sample(c("set", "sets", "product", "rollup", "cube", "col"), 1L)
  if (kind == "col") {
    name <- sample(names(leaves), 1L)
    return(list(expr = leaves[[name]], family = list(sub("^typed_", "", name))))
  }
  call <- c(set = "grouping_set", sets = "grouping_sets", product = "grouping_spec", rollup = "rollup", cube = "cube")[[kind]]
  size <- if (kind %in% c("set", "product")) sample(0:3, 1L) else sample(1:3, 1L)
  children <- replicate(size, node(if (kind %in% c("set", "rollup", "cube")) 0L else depth - 1L), simplify = FALSE)
  families <- lapply(children, `[[`, "family")
  expr <- as.call(c(list(as.name(call)), lapply(children, `[[`, "expr")))
  family <- switch(kind,
    set = list(unique(as.character(unlist(families)))),
    sets = do.call(c, families),
    product = {
      res <- list(character())
      for (factor in families) {
        res <- do.call(c, lapply(res, function(left) lapply(factor, function(right) unique(c(left, right)))))
      }
      res
    },
    rollup = lapply(size:0L, function(n) unique(as.character(unlist(families[seq_len(n)])))),
    cube = {
      bits <- expand.grid(rep(list(c(TRUE, FALSE)), size))
      ranked <- do.call(order, c(list(-rowSums(bits)), as.list(as.data.frame(!bits))))
      lapply(ranked, function(i) unique(as.character(unlist(families[as.logical(bits[i, ])]))))
    }
  )
  list(expr = expr, family = family)
}
verify <- function(case) {
  expr <- case$expr
  if (is.name(expr) || identical(expr[[1L]], quote(dplyr::where))) return("bare")
  family <- case$family
  if (length(family) > 300L) return("large")
  keys <- vapply(family, function(x) paste(sort(x), collapse = ""), "")
  dimensions <- unique(as.character(unlist(family)))
  spec <- eval(expr)
  run <- function(policy) inspect_grouping(input, .grouping = spec, .duplicates = policy, .format = "list")
  verify_output <- function(got, expected) {
    stopifnot(identical(got$included, lapply(expected, function(x) dimensions[dimensions %in% x])),
              identical(got$set_id, seq_along(expected)))
  }
  verify_output(run("keep"), family)
  verify_output(run("drop"), family[!duplicated(keys)])
  if (!anyDuplicated(keys)) {
    verify_output(run("error"), family)
    return("unique")
  }
  err <- tryCatch(run("error"), error = identity)
  stopifnot(inherits(err, "marginplyr_error"))
  msg <- strsplit(conditionMessage(err), "\n", fixed = TRUE)[[1L]]
  stopifnot(grepl("Duplicate grouping sets were produced at", msg[[1L]], fixed = TRUE))
  pos <- strsplit(sub("^[^0-9]*([0-9,; ]+)\\.$", "\\1", msg[[2L]]), "; ", fixed = TRUE)[[1L]]
  groups <- lapply(pos, function(xs) as.integer(strsplit(xs, ", ", fixed = TRUE)[[1L]]))
  stopifnot(all(vapply(groups, function(ids) {
    length(ids) >= 2L && all(ids %in% seq_along(keys)) && length(unique(keys[ids])) == 1L
  }, logical(1))))
  "duplicate"
}
limit <- as.integer(commandArgs(TRUE)[[1L]])
results <- character(limit)
for (i in seq_len(limit)) {
  candidate <- node(5L)
  results[[i]] <- tryCatch(verify(candidate), error = function(e) {
    print(candidate$expr)
    print(candidate$family)
    stop(e)
  })
}
print(table(results))
