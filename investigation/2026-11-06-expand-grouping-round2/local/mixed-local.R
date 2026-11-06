args <- commandArgs(TRUE)
pkgload::load_all(args[[1]], quiet = TRUE)
out <- args[[2]]
fixed <- if (length(args) >= 3L && args[[3]] == "shared") c("g", "g", NA) else c("z", NA, "a")
library(dplyr)
types <- list(
  integer = c(2L, NA_integer_, 1L),
  factor_na = structure(c(2L, NA_integer_, 1L), levels = c("b", NA, "a"), class = "factor"),
  ordered = ordered(c("b", NA, "a"), levels = c("b", "a")),
  date = as.Date(c("2024-02-02", NA, "2023-01-01")),
  list = list(NA, NULL, list(1)),
  listof = vctrs::list_of(c(1L, 2L), integer(), NA_integer_),
  matrix = matrix(c(2L, NA, 1L, NA, NA, 3L), ncol = 2),
  listmatrix = matrix(list(1, NULL, NA, NULL, NULL, list(2)), ncol = 2),
  packed = tibble(z = c(2L, NA, 1L), y = list(NULL, NA, list(1))),
  nested = tibble(z = tibble(a = c(2L, NA, 1L), b = c(NA, NA, 3L))),
  empty = tibble(.rows = 3)
)
miss <- function(x) {
  if (is.data.frame(x)) {
    return(Reduce(`&`, lapply(x, miss), init = rep(TRUE, nrow(x))))
  }
  if (is.array(x)) {
    y <- if (is.list(x)) vapply(x, is.null, logical(1)) else is.na(x)
    dim(y) <- dim(x)
    return(rowSums(matrix(y, nrow = dim(x)[[1]])) == prod(dim(x)[-1]))
  }
  vctrs::vec_detect_missing(x)
}
sets <- list(c("a", "b"), "a", "b", character())
name_pairs <- list(c("a", "b"), c(".data", ".env"), c("..01", "..-1"), c("TRUE", "FALSE"), c("..marginplyr_1", "..marginplyr_2"))
rows <- list()
for (i in names(types)) for (j in names(types)) for (np in seq_along(name_pairs)) {
  nm <- name_pairs[[np]]
  x <- tibble(fixed = fixed, a = types[[i]], b = types[[j]], payload = 11:13)
  names(x)[2:3] <- nm
  before <- x
  reference <- tryCatch({
    safe <- x
    names(safe)[2:3] <- c("a", "b")
    branches <- lapply(seq_along(sets), function(s) {
      y <- safe
      absent <- setdiff(c("a", "b"), sets[[s]])
      for (k in absent) {
        y[[k]] <- vctrs::vec_init(safe[[k]], 3)
      }
      y$sid <- s
      y
    })
    y <- select(Reduce(union_all, branches), fixed, a, b, sid, payload)
    names(y)[2:3] <- nm
    y
  }, error = identity)
  for (sorting in c("none", "first", "last")) {
    expected <- tryCatch({
      if (inherits(reference, "error")) stop(reference)
      y <- reference
      if (sorting != "none") {
        keys <- list(miss(y$fixed), y$fixed)
        for (k in seq_along(nm)) {
          bits <- vapply(y$sid, function(s) as.integer(!c("a", "b")[[k]] %in% sets[[s]]), integer(1))
          if (sorting == "first") bits <- -bits
          keys <- c(keys, list(bits, miss(y[[nm[[k]]]]), y[[nm[[k]]]]))
        }
        keys <- c(keys, list(y$sid))
        y <- vctrs::vec_slice(y, vctrs::vec_order(tibble::new_tibble(setNames(keys, paste0("v",seq_along(keys))), nrow=12)))
      }
      y
    }, error = identity)
    actual <- tryCatch(expand_with_margins(x, .by = fixed, .grouping = cube(all_of(nm[[1]]), all_of(nm[[2]])), .margin_label=NULL, .id="sid", .sort=sorting), error=identity)
    status <- if (inherits(expected,"error")) "ordinary_failure" else if (inherits(actual,"error")) "package_error" else if (!identical(actual, expected)) "difference" else "match"
    detail <- if (inherits(actual,"error")) conditionMessage(actual) else if (status=="difference") paste(all.equal(actual,expected),collapse="; ") else if (inherits(expected,"error")) conditionMessage(expected) else ""
    stopifnot(identical(x,before))
    rows[[length(rows)+1]] <- data.frame(left=i,right=j,names=np,sort=sorting,status=status,detail=detail)
  }
}
result <- bind_rows(rows)
write.csv(result,out,row.names=FALSE)
print(table(result$status))
print(head(result[result$status %in% c("package_error","difference"),], 12))
cat("comparisons", nrow(result), "\n")
print(sessionInfo())
