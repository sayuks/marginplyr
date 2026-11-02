# Dated public-seam probe; no package implementation is changed.
pkgload::load_all(quiet = TRUE)
Sys.setenv(TESTTHAT_PARALLEL = "false")
suppressPackageStartupMessages(library(dplyr))
set.seed(26110231)
new_bucket <- function(x) vctrs::new_vctr(x, class = "hunt_bucket")
vec_proxy_equal.hunt_bucket <- function(x, ...) floor(vctrs::vec_data(x) / 2)
format.hunt_bucket <- function(x, ...) as.character(vctrs::vec_data(x))
vec_ptype2.hunt_bucket.hunt_bucket <- function(x, y, ...) new_bucket(double())
vec_cast.hunt_bucket.hunt_bucket <- function(x, to, ...) x
for (nm in c("vec_proxy_equal", "vec_ptype2", "vec_cast")) {
  suffix <- if (nm == "vec_proxy_equal") "hunt_bucket" else "hunt_bucket.hunt_bucket"
  registerS3method(nm, suffix, get(paste0(nm, ".", suffix)), asNamespace("vctrs"))
}
mk <- function() {
  shared <- new.env(parent = emptyenv())
  tibble(
    text = c("b", "a", NA, "a", "b", NA),
    factor = structure(c(1L, 2L, NA_integer_, 1L, 2L, NA_integer_),
      levels = c("a", NA_character_), class = "factor"),
    bucket = new_bucket(c(1, 0, 2, 3, 4, 5)),
    versions = package_version(c("1.0", "1.0.0", "2.0", "2.0.0", "1.0", "2.0.0")),
    packed = tibble(a = c(1L, 1L, 2L, 2L, NA, NA),
      b = factor(c("a", "a", "b", "b", NA, NA))),
    matrix = I(matrix(c(1L,1L,2L,2L,NA,NA,3L,3L,4L,4L,NA,NA), ncol = 2)),
    reference = list(shared, shared, NULL, NULL, new.env(), new.env()),
    complex = c(1 + 1i, 1 + 1i, NaN + 1i, NA_complex_, 0i, 0i),
    raw = as.raw(c(0, 0, 2, 2, 4, 4)),
    integer64 = bit64::as.integer64(c("9007199254740992", "9007199254740993",
       "9007199254740992", "9007199254740993", NA, NA)),
    payload = c(10, 10, 30, NA, Inf, -Inf),
    row_id = seq_len(6L)
  )
}
base <- mk()
key_names <- setdiff(names(base), c("payload", "row_id"))
canon <- function(cells) {
  keys <- vapply(cells, function(x) paste(x, collapse = ","), character(1))
  sort(keys)
}
records <- list(); record <- 0L; projections <- 0L
for (iteration in seq_len(180L)) {
  chosen <- sample(key_names, 4L)
  dimensions <- chosen[1:2]; fixed <- chosen[3:4]
  if (iteration %% 3L == 0L) fixed <- character()
  indices <- switch(as.character(iteration %% 4L), `0` = integer(), `1` = 1:6,
    `2` = c(6L, 2L, 4L, 1L), `3` = c(2L, 4L, 2L, 5L))
  input <- vctrs::vec_slice(base, indices)
  # New row identifiers make duplicate sampled source rows distinguishable.
  input$row_id <- seq_len(nrow(input))
  family <- c("rollup", "cube", "union")[[1L + iteration %% 3L]]
  if (family == "rollup") {
    spec <- rlang::eval_tidy(rlang::call2("rollup", !!!rlang::syms(dimensions)))
    sets <- list(dimensions, dimensions[1L], character())
  } else if (family == "cube") {
    spec <- rlang::eval_tidy(rlang::call2("cube", !!!rlang::syms(dimensions)))
    sets <- list(dimensions, dimensions[1L], dimensions[2L], character())
  } else {
    units <- lapply(dimensions, function(x) rlang::call2("grouping_set", rlang::sym(x)))
    spec <- rlang::eval_tidy(rlang::call2("grouping_sets", !!!units))
    sets <- lapply(dimensions, function(x) x)
  }
  labels <- if ("raw" %in% dimensions) setNames(lapply(dimensions, function(x) {
    if (x == "raw") "ALL" else NULL
  }), dimensions) else NULL
  for (keep in c(FALSE, TRUE)) for (sort_option in c("none", "first", "last")) {
    for (verb_name in c("nest_with_margins", "nest_by_with_margins")) {
      before <- serialize(input, NULL)
      result <- tryCatch(get(verb_name)(input, .by = all_of(fixed), .grouping = spec,
        .margin_label = labels, .id = "set", .keep = keep, .sort = sort_option),
        error = identity)
      record <- record + 1L
      if (inherits(result, "error")) {
        records[[record]] <- data.frame(iteration, family, verb_name, keep, sort_option,
          dimensions=paste(dimensions,collapse=","), fixed=paste(fixed,collapse=","),
          verdict="error", detail=conditionMessage(result))
        next
      }
      for (set_i in seq_along(sets)) {
        keys <- c(fixed, sets[[set_i]])
        expected <- if (nrow(input) == 0L) list() else if (length(keys) == 0L) {
          list(seq_len(nrow(input)))
        } else vctrs::vec_group_loc(input[keys])$loc
        cells <- result$data[result$set == set_i]
        actual <- lapply(cells, function(cell) cell$row_id)
        if (!identical(canon(actual), canon(expected))) {
          stop("Membership mismatch in case ", record, " set ", set_i,
            ": expected ", paste(canon(expected),collapse="/"),
            "; observed ",paste(canon(actual),collapse="/"))
        }
        cell_names <- if (keep) c(fixed, dimensions, setdiff(names(input), c(fixed,dimensions))) else {
          setdiff(names(input),c(fixed, dimensions))
        }
        for (cell in cells) {
          stopifnot(identical(names(cell), cell_names))
          for (col in cell_names) {
            want <- vctrs::vec_slice(input[[col]], cell$row_id)
            if (!identical(cell[[col]], want)) {
              # Bare names and unsupported arbitrary attrs can be dropped by
              # dplyr union; separate those from value changes below.
              if (!isTRUE(all.equal(cell[[col]], want, check.attributes=FALSE))) {
                stop("Projection value mismatch case ",record," column ",col)
              }
            }
            projections <- projections + 1L
          }
        }
      }
      stopifnot(identical(serialize(input,NULL), before))
      records[[record]] <- data.frame(iteration, family, verb_name, keep, sort_option,
        dimensions=paste(dimensions,collapse=","), fixed=paste(fixed,collapse=","),
        verdict="match", detail="")
    }
  }
}
ledger <- do.call(rbind,records)
write.csv(ledger, "investigation/2026-11-02-nest-grouping-hunt/local/composed-key-results.csv", row.names=FALSE)
print(table(ledger$verdict)); cat("Cell-column projections:",projections,"\n")
if (any(ledger$verdict == "error")) print(unique(ledger[ledger$verdict=="error",c("dimensions","fixed","detail")]),row.names=FALSE)
print(sessionInfo())
