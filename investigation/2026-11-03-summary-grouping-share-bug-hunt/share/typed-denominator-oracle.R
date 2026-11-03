Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(quiet = TRUE)
dir <- "investigation/2026-11-03-summary-grouping-share-bug-hunt/share"
keys <- list(
  character = c("a", "b", "a", NA),
  double_nan = c(1, NaN, 1, NA_real_),
  factor = factor(c("a", "b", "a", NA), levels = c("a", "b", "unused")),
  factor_na_level = structure(c(1L, 2L, 1L, NA_integer_), levels = c("a", NA_character_), class = "factor"),
  ordered = ordered(c("a", "b", "a", NA)),
  Date = as.Date(c(1, 2, 1, NA), origin = "1970-01-01"),
  POSIXct = as.POSIXct(c(1, 2, 1, NA), origin = "1970-01-01", tz = "Asia/Tokyo"),
  POSIXlt = as.POSIXlt(as.POSIXct(c(1, 2, 1, NA), origin = "1970-01-01", tz = "UTC")),
  difftime = as.difftime(c(1, 2, 1, NA), units = "hours"),
  complex = c(1+1i, 2+1i, 1+1i, NA_complex_),
  raw = as.raw(c(1, 2, 1, 0)),
  list = list(1, c(2, 3), 1, NULL),
  packed = data.frame(a = c(1, 2, 1, NA), b = c("a", "b", "a", NA)),
  matrix = cbind(a = c(1, 2, 1, NA), b = c(2, 3, 2, NA)),
  array = array(c(1, 2, 1, NA), dim = c(4L, 1L, 1L)),
  integer64 = bit64::as.integer64(c("9007199254740992", "9007199254740993", "9007199254740992", NA))
)
rows <- list()
for (key_name in names(keys)) {
  for (role in c("fixed", "dimension")) {
    for (backend in c("local", "dtplyr")) {
      input <- tibble::tibble(x = c(1, 3, 2, 4), h = c("u", "u", "v", "v"))
      input$f <- if (role == "fixed") keys[[key_name]] else rep("F", 4)
      input$g <- if (role == "dimension") keys[[key_name]] else c("a", "b", "a", "b")
      data <- if (backend == "local") input else dtplyr::lazy_dt(input)
      summarize <- function(shares) {
        args <- list(value = quote(sum(x)))
        if (shares) {
          args$parent <- quote(share_of_parent(value))
          args$total <- quote(share_of_total(value))
        }
        args$.by <- quote(f)
        args$.grouping <- quote(rollup(g, h))
        args$.id <- "occ"
        args$.margin_label <- NA_character_
        out <- eval(rlang::call2("summarize_with_margins", quote(data), !!!args))
        if (inherits(out, "dtplyr_step")) dplyr::collect(out) else out
      }
      plain <- tryCatch(summarize(FALSE), error = identity)
      out <- tryCatch(summarize(TRUE), error = identity)
      status <- if (inherits(out, "error")) {
        if (inherits(plain, "error")) "paired_error" else "share_only_error"
      } else "pass"
      checked <- 0L
      error <- ""
      if (inherits(out, "error")) error <- conditionMessage(out)
      if (!inherits(out, "error")) {
        oracle <- tryCatch({
          for (i in seq_len(nrow(out))) {
            for (kind in c("parent", "total")) {
              own <- out$occ[[i]] == 3L
              expected <- 1
              if (!own) {
                target <- if (kind == "total") 3L else out$occ[[i]] + 1L
                candidates <- which(out$occ == target)
                join <- if (kind == "parent" && target == 2L) c("f", "g") else "f"
                for (key in join) {
                  same <- vctrs::vec_equal(vctrs::vec_slice(out[[key]], rep(i, length(candidates))),
                                           vctrs::vec_slice(out[[key]], candidates), na_equal = TRUE)
                  candidates <- candidates[same]
                }
                stopifnot(length(candidates) == 1L)
                expected <- out$value[[i]] / out$value[[candidates]]
              }
              stopifnot(isTRUE(all.equal(out[[kind]][[i]], expected)))
              checked <- checked + 1L
            }
          }
        }, error = identity)
        if (inherits(oracle, "error")) {
          status <- "oracle_disagreement"
          error <- conditionMessage(oracle)
          saveRDS(list(input = input, plain = plain, out = out), file.path(dir, paste0("typed-", key_name, "-", role, "-", backend, ".rds")))
        }
      }
      rows[[length(rows) + 1L]] <- data.frame(key_name, role, backend, status, checked, error)
      cat(key_name, role, backend, status, checked, "\n")
    }
  }
}
out <- do.call(rbind, rows)
write.csv(out, file.path(dir, "typed-denominator-oracle.csv"), row.names = FALSE)
cat("SHA:", system2("git", c("rev-parse", "HEAD"), stdout = TRUE), "\n")
print(table(out$status))
cat("Checked ratios:", sum(out$checked), "\n")
