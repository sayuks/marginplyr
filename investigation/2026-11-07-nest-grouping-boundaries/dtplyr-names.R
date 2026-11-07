args <- commandArgs(trailingOnly = TRUE)
pkgload::load_all(args[[1L]], quiet = TRUE)
library(dplyr)
library(dtplyr)
spellings <- c(
  ".", ".N", ".I", ".SD", ".SDall", ".GRP", ".NGRP", ".BY", ".ROW", ".xSD", ".iSD",
  ".EACHI", ".NATURAL", ".rows", "...", "..1", "..01", "..0", "..-1", "..+1",
  ".data", ".env", "T", "F", "if", "by", "keyby", "on", "nomatch", "which",
  "getExportedValue", "as.data.table", "list", "vec_slice", "n", "i.g", "x.g",
  "g g", "g`g", "g\ng", "雪", "..marginplyr_nest_1", "..marginplyr_column_1"
)
normalize <- function(out, key, result_name, alias) {
  inner <- lapply(out[[key]], function(cell) {
    columns <- as.list(cell)
    names(columns)[names(columns) == result_name] <- alias
    list(columns = columns, n = nrow(cell))
  })
  cols <- as.list(ungroup(out))
  cols[[key]] <- inner
  groups <- group_vars(out)
  names(cols)[names(cols) == result_name] <- alias
  groups[groups == result_name] <- alias
  list(columns = cols, groups = groups)
}
outcome <- function(spelling, role, rowwise, keep, use_lazy = TRUE) {
  data <- tibble(g = c("b", "a", "a"), w = c(31L, 12L, 42L))
  .by <- NULL
  dim <- "g"
  key <- "cell"
  id <- "id"
  if (role == "dimension") {
    names(data)[1L] <- spelling
    dim <- spelling
  }
  if (role == "by") {
    names(data)[1L] <- spelling
    .by <- spelling
    dim <- NULL
  }
  if (role == "payload") names(data)[2L] <- spelling
  if (role == "key") key <- spelling
  if (role == "id") id <- spelling
  src <- if (use_lazy) dtplyr::lazy_dt(data) else data
  verb <- if (rowwise) nest_by_with_margins else nest_with_margins
  warnings <- character()
  tryCatch(withCallingHandlers({
    out <- verb(src, .by = all_of(.by), .grouping = rollup(all_of(dim)),
                .key = key, .id = id, .keep = keep, .sort = "last")
    if (inherits(out, "dtplyr_step")) {
      out <- dplyr::as_tibble(out, .name_repair = "minimal")
    }
    list(value = normalize(out, key, spelling, "RENAMED"), warnings = warnings)
  }, warning = function(cnd) {
    warnings <<- c(warnings, conditionMessage(cnd))
    invokeRestart("muffleWarning")
  }), error = function(cnd) {
    list(error = conditionMessage(cnd), error_class = class(cnd), warnings = warnings)
  })
}
cases <- expand.grid(name = spellings, role = c("dimension", "by", "payload", "key", "id"),
                     rowwise = c(FALSE, TRUE), keep = c(FALSE, TRUE),
                     stringsAsFactors = FALSE)
cases$result <- NA_character_
cases$detail <- NA_character_
for (i in seq_len(nrow(cases))) {
  case <- cases[i, ]
  got <- outcome(case$name, case$role, case$rowwise, case$keep)
  expected <- outcome("RENAMED", case$role, case$rowwise, case$keep)
  cases$result[[i]] <- if (identical(got, expected)) "match" else if (!is.null(got$error)) "error" else "difference"
  if (cases$result[[i]] != "match") {
    cases$detail[[i]] <- if (!is.null(got$error)) {
      gsub("\\n", " ", got$error)
    } else {
      paste(capture.output(str(got)), collapse = " ")
    }
  }
}
write.csv(cases, args[[2L]], row.names = FALSE)
print(table(cases$role, cases$result))
print(unique(cases[cases$result != "match", c("name", "role", "result", "detail")]), row.names = FALSE, right = FALSE)
