# Dated investigation probe; no package code is modified.
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(quiet = TRUE)
out <- "investigation/2026-11-02-nest-grouping-hunt/backend"
cat("SHA:", system2("git", c("rev-parse", "HEAD"), stdout = TRUE), "\n")
cat(R.version.string, "\n")
for (package in c("dplyr", "dtplyr", "data.table", "vctrs", "tidyr", "tibble")) {
  cat(package, as.character(utils::packageVersion(package)), "\n")
}
values <- list(
  logical = c(TRUE, NA, FALSE, TRUE), integer = c(1L, NA, 2L, 1L),
  double = c(1, NA_real_, NaN, 1), infinity = c(Inf, -Inf, NA_real_, Inf),
  complex = c(1+1i, NA_complex_, 2+0i, 1+1i), raw = as.raw(c(1, 0, 2, 1)),
  character = c("a", NA_character_, "b", "a"),
  unicode = c("e\u0301", "\u00e9", NA_character_, "e\u0301"),
  factor = factor(c("a", NA, "b", "a"), levels = c("b", "a", "unused")),
  ordered = ordered(c("a", NA, "b", "a"), levels = c("b", "a", "unused")),
  na_level = factor(c("a", NA, "b", "a"), exclude = NULL),
  date = as.Date(c("2025-01-01", NA, "2025-01-02", "2025-01-01")),
  datetime = as.POSIXct(c("2025-01-01", NA, "2025-01-02", "2025-01-01"), tz = "Japan"),
  time = as.difftime(c(1, NA, 2, 1), units = "hours"),
  list = list(1L, NULL, c(1, 2), 1L),
  list_nested = list(list(a=1), list(a=1), NULL, list(a=2)),
  list_df = list(data.frame(x=1), data.frame(x=1), data.frame(x=2), data.frame(x=1)),
  list_of = vctrs::list_of(1L, integer(), c(1L, 2L), 1L),
  AsIs = I(c(1, NA, 2, 1)),
  AsIs_list = I(list(1L, NULL, c(1, 2), 1L)),
  list_matrix = list(matrix(1:4, 2L), matrix(1:4, 2L), NULL, matrix(5:8, 2L)),
  rcrd = vctrs::new_rcrd(list(x = c(1, NA, 2, 1), y = c(1L, NA, 2L, 1L)), class = "audit_rcrd")
)
format.audit_rcrd <- function(x, ...) paste0(vctrs::field(x,"x"), "/", vctrs::field(x,"y"))
if (requireNamespace("bit64", quietly = TRUE)) values$integer64 <- bit64::as.integer64(c("9007199254740992", NA, "9007199254740993", "9007199254740992"))
if (requireNamespace("hms", quietly = TRUE)) values$hms <- hms::hms(c(1, NA, 2, 1))
canonical <- function(result) {
  result <- tibble::as_tibble(result)
  if (nrow(result)) result <- result[order(result$set, vapply(result$data, function(cell) paste(cell$row_id, collapse=","), character(1))), ]
  result$data <- lapply(result$data, tibble::as_tibble)
  result
}
records <- list(); record <- function(...) records[[length(records)+1L]] <<- data.frame(..., stringsAsFactors=FALSE)
for (type in names(values)) for (size in c(0L, 1L, 4L)) {
  value <- vctrs::vec_slice(values[[type]], seq_len(size))
  original <- tibble::tibble(g=rep(c("a","b","a","a"),length.out=size), p=value, row_id=seq_len(size))
  lazy <- tryCatch(dtplyr::lazy_dt(original), error=identity)
  material <- tryCatch(dplyr::collect(lazy), error=identity)
  if (inherits(material,"error")) {
    record(type=type,size=size,role="source",keep=NA,verb="none",status="source_error",message=conditionMessage(material)); next
  }
  for (role in c("payload", "fixed", "dimension")) for (keep in c(FALSE,TRUE)) for (verb in c("nest","nest_by")) {
    run <- function(data) {
      fn <- if (verb=="nest") nest_with_margins else nest_by_with_margins
      fn(data, .by=dplyr::all_of(if(role=="fixed") "p" else character()),
         .grouping=if(role=="dimension") rollup(p) else rollup(g),
         .margin_label=NULL, .keep=keep,.id="set",.sort="last")
    }
    expected <- tryCatch(canonical(run(material)),error=identity)
    warnings <- character()
    actual <- tryCatch(withCallingHandlers(canonical(dplyr::collect(run(lazy))),warning=function(w){warnings<<-c(warnings,conditionMessage(w));invokeRestart("muffleWarning")}),error=identity)
    if(inherits(expected,"error") || inherits(actual,"error")) {
      status <- if(inherits(actual,"marginplyr_error")) "refused" else if(inherits(actual,"error") && inherits(expected,"error")) "both_error" else if(inherits(actual,"error")) "backend_error" else "local_error"
      message <- if(inherits(actual,"error")) conditionMessage(actual) else conditionMessage(expected)
    } else {
      equal <- isTRUE(all.equal(actual,expected, check.attributes=TRUE))
      status <- if(equal) "match" else "mismatch"
      message <- if(equal) "" else paste(all.equal(actual,expected),collapse="; ")
    }
    if(length(warnings)) message <- paste(message,"WARN",paste(warnings,collapse=" | "))
    record(type=type,size=size,role=role,keep=keep,verb=verb,status=status,message=message)
  }
}
results <- do.call(rbind,records)
utils::write.csv(results,file.path(out,"type-differential.csv"),row.names=FALSE)
print(table(results$status)); print(results[!results$status %in% c("match","refused","both_error"),],row.names=FALSE)
