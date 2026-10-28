args <- commandArgs(trailingOnly = TRUE)
devtools::load_all(if (length(args)) args[[1L]] else getwd(), quiet = TRUE)
script_path <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[[1L]])
output_dir <- dirname(normalizePath(script_path))
suppressPackageStartupMessages(library(dplyr))
set.seed(29231)
keys <- list(
  double = c(1, 2, NA, NaN, -0, 0),
  character = c("a", "a ", NA, "", "é", "e\u0301"),
  logical = c(TRUE, FALSE, NA, TRUE, FALSE, NA),
  complex = c(1+1i, 1+2i, NA_complex_, NaN+1i, 0+0i, 1+1i),
  raw = as.raw(c(0,1,2,0,1,2)),
  factor = factor(c("a", "b", NA, "a", "b", NA), levels = c("b", "a", "unused")),
  factor_na_level = structure(c(1L, 2L, NA_integer_, 1L, 2L, NA_integer_), levels = c("x", NA_character_), class = "factor"),
  list = list(1,2,NULL,NA_real_,c(1,2),1),
  AsIs = I(c(1,2,NA,1,2,NA)),
  date = as.Date(c(1,2,NA,1,2,NA), origin = "1970-01-01"),
  time = as.POSIXct(c(1,2,NA,1,2,NA), origin = "1970-01-01", tz = "Asia/Tokyo"),
  difftime = as.difftime(c(1,2,NA,1,2,NA), units = "days"),
  integer64 = bit64::as.integer64(c("9007199254740992", "9007199254740993", NA, "9007199254740992", "9007199254740993", NA)),
  numeric_named = setNames(c(1,2,NA,1,2,NA), letters[1:6]),
  matrix = matrix(c(1,1,2,2,3,3,3,2,1,3,2,1), ncol=2),
  packed = tibble(a=c(1,1,2,2,3,3),b=c(3,2,1,3,2,1))
)
keys$ordered <- ordered(keys$factor)
keys$units <- units::set_units(c(1,2,NA,1,2,NA), "m")
failures <- list()
passed <- 0L
refused <- 0L
for (keyname in names(keys)) for (role in c("fixed", "dimension")) {
  for (label in list(NULL, "Total")) for (sort in c("none", "last", "first")) {
    data <- tibble(g = rep(c("a", "b"), 3), h = rep(c("x", "x", "y"), 2), x = c(1,3,2,-1,0,NA))
    if (role == "fixed") data$f <- keys[[keyname]] else data$g <- keys[[keyname]]
    report <- function(input) summarize_with_margins(input,
      s = sum(x, na.rm=TRUE), p = share_of_parent(s), t = share_of_total(s),
      .by = all_of(if (role == "fixed") "f" else character()),
      .grouping = rollup(g,h), .id = "set", .margin_label = label, .sort = sort)
    a <- tryCatch(report(data), error = identity)
    b <- tryCatch(collect(report(dtplyr::lazy_dt(data))), error = identity)
    info <- paste(keyname, role, if (is.null(label)) "NULL" else label, sort)
    if (inherits(a, "error") || inherits(b, "error")) {
      refused <- refused+1L
      cat("REFUSED", info, "local:", if(inherits(a,"error")) conditionMessage(a) else "OK", "dtplyr:", if(inherits(b,"error")) conditionMessage(b) else "OK", "\n")
      next
    }
    matches <- vctrs::vec_match(a, b)
    eq <- nrow(a) == nrow(b) && !anyNA(matches) && !anyDuplicated(matches)
    if (!eq) {
      failures[[info]] <- list(a=a,b=b)
      cat("FAIL", info, paste(all.equal(a,b,check.attributes=FALSE),collapse="; "),"\n")
    } else passed <- passed+1L
  }
}
cat("PASS",passed,"REFUSED",refused,"FAIL",length(failures),"\n")
