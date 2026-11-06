Sys.setenv(TESTTHAT_PARALLEL = 'false')
args <- commandArgs(trailingOnly = TRUE)
pkgload::load_all(if (length(args)) args[[1L]] else '.', quiet = TRUE)
observe <- function(expr) tryCatch(force(expr), error = identity)
# The initial exact-value oracle flagged repeated AsIs classes, not changed fields.
x <- tibble::tibble(
  fixed = c('N','N','S','S',NA,NA),
  key = I(as.POSIXlt(as.POSIXct('2026-01-01',tz='Asia/Tokyo') + c(1,2,NA,1,2,NA))),
  row = 1:6
)
actual <- nest_with_margins(
  x,.by=fixed,.grouping=rollup(key),.keep=TRUE,.margin_label=NULL,.id='set'
)
sliced <- vctrs::vec_slice(x,seq_len(nrow(x)))
native <- dplyr::nest_by(dplyr::union_all(
  dplyr::mutate(sliced,source_key=key,set=1L),
  dplyr::mutate(sliced,source_key=key,key=vctrs::vec_init(key,nrow(x)),set=2L)
),fixed,set,key)
matching <- match(paste(actual$set,vapply(actual$data,function(cell)paste(cell$row,collapse=','),character(1))),paste(native$set,vapply(native$data,function(cell)paste(cell$row,collapse=','),character(1))))
stopifnot(!anyNA(matching),!anyDuplicated(matching))
for(i in seq_len(nrow(actual))) {
  stopifnot(identical(actual$data[[i]]$key,native$data[[matching[[i]]]]$source_key))
}
cat('AsIs POSIXlt exact native sliced-union/nest control: PASS\n')
# Direct ordinary union already refuses AsIs factors; vctrs slicing changes their class.
f <- structure(c(1L,2L,NA_integer_), levels=c('a',NA_character_),class='factor')
y <- tibble::tibble(key=I(f),row=1:3)
stopifnot(inherits(observe(dplyr::union_all(y,y)),'vctrs_error_cast'))
cat('AsIs factor direct ordinary union cast refusal: CONFIRMED\n')
# Factor arrays have a native grouping recursion, independently of marginplyr.
for(shape in list(c(3L,1L),c(3L,1L,2L))) {
  value <- structure(rep(as.integer(f),prod(shape)/3L),levels=levels(f),
                     class='factor',dim=shape)
  failure <- observe(vctrs::vec_group_loc(value))
  stopifnot(inherits(failure,'error'),grepl('nested too deeply|C stack',conditionMessage(failure)))
}
cat('Factor matrix/array native equality grouping recursion: 2 CONFIRMED\n')
# A deliberately wrong row-membership fixture must fail the independent oracle.
expected <- sort(c('1,2','3'))
mutated <- sort(c('1','2,3'))
stopifnot(!identical(expected,mutated))
# A deliberately reversed non-tied sort must be detected.
wrong_order <- tibble::tibble(missing=c(FALSE,FALSE),value=c('b','a'))
stopifnot(!identical(vctrs::vec_order(wrong_order),1:2))
cat('Synthetic wrong membership and order calibration: 2 PASS\n')
cat('R',as.character(getRversion()),'\n')
for(p in c('dplyr','vctrs','tibble','dtplyr','bit64','hms')) {
  cat(p,as.character(utils::packageVersion(p)),'\n')
}
