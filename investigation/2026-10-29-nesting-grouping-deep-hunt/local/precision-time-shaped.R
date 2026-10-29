args <- commandArgs(TRUE)
pkgload::load_all(if (length(args)) args[[1L]] else ".", quiet = TRUE)
make_value <- function(kind, n) {
  ix <- rep(seq_len(6L), length.out = n)
  if (kind == "integer64") return(bit64::as.integer64(c("9223372036854775807", "-9223372036854775807", "9007199254740993", "9007199254740994", NA, "9007199254740993"))[ix])
  if (kind == "posixlt") return(as.POSIXlt(as.POSIXct(c(0, 1604208600, 1604212200, NA_real_, Inf, 1604208600)[ix], origin="1970-01-01", tz="America/New_York")))
  if (kind == "nonfinite") return(c(0, -0, NA_real_, NaN, Inf, -Inf)[ix])
  if (kind == "complex") return(c(1+1i, 1+1i, complex(real=NA_real_, imaginary=1), complex(real=1, imaginary=NA_real_), 0+0i, -0+0i)[ix])
  if (kind == "factor_matrix") return(structure(rep(c(1L,2L,NA_integer_,3L,1L,2L)[ix],2L), levels=c("a",NA_character_,"unused"), class="factor", dim=c(n,2L)))
  if (kind == "packed_factor") return(tibble::tibble(f=structure(c(1L,2L,NA_integer_,3L,1L,2L)[ix],levels=c("a",NA_character_,"unused"),class="factor"),z=ix))
  if (kind == "array1") return(array(c(NA_real_,NaN,1,2,1,NA_real_)[ix],dim=n))
  stop("unknown fixture")
}
values_match <- function(observed, expected, kind) {
  if (kind == "integer64") return(identical(as.character(observed),as.character(expected)))
  if (kind == "posixlt") return(identical(as.numeric(observed),as.numeric(expected)))
  isTRUE(all.equal(observed,expected,check.attributes=TRUE))
}
compared <- calls <- 0L
for (kind in c("integer64","posixlt","nonfinite","complex","factor_matrix","packed_factor","array1")) {
  roles <- if (kind == "factor_matrix") "payload" else c("payload","fixed","dimension")
  for (role in roles) for (n in c(0L,1L,6L)) for (keep in c(FALSE,TRUE)) for (verb in c("nest_with_margins","nest_by_with_margins")) for (sorting in c("none","first","last")) {
    x <- tibble::tibble(g=rep(c("b","a",NA_character_),length.out=n), typed=make_value(kind,n),row_id=seq_len(n))
    fixed <- if (role == "fixed") "typed" else character()
    dimension <- if (role == "dimension") "typed" else "g"
    spec <- rlang::eval_bare(rlang::call2("rollup",rlang::call2("all_of",dimension)))
    sets <- list(dimension,character())
    before <- serialize(x,NULL)
    result <- get(verb)(x,.by=dplyr::all_of(fixed),.grouping=spec,.margin_label=NULL,.keep=keep,.id="set",.key=".rows",.sort=sorting)
    calls <- calls+1L
    stopifnot(identical(serialize(x,NULL),before))
    for (j in seq_along(sets)) {
      keys <- c(fixed,sets[[j]])
      groups <- if (!n) list() else if(length(keys))vctrs::vec_group_loc(x[keys])$loc else list(seq_len(n))
      cells <- result[[".rows"]][result$set==j]
      stopifnot(length(groups)==length(cells))
      expected_members <- sort(vapply(groups,function(ix)paste(ix,collapse=","),character(1)))
      actual_members <- sort(vapply(cells,function(cell)paste(cell$row_id,collapse=","),character(1)))
      stopifnot(identical(expected_members,actual_members))
      for (cell in cells) {
        projection <- vctrs::vec_slice(x,cell$row_id)
        for (column in names(cell)) {
          if (column == "typed") stopifnot(values_match(cell[[column]],projection[[column]],kind)) else stopifnot(identical(cell[[column]],projection[[column]]))
          compared <- compared+1L
        }
      }
    }
  }
}
# Factor matrices were a valid payload. Grouping them already overflowed in
# dplyr and vctrs independently, so those key roles were excluded above.
f <- make_value("factor_matrix",3L)
x <- tibble::tibble(f=f,row_id=1:3)
controls <- list(
  grouping=tryCatch(dplyr::group_by(x,f),error=identity),
  location=tryCatch(vctrs::vec_group_loc(f),error=identity),
  nesting=tryCatch(nest_with_margins(x,.by=f),error=identity)
)
stopifnot(all(vapply(controls,function(x)inherits(x,"stackOverflowError"),logical(1))))
for(name in names(controls))cat("factor-matrix",name,":",class(controls[[name]])[[1L]],"/",conditionMessage(controls[[name]]),"\n")
# POSIXlt was converted by ordinary union too; the oracle above compared instants.
tm <- tibble::tibble(typed=make_value("posixlt",6L),row_id=1:6)
stopifnot(inherits(dplyr::union_all(tm,tm)$typed,"POSIXct"))
cat("PASS:",calls,"calls;",compared,"source cell-column projections; extreme integer64, temporal instants, nonfinite/complex keys, structured factors and one-dimensional arrays\n")
