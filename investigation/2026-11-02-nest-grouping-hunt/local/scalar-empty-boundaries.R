pkgload::load_all(quiet = TRUE)
Sys.setenv(TESTTHAT_PARALLEL = "false")
wrap <- list(plain=identity, named=function(x) setNames(x,"alias"),
  matrix=function(x) matrix(x,1L,1L), array=function(x) array(x,c(1L,1L,1L)),
  AsIs=I)
calls <- 0L
for (n in c(0L, 3L)) for (id_wrapper in wrap) for (key_wrapper in wrap) {
  for (key in c("cell", ".rows", "..1", "...", "a\nb")) {
    for (verb_name in c("nest_with_margins", "nest_by_with_margins")) {
      input <- tibble::tibble(value=seq_len(n))
      result <- get(verb_name)(input,.id=id_wrapper("set"),.key=key_wrapper(key))
      expected_rows <- as.integer(n > 0L || verb_name == "nest_by_with_margins")
      stopifnot(identical(names(result),c("set",key)),nrow(result)==expected_rows,
        identical(result$set,rep(1L,expected_rows)),
        identical(dplyr::group_vars(result), if (verb_name == "nest_by_with_margins") "set" else character()))
      if (expected_rows) stopifnot(identical(as.list(result[[key]][[1L]]),as.list(input)))
      calls <- calls+1L
    }
  }
}
cat("Scalar output-name/empty-cardinality public calls:",calls,"all matched\n")
# Replay prior equality failures with desired-behavior assertions.
versions <- tibble::tibble(g=package_version(c("1.0","1.0.0")),row_id=1:2)
stopifnot(identical(vctrs::vec_group_loc(versions$g)$loc,list(1:2)))
for (verb in list(nest_with_margins,nest_by_with_margins)) {
  result <- verb(versions,.grouping=rollup(g),.id="set",.keep=TRUE)
  stopifnot(identical(result$data[[which(result$set==1L)]]$row_id,1:2),
    identical(result$data[[which(result$set==2L)]]$row_id,1:2))
}
cat("Historical package-version source equality: both verbs matched\n")
source("investigation/2026-10-31-nest-spec-greedy/local/factor-equality.R", local=TRUE)
# The old observation branch intentionally expects the old defect and stops;
# this command is not reached unless invoked with --assert.
cat("Historical NA-level factor source equality: both verbs matched\n")
