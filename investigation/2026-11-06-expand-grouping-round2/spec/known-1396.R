# Independent replay of the already-open #1396; no new ticket is implied.
Sys.setenv(TESTTHAT_PARALLEL = 'false')
pkgload::load_all(Sys.getenv('MARGINPLYR_HUNT_SOURCE', '.'), quiet = TRUE)
data <- tibble::tibble(a = c('north', 'south'), b = c(4L, 4L))
run <- function(operator) {
  calls <- 0L
  authored_namespace <- function(...) {
    calls <<- calls + 1L
    function(...) 'b'
  }
  env <- rlang::env(globalenv(), s = grouping_set(a))
  rlang::env_bind(env, !!operator := authored_namespace)
  head <- rlang::call2(operator, quote(base), quote(`(`))
  captured <- rlang::new_quosure(rlang::call2(head, quote(s)), env)
  direct <- names(tidyselect::eval_select(captured, data))
  direct_calls <- calls
  calls <- 0L
  specification <- grouping_sets(!!captured)
  actual <- inspect_grouping(data, .grouping = specification, .format = 'list')
  list(direct = direct, direct_calls = direct_calls,
       included = actual$included[[1L]], nested_calls = calls)
}
public <- run('::')
private <- run(':::')
stopifnot(identical(public$direct, 'b'), public$direct_calls == 1L,
          identical(private$direct, 'b'), private$direct_calls == 1L,
          identical(private$included, 'b'), private$nested_calls == 1L)
base_control <- inspect_grouping(data, .grouping = grouping_sets(base::`(`(grouping_set(a))),
                                 .format = 'list')
stopifnot(identical(base_control$included, list('a')))
print(list(public = public, private = private, base_control = base_control$included))
if ('--assert' %in% commandArgs(TRUE)) {
  stopifnot(identical(public$included, public$direct), public$nested_calls == 1L)
} else {
  stopifnot(identical(public$included, 'a'), public$nested_calls == 0L)
}
