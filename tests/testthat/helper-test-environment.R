# Forces every lazy binding of the environment testthat evaluates the suite in,
# so that environment holds the namespace's values as `pkgload::load_all()`
# gives them.
#
# `testthat::test_env()` is `rlang::env_clone()` of the namespace, and helpers
# are evaluated in that clone itself. Against an installed package each
# namespace binding is a lazy-load promise, and the clone holds its own copy,
# which forcing the namespace's does not force. marginplyr refuses an unforced
# promise in the caller's environment (`selection_binding_deferred()`), so a
# test that captures a constructor without calling it, as in
# `rollup(grouping_set(a, b))`, would otherwise pass or fail by whether an
# earlier file in the same worker had called it. A user's session never meets
# such a copy: the attached export is the namespace's own promise.
local({
  env <- parent.env(environment())
  names <- names(env)
  for (name in names[rlang::env_binding_are_lazy(env, names)]) {
    get(name, envir = env, inherits = FALSE)
  }
})
