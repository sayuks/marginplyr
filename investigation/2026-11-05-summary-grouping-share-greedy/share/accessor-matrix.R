pkgload::load_all('.', quiet = TRUE)
Sys.setenv(TESTTHAT_PARALLEL = 'false')
assert_contract <- '--assert' %in% commandArgs(trailingOnly = TRUE)
data <- data.frame(g = c('a', 'b'), x = c(1, 3), prior = c(100, 300))
external <- list2env(list(prior = 7), parent = emptyenv())
expressions <- list(
 bracket_exact = quote(sum(parent.env(environment())[['prior', exact = TRUE]])),
 bracket_exact_false = quote(sum(parent.env(environment())[['prior', exact = FALSE]])),
 get_element = quote(sum(getElement(parent.env(environment()), 'prior'))),
 subset2 = quote(sum(.subset2(parent.env(environment()), 'prior'))),
 env_get_base_frame = quote(sum(rlang::env_get(environment(), 'prior', inherit = TRUE))),
 env_get_stop_empty = quote(sum(rlang::env_get(rlang::current_env(), 'prior', inherit = TRUE, last = emptyenv()))),
 env_get_stop_base = quote(sum(rlang::env_get(rlang::current_env(), 'prior', inherit = TRUE, last = baseenv()))),
 env_get_stop_global = quote(sum(rlang::env_get(rlang::current_env(), 'prior', inherit = TRUE, last = globalenv()))),
 bracket_control = quote(sum(parent.env(environment())[['prior']])),
 dollar_control = quote(sum(parent.env(environment())$prior)),
 env_get_control = quote(sum(rlang::env_get(rlang::current_env(), 'prior', inherit = TRUE))),
 get_control = quote(sum(get('prior', envir = parent.env(environment())))),
 external_field = quote(sum(external[['prior', exact = TRUE]])),
 external_subset2 = quote(sum(.subset2(external, 'prior'))),
 external_element = quote(sum(getElement(external, 'prior'))),
 external_env_get = quote(sum(rlang::env_get(external, 'prior', inherit = TRUE, last = emptyenv())))
)
inputs <- list(data.frame = data, tibble = tibble::as_tibble(data), data.table = data.table::as.data.table(data))
rows <- list()
for (input_name in names(inputs)) for (helper in c('share_of_parent', 'share_of_total')) {
 input <- inputs[[input_name]]; original <- serialize(as.data.frame(input), NULL)
 for (nm in names(expressions)) {
  expr <- expressions[[nm]]
  independent <- startsWith(nm, 'external_'); repaired <- grepl('_control$', nm)
  for (direction in c('after_share', 'source_alias')) {
   result <- tryCatch(if (direction == 'after_share') summarize_with_margins(input, value = sum(x), prior = !!rlang::call2(helper, quote(value)), observed = !!expr, .grouping = rollup(g), .sort = 'last') else summarize_with_margins(input, prior = sum(x), observed = !!expr, ratio = !!rlang::call2(helper, quote(observed)), .grouping = rollup(g), .sort = 'last'), error = identity)
   if (repaired) {
    stopifnot(inherits(result, 'marginplyr_error'))
    status <- 'correct_refusal'
   } else if (independent) {
    stopifnot(!inherits(result, 'error'), identical(result$observed, rep(7, 3)))
    status <- 'independent_success'
   } else {
    stopifnot(!inherits(result, 'error'), identical(result$observed, if (direction == 'after_share') c(100, 300, 400) else c(1, 3, 4)))
    stopifnot(identical(if (direction == 'after_share') result$prior else result$ratio, c(0.25, 0.75, 1)))
    status <- 'missing_refusal'
   }
   if (assert_contract && !independent) stopifnot(inherits(result, 'marginplyr_error'))
   rows[[length(rows) + 1L]] <- data.frame(input_name, helper, nm, direction, status)
  }
 }
 stopifnot(identical(original, serialize(as.data.frame(input), NULL)))
}
out <- do.call(rbind, rows)
output_dir <- Sys.getenv('MP_SHARE_HUNT_OUTPUT', unset = '')
if (nzchar(output_dir)) write.csv(out, file = file.path(output_dir, 'accessor-results.csv'), row.names = FALSE)
print(table(out$status))
cat('cases:', nrow(out), '; source bytes unchanged\n')
