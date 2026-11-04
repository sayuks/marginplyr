Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(quiet = TRUE)
input <- data.frame(g=c("a","b"),x=c(1,3), prior=c(100,300))
exprs <- alist(
  default_remove = (function(prior = {base::rm(prior); 1}) {prior; sum(prior)})(),
  default_force_remove = (function(prior = {base::rm(prior); 1}) {force(prior); sum(prior)})(),
  default_explicit_remove = (function(prior = {base::rm("prior", envir = environment()); 1}) {prior; sum(prior)})(),
  inline_remove_control = (function(prior = 1) {prior; base::rm(prior); sum(prior)})(),
  default_constant_control = (function(prior = 1) {prior; sum(prior)})()
)
for (name in names(exprs)) {
  out <- tryCatch(summarize_with_margins(input, value=sum(x), prior=share_of_total(value), derived=!!exprs[[name]], .grouping=rollup(g)),error=function(e)e)
  cat(name, "\n"); print(out)
}
