args <- commandArgs(trailingOnly = TRUE)
root <- if (length(args)) args[[1L]] else getwd()
devtools::load_all(root, quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
data <- data.frame(g = c("a", "b"), x = c(1, 3), old = c(100,300))
reads <- alist(
  old,
  .data$old,
  .data[[key]],
  get("old"),
  get0("old"),
  mget("old")[[1L]],
  eval(quote(old)),
  evalq(old),
  eval.parent(quote(old)),
  eval.parent(substitute(old)),
  get("old", inherits = FALSE),
  .Internal(get("old", environment(), "any", TRUE)),
  do.call(identity, list(old)),
  do.call("get", list("old")),
  base::with(environment(), old),
  (function() old)(),
  (function(old = old) old)(),
  {a <- 1; old},
  .Primitive("get")("old"),
  identity(get)("old"),
  .subset2(as.list(environment()), "old"),
  base::get("old", mode = "numeric"),
  .data[[paste0("o", "ld")]],
  .env$old
)
key <- "old"
old <- 7
for (i in seq_along(reads)) {
  expr <- reads[[i]]
  call <- rlang::expr(summarize_with_margins(data,
    s = sum(x), old = share_of_total(s), derived = sum(!!expr),
    .grouping = rollup(g), .margin_label = NULL))
  ans <- tryCatch(rlang::eval_bare(call), error = identity)
  cat("\nREAD", i, paste(deparse(expr), collapse = " "), "\n")
  if (inherits(ans, "error")) cat("ERROR", class(ans)[[1L]], conditionMessage(ans), "\n") else print(ans)
}
cat("\nSOURCE SIZES AND TYPES\n")
forms <- alist(1L, 1, NA_real_, NaN, -0, Inf, -Inf,
  setNames(1, "v"), structure(1, foo = "bar"), structure(1, class = "numeric"),
  as.raw(1), as.Date("2020-01-01"), list(1), integer(), c(1, 2), matrix(1),
  tibble(x = 1), expression(1), factor("1"))
for (kind in c("parent", "total")) for (i in seq_along(forms)) {
  expr <- forms[[i]]
  share <- rlang::call2(paste0("share_of_", kind), quote(s))
  for (backend in c("local", "dtplyr")) {
    input <- if (backend == "local") data else dtplyr::lazy_dt(data)
    ans <- tryCatch(rlang::inject(summarize_with_margins(input,
      s = !!expr, p = !!share, .grouping = rollup(g), .margin_label = NULL)) |> collect(), error = identity)
    cat(kind, backend, paste(deparse(expr), collapse = " "), ": ")
    if (inherits(ans, "error")) cat(class(ans)[[1L]], conditionMessage(ans), "\n") else cat(typeof(ans$p), paste(ans$p, collapse = ","), "\n")
  }
}
