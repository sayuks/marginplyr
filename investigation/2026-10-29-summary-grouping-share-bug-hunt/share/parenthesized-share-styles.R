args <- commandArgs(trailingOnly = TRUE)
devtools::load_all(if (length(args)) args[[1L]] else getwd(), quiet = TRUE)
data <- data.frame(g = c("a", "b"), x = c(1, 3))
for (fn in alist(~sum(.x), (~sum(.x)), ((~sum(.x))))) {
  for (helper in c("share_of_total", "share_of_parent")) {
    for (style in c("direct", "across")) {
      share <- if (style == "direct") {
        list(share = rlang::call2(helper, quote(s)))
      } else {
        list(rlang::call2("dplyr::across", quote(s), rlang::sym(helper), .names = "share"))
      }
      if (style == "across") share[[1L]][[1L]] <- quote(dplyr::across)
      answer <- tryCatch(rlang::inject(summarize_with_margins(
        data, dplyr::across(x, !!fn, .names = "s"), !!!share,
        .grouping = rollup(g)
      )), error = identity)
      cat(deparse(fn), helper, style, "\n")
      if (inherits(answer, "error")) {
        cat(class(answer)[[1L]], conditionMessage(answer), "\n")
      } else {
        stopifnot(identical(answer$share, c(0.25, 0.75, 1)))
        print(answer)
      }
    }
  }
}
