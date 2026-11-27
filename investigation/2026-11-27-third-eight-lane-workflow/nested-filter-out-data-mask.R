# Investigated SHA: 56c7679f35b266cb4c598afffa4fd63ed84d3cba
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# filter_out() and nest_by() nested in a summary do not own their selection helpers (#1819).
suppressMessages(pkgload::load_all(".", quiet = TRUE))
suppressMessages(library(dplyr))
d <- tibble(g = c("a", "a", "b"), v = c(1, 2, 4))
nested <- tibble(aa = 1:3)          # `aa` is a column of the nested frame only
show <- function(label, expr) {
  out <- tryCatch({ x <- expr; paste(paste0(ifelse(is.na(x$g), "<total>", x$g), "=", x$n), collapse = " ") },
                  error = function(e) paste("ERROR:", substr(gsub("\\s+", " ", conditionMessage(e)), 1, 80)))
  cat(sprintf("%-52s %s\n", label, out))
}
w <- function(call) function() eval(call)
cat("Expected (ADR 0019, independent data-mask ownership): the nested dplyr verb owns pick(aa); each row is n = 1 (filter_out) or n = 2 (filter).\n\n")
for (verb in c("filter", "filter_out", "nest_by", "mutate", "distinct", "reframe")) {
  call <- bquote(summarize_with_margins(d, n = nrow(.(as.name(verb))(nested, pick(aa)[[1]] > 1)),
                                        .grouping = rollup(g), .margin_label = NULL))
  show(sprintf("marginplyr: nrow(%s(nested, pick(aa)[[1]] > 1))", verb), eval(call))
}
cat("\nPlain dplyr on the same grouped input, no marginplyr (dplyr expands pick() syntactically in any call argument):\n")
show("native summarise: nrow(filter(nested, pick(aa)...))",
     summarise(group_by(d, g), n = nrow(filter(nested, pick(aa)[[1]] > 1))))
show("native summarise: nrow(filter_out(nested, pick(aa)...))",
     summarise(group_by(d, g), n = nrow(filter_out(nested, pick(aa)[[1]] > 1))))
cat("\nCaller-written equivalent that marginplyr does shield (the registry lists filter), same nested pick():\n")
show("marginplyr: filter, then the complement by hand",
     summarize_with_margins(d, n = nrow(filter(nested, !(pick(aa)[[1]] > 1))), .grouping = rollup(g), .margin_label = NULL))

# Added in adjudication: the shapes where plain dplyr is right and the Margin summary is not.
show <- function(label, expr) cat(label, ":", tryCatch(paste(capture.output(print(as.data.frame(expr))), collapse = " | "), error = function(e) paste("ERROR", class(e)[1], gsub("\n", " ", conditionMessage(e)))), "\n")
d2 <- dplyr::tibble(g = c("a", "a", "b"), v = c(1, 2, 4))
nested_g <- dplyr::tibble(g = c("x", "y", "z"), aa = 1:3)
show("Margin summary, filter_out(nested_g, if_any(everything(), ~ .x == 'x'))", summarize_with_margins(d2, n = nrow(dplyr::filter_out(nested_g, dplyr::if_any(dplyr::everything(), ~ .x == "x"))), .grouping = rollup(g)))
show("Margin summary, the registered filter(nested_g, !if_any(...))         ", summarize_with_margins(d2, n = nrow(dplyr::filter(nested_g, !dplyr::if_any(dplyr::everything(), ~ .x == "x"))), .grouping = rollup(g)))
show("plain summarise(), filter_out(...)                                    ", dplyr::summarise(d2, n = nrow(dplyr::filter_out(nested_g, dplyr::if_any(dplyr::everything(), ~ .x == "x"))), .by = g))
cat("the nested call alone:", nrow(dplyr::filter_out(nested_g, dplyr::if_any(dplyr::everything(), ~ .x == "x"))), "\n")
show("Margin summary, ncol(nest_by(nested_g, across(everything())))         ", summarize_with_margins(d2, n = ncol(dplyr::nest_by(nested_g, dplyr::across(dplyr::everything()))), .grouping = rollup(g)))
show("plain summarise(), ncol(nest_by(...))                                 ", dplyr::summarise(d2, n = ncol(dplyr::nest_by(nested_g, dplyr::across(dplyr::everything()))), .by = g))
cat("the nested call alone:", ncol(dplyr::nest_by(nested_g, dplyr::across(dplyr::everything()))), "\n")
