# Investigated SHA: 56c7679f35b266cb4c598afffa4fd63ed84d3cba
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# print() shows an injected nested Grouping specification as <mrgn_gr_> (#1823).
suppressMessages(pkgload::load_all(".", quiet = TRUE))
# Contract (?grouping_set, @return): "Printing it shows the constructor call as written,
# nested constructors included". The documented extension idiom injects the previous
# specification with `!!`.
s <- grouping_set(region)
s <- grouping_sets(!!s, grouping_set())          # idiom quoted in ?grouping_set
cat("--- injected specification (documented idiom)\n"); print(s)
cat("--- same nested constructor written literally\n"); print(grouping_sets(grouping_set(region), grouping_set()))
cat("--- same nested specification passed by name\n"); inner <- grouping_set(region); print(grouping_sets(inner, grouping_set()))
cat("--- injected through !!! and rlang::inject\n")
print(grouping_sets(!!!list(rollup(a), cube(b, c)))); print(rlang::inject(grouping_sets(!!rollup(a), b)))
cat("--- native control: rlang's own display of a quosure-captured object injected into a call\n")
x <- rollup(a); print(rlang::expr_deparse(rlang::call2("grouping_sets", x)))
cat("--- injected quosure and symbol are shown as code (pinned by test-grouping-interface.R)\n")
print(rollup(!!rlang::quo(region), !!quote(store)))
out <- capture.output(print(s))[2]
cat("--- evaluating the printed call:\n")
res <- tryCatch(eval(parse(text = out)), error = function(e) paste("ERROR:", conditionMessage(e)))
print(if (is.character(res)) res else "rebuilt")
